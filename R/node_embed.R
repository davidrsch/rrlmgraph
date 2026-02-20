#' Embed function nodes with TF-IDF, Ollama, or OpenAI vectors
#'
#' Builds embeddings from the concatenated text of all nodes
#' (`signature`, `body_text`, `roxygen_text`).  Three backends are
#' supported:
#'
#' \describe{
#'   \item{`tfidf`}{Sparse TF-IDF vectors (default, no API required).}
#'   \item{`ollama`}{768-dim dense vectors via the local Ollama daemon
#'     (`nomic-embed-text`).  Falls back to TF-IDF when Ollama is
#'     unavailable.}
#'   \item{`openai`}{1536-dim dense vectors via the OpenAI
#'     `text-embedding-3-small` model.  Requires the `OPENAI_API_KEY`
#'     environment variable.}
#' }
#'
#' @param func_nodes A list of node records as returned by
#'   [extract_function_nodes()].  Each record must have `node_id`,
#'   `signature`, `body_text`, and `roxygen_text` fields.
#' @param method Character(1).  One of `"tfidf"`, `"ollama"`, or
#'   `"openai"`.  Default `"tfidf"`.
#' @param min_term_count Integer(1).  TF-IDF vocabulary pruning: drop
#'   terms appearing fewer than this many times.  Default `1L`.
#' @param min_doc_count Integer(1).  TF-IDF vocabulary pruning: drop
#'   terms in fewer than this many documents.  Default `1L`.
#' @param existing_corpus Optional character vector of additional
#'   document texts to include when fitting the TF-IDF vocabulary.
#'   This keeps incremental updates in the same vector space.
#'   Ignored for `"ollama"` and `"openai"` backends.
#' @param verbose Logical(1).  Emit progress messages.  For the
#'   `"openai"` backend also prints an estimated cost.  Default
#'   `FALSE`.
#' @param cache_dir Character(1) or `NULL`.  Directory used to store
#'   embedding caches for the `ollama` and `openai` backends.  `NULL`
#'   (default) uses the `.rrlmgraph/` folder under the working
#'   directory.
#'
#' @return A named list:
#'   \describe{
#'     \item{`embeddings`}{Named list of `numeric` vectors, one per node.}
#'     \item{`model`}{Embedding model for use with [embed_query()].}
#'     \item{`matrix`}{Optional numeric matrix (rows = nodes) for
#'       backends that return dense vectors.  `NULL` for TF-IDF.}
#'   }
#'
#' @seealso [embed_query()], [cosine_similarity()], [ollama_available()]
#' @export
#' @examples
#' \dontrun{
#' proj   <- detect_rproject("/path/to/mypkg")
#' nodes  <- extract_function_nodes(proj$r_files)
#' result <- embed_nodes(nodes)                          # TF-IDF
#' result <- embed_nodes(nodes, method = "ollama")      # Ollama
#' result <- embed_nodes(nodes, method = "openai")      # OpenAI
#' }
embed_nodes <- function(
  func_nodes,
  method           = c("tfidf", "ollama", "openai"),
  min_term_count   = 1L,
  min_doc_count    = 1L,
  existing_corpus  = NULL,
  verbose          = FALSE,
  cache_dir        = NULL
) {
  method <- match.arg(method)

  if (length(func_nodes) == 0L) {
    return(list(embeddings = list(), model = NULL, matrix = NULL))
  }

  node_ids <- vapply(func_nodes, `[[`, character(1), "node_id")
  texts    <- vapply(func_nodes, .node_text, character(1))

  switch(
    method,
    tfidf  = {
      all_texts <- if (length(existing_corpus) > 0L) {
        c(existing_corpus, texts)
      } else {
        texts
      }
      # Fit on full corpus but embed only the new nodes
      model <- .fit_tfidf(all_texts,
                           c(rep(NA_character_, length(existing_corpus)),
                             node_ids),
                           min_term_count, min_doc_count)
      # Keep only the new nodes' embeddings
      model$embeddings <- model$embeddings[node_ids]
      list(embeddings = model$embeddings, model = model, matrix = NULL)
    },
    ollama = .embed_ollama(func_nodes, node_ids, texts, verbose, cache_dir),
    openai = .embed_openai(func_nodes, node_ids, texts, verbose, cache_dir)
  )
}

#' Embed a free-text query into the model's vector space
#'
#' Projects `query` into the same vector space produced by [embed_nodes()],
#' using the vocabulary and TF-IDF transformer stored in `model`.  For the
#' `"ollama"` and `"openai"` backends the same API call is made for the
#' query as for the document nodes.
#'
#' @param query  Character(1).  The free-text query string.
#' @param model  The `model` element returned by [embed_nodes()].
#' @param method Character(1).  Must match the method used when building the
#'   model.  One of `"tfidf"`, `"ollama"`, or `"openai"`.
#'
#' @return A `numeric` vector in the same vector space as the stored node
#'   embeddings.  Returns a zero-length numeric if `model` is `NULL`.
#'
#' @seealso [embed_nodes()], [cosine_similarity()]
#' @export
#' @examples
#' \dontrun{
#' result <- embed_nodes(nodes)
#' qvec   <- embed_query("how are models evaluated?", result$model)
#' }
embed_query <- function(query, model, method = c("tfidf", "ollama", "openai")) {
  method <- match.arg(method)

  if (is.null(model)) {
    return(numeric(0))
  }

  switch(
    method,
    tfidf = {
      tokens <- text2vec::itoken(
        as.character(query),
        tokenizer = .tokenize,
        ids = "query",
        progressbar = FALSE
      )
      dtm <- text2vec::create_dtm(tokens, model$vectorizer)
      tfidf_m <- model$tfidf$transform(dtm)
      as.numeric(tfidf_m[1L, , drop = TRUE])
    },
    ollama = {
      if (!ollama_available()) {
        cli::cli_warn("Ollama not available; returning zero vector.")
        return(numeric(768L))
      }
      res <- tryCatch(
        ollamar::embed("nomic-embed-text", as.character(query)),
        error = function(e) NULL
      )
      if (is.null(res)) numeric(768L) else as.numeric(res$embedding[[1L]])
    },
    openai = {
      key <- Sys.getenv("OPENAI_API_KEY", unset = "")
      if (!nzchar(key)) {
        cli::cli_abort("OPENAI_API_KEY is not set.")
      }
      vec <- .openai_embed_texts(as.character(query), key, verbose = FALSE)
      if (length(vec) == 0L) numeric(1536L) else as.numeric(vec[[1L]])
    }
  )
}

#' Compute cosine similarity between two numeric vectors
#'
#' @param a Numeric vector.
#' @param b Numeric vector of the same length as `a`.
#'
#' @return Numeric(1) in `[-1, 1]`.  Returns `0` if either vector has zero
#'   norm (to avoid `NaN`).
#'
#' @seealso [embed_nodes()], [embed_query()]
#' @export
#' @examples
#' cosine_similarity(c(1, 0, 0), c(1, 0, 0))  # 1
#' cosine_similarity(c(1, 0, 0), c(0, 1, 0))  # 0
cosine_similarity <- function(a, b) {
  if (length(a) != length(b)) {
    cli::cli_abort(
      "`a` and `b` must have the same length ({length(a)} vs {length(b)})."
    )
  }
  norm_a <- sqrt(sum(a^2))
  norm_b <- sqrt(sum(b^2))
  if (norm_a == 0 || norm_b == 0) {
    return(0)
  }
  as.numeric(sum(a * b) / (norm_a * norm_b))
}

# ---- internal helpers -----------------------------------------------

#' @keywords internal
.node_text <- function(node) {
  parts <- c(
    if (!is.null(node$signature)) node$signature else "",
    if (!is.null(node$body_text)) node$body_text else "",
    if (!is.null(node$roxygen_text)) node$roxygen_text else ""
  )
  paste(parts[nchar(trimws(parts)) > 0L], collapse = " ")
}

#' @keywords internal
.tokenize <- function(text) {
  # Lower-case, split on non-alphanumeric characters (including _ and .)
  # Keep tokens >=2 chars to reduce noise
  tokens <- strsplit(tolower(text), "[^a-z0-9]+", perl = TRUE)
  lapply(tokens, function(toks) toks[nchar(toks) >= 2L])
}

#' @keywords internal
.fit_tfidf <- function(texts, ids, min_term_count, min_doc_count) {
  it <- text2vec::itoken(
    texts,
    tokenizer = .tokenize,
    ids = ids,
    progressbar = FALSE
  )

  vocab <- text2vec::create_vocabulary(it)
  vocab <- text2vec::prune_vocabulary(
    vocab,
    term_count_min = as.integer(min_term_count),
    doc_count_min = as.integer(min_doc_count)
  )

  vectorizer <- text2vec::vocab_vectorizer(vocab)

  # Re-iterate: itoken is consumed after first use
  it2 <- text2vec::itoken(
    texts,
    tokenizer = .tokenize,
    ids = ids,
    progressbar = FALSE
  )
  dtm <- text2vec::create_dtm(it2, vectorizer)

  tfidf <- text2vec::TfIdf$new()
  dtm_tfidf <- text2vec::fit_transform(dtm, tfidf)

  # Convert each row to a dense numeric vector and store by node_id
  embeddings <- stats::setNames(
    lapply(seq_len(nrow(dtm_tfidf)), function(i) {
      as.numeric(dtm_tfidf[i, , drop = TRUE])
    }),
    ids
  )

  list(
    embeddings = embeddings,
    vectorizer = vectorizer,
    tfidf = tfidf,
    vocab = vocab,
    n_dims = ncol(dtm_tfidf)
  )
}

# ---- Ollama backend --------------------------------------------------

#' Check whether Ollama is available
#'
#' Returns `TRUE` if the \pkg{ollamar} package is installed **and** the
#' local Ollama daemon is reachable (i.e. responds within a short
#' timeout).  This is used internally to decide whether to fall back to
#' TF-IDF when the caller requests `method = "ollama"`.
#'
#' @return Logical(1).
#' @seealso [embed_nodes()], [embed_query()]
#' @export
#' @examples
#' \dontrun{
#' if (ollama_available()) {
#'   result <- embed_nodes(nodes, method = "ollama")
#' }
#' }
ollama_available <- function() {
  if (!requireNamespace("ollamar", quietly = TRUE)) {
    return(FALSE)
  }
  tryCatch({
    res <- ollamar::list_models()
    !is.null(res)
  }, error = function(e) FALSE)
}

#' Build Ollama embeddings for a set of nodes
#' @keywords internal
.embed_ollama <- function(func_nodes, node_ids, texts,
                           verbose = FALSE, cache_dir = NULL) {
  # ---- check availability -------------------------------------------
  if (!ollama_available()) {
    cli::cli_warn(
      "Ollama is not available; falling back to TF-IDF embeddings."
    )
    model <- .fit_tfidf(texts, node_ids, 1L, 1L)
    return(list(embeddings = model$embeddings,
                model       = model,
                matrix      = NULL))
  }

  # ---- cache set-up --------------------------------------------------
  cache_path <- .resolve_embed_cache(cache_dir, "embeddings_ollama.rds")
  cache      <- .load_embed_cache(cache_path)

  # Use a SHA-1 of the node text as the cache key
  hashes <- vapply(texts, .text_hash, character(1))
  names(hashes) <- node_ids

  to_embed <- node_ids[!hashes %in% names(cache)]
  cached   <- node_ids[ hashes %in% names(cache)]

  if (verbose) {
    cli::cli_inform(
      "Ollama: {length(cached)} cached, {length(to_embed)} to embed."
    )
  }

  # ---- embed in batches of 10 ----------------------------------------
  batch_size <- 10L
  new_embeddings <- list()

  if (length(to_embed) > 0L) {
    batches <- split(to_embed, ceiling(seq_along(to_embed) / batch_size))
    for (batch_ids in batches) {
      batch_texts <- texts[match(batch_ids, node_ids)]
      for (i in seq_along(batch_ids)) {
        vec <- tryCatch({
          res <- ollamar::embed("nomic-embed-text", batch_texts[[i]])
          as.numeric(res$embedding[[1L]])
        }, error = function(e) {
          cli::cli_warn("Ollama embed failed for {batch_ids[[i]]}: {e$message}")
          rep(0, 768L)
        })
        key <- hashes[[batch_ids[[i]]]]
        new_embeddings[[key]] <- vec
      }
    }
    # Persist augmented cache
    updated_cache <- c(cache, new_embeddings)
    .save_embed_cache(cache_path, updated_cache)
    cache <- updated_cache
  }

  # ---- assemble output -----------------------------------------------
  emb_list <- stats::setNames(
    lapply(hashes, function(h) cache[[h]]),
    node_ids
  )

  n   <- length(node_ids)
  mat <- if (n > 0L && !is.null(emb_list[[1L]])) {
    do.call(rbind, emb_list)
  } else {
    NULL
  }

  list(
    embeddings = emb_list,
    model      = list(method = "ollama"),
    matrix     = mat
  )
}

# ---- OpenAI backend --------------------------------------------------

#' Build OpenAI embeddings for a set of nodes
#' @keywords internal
.embed_openai <- function(func_nodes, node_ids, texts,
                           verbose = FALSE, cache_dir = NULL) {
  # ---- API key validation -------------------------------------------
  key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  if (!nzchar(key)) {
    cli::cli_abort(
      c("OPENAI_API_KEY environment variable is not set.",
        "i" = "Set it with {.code Sys.setenv(OPENAI_API_KEY = '<your-key>')}.")
    )
  }

  if (!requireNamespace("httr2", quietly = TRUE)) {
    cli::cli_abort("{.pkg httr2} is required for the OpenAI backend.")
  }

  # ---- cache set-up --------------------------------------------------
  cache_path <- .resolve_embed_cache(cache_dir, "embeddings_openai.rds")
  cache      <- .load_embed_cache(cache_path)

  hashes <- vapply(texts, .text_hash, character(1))
  names(hashes) <- node_ids

  to_embed <- node_ids[!hashes %in% names(cache)]
  cached   <- node_ids[ hashes %in% names(cache)]

  # ---- cost estimate --------------------------------------------------
  if (verbose) {
    # text-embedding-3-small: ~$0.02 per 1M tokens; estimate ~100 tok/node
    est_tokens <- length(to_embed) * 100L
    est_cost   <- round(est_tokens / 1e6 * 0.02, 6)
    cli::cli_inform(
      paste0("OpenAI: {length(cached)} cached, {length(to_embed)} to embed. ",
             "Estimated cost: ${est_cost} (est. {est_tokens} tokens).")
    )
  }

  # ---- embed in batches of 100 ---------------------------------------
  if (length(to_embed) > 0L) {
    batch_size <- 100L
    batches    <- split(to_embed, ceiling(seq_along(to_embed) / batch_size))
    new_embeddings <- list()

    for (batch_ids in batches) {
      batch_texts <- texts[match(batch_ids, node_ids)]
      vecs <- .openai_embed_texts(batch_texts, key, verbose = FALSE)

      for (i in seq_along(batch_ids)) {
        key_hash <- hashes[[batch_ids[[i]]]]
        new_embeddings[[key_hash]] <- vecs[[i]]
      }
    }

    updated_cache <- c(cache, new_embeddings)
    .save_embed_cache(cache_path, updated_cache)
    cache <- updated_cache
  }

  # ---- assemble output -----------------------------------------------
  emb_list <- stats::setNames(
    lapply(hashes, function(h) cache[[h]]),
    node_ids
  )

  n   <- length(node_ids)
  mat <- if (n > 0L && !is.null(emb_list[[1L]])) {
    do.call(rbind, emb_list)
  } else {
    NULL
  }

  list(
    embeddings = emb_list,
    model      = list(method = "openai"),
    matrix     = mat
  )
}

#' Call the OpenAI embeddings API with exponential backoff on 429
#' @keywords internal
.openai_embed_texts <- function(texts, api_key, verbose = FALSE) {
  req <- httr2::request("https://api.openai.com/v1/embeddings") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = "text-embedding-3-small",
      input = as.list(texts)
    )) |>
    httr2::req_retry(
      max_tries = 5L,
      is_transient = function(resp) httr2::resp_status(resp) == 429L,
      backoff = ~ stats::runif(1, min = 1, max = 2^.x)
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)

  if (httr2::resp_status(resp) != 200L) {
    msg <- tryCatch(
      httr2::resp_body_json(resp)$error$message,
      error = function(e) paste("HTTP", httr2::resp_status(resp))
    )
    cli::cli_abort("OpenAI API error: {msg}")
  }

  body <- httr2::resp_body_json(resp)
  lapply(body$data, function(item) as.numeric(item$embedding))
}

# ---- embedding cache helpers -----------------------------------------

#' @keywords internal
.resolve_embed_cache <- function(cache_dir, filename) {
  if (is.null(cache_dir)) {
    cache_dir <- file.path(getwd(), ".rrlmgraph")
  }
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.path(cache_dir, filename)
}

#' @keywords internal
.load_embed_cache <- function(path) {
  if (file.exists(path)) {
    tryCatch(readRDS(path), error = function(e) list())
  } else {
    list()
  }
}

#' @keywords internal
.save_embed_cache <- function(path, cache) {
  tryCatch(
    saveRDS(cache, file = path),
    error = function(e) {
      cli::cli_warn("Could not save embedding cache: {e$message}")
    }
  )
}

#' Compute a short hash of a text string for use as a cache key
#' @keywords internal
.text_hash <- function(text) {
  # Use digest if available; fall back to a simple CRC-style hash
  if (requireNamespace("digest", quietly = TRUE)) {
    digest::digest(text, algo = "sha1", serialize = FALSE)
  } else {
    as.character(sum(utf8ToInt(substr(text, 1L, 500L))) *
                 nchar(text))
  }
}
