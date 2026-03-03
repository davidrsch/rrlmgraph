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
  method = c("tfidf", "ollama", "openai"),
  min_term_count = 1L,
  min_doc_count = 1L,
  existing_corpus = NULL,
  verbose = FALSE,
  cache_dir = NULL
) {
  method <- match.arg(method)

  if (length(func_nodes) == 0L) {
    return(list(embeddings = list(), model = NULL, matrix = NULL))
  }

  node_ids <- vapply(func_nodes, `[[`, character(1), "node_id")
  texts <- vapply(func_nodes, .node_text, character(1))

  switch(
    method,
    tfidf = {
      all_texts <- if (length(existing_corpus) > 0L) {
        c(existing_corpus, texts)
      } else {
        texts
      }
      # Fit on full corpus but embed only the new nodes
      model <- .fit_tfidf(
        all_texts,
        c(rep(NA_character_, length(existing_corpus)), node_ids),
        min_term_count,
        min_doc_count
      )
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
      tfidf_m <- tryCatch(
        model$tfidf$transform(dtm),
        error = function(e) {
          # Fallback: apply pre-extracted IDF weights directly.
          # model$idf_weights is a plain numeric vector saved by .fit_tfidf()
          # so it survives igraph round-trips even when the R6 TfIdf object
          # fails to deserialise cleanly (e.g. missing private fields).
          idf <- model$idf_weights
          if (is.numeric(idf) && length(idf) == ncol(dtm)) {
            dtm %*% Matrix::Diagonal(x = idf)
          } else {
            dtm # last-resort: raw TF, better than nothing
          }
        }
      )
      as.numeric(tfidf_m[1L, , drop = TRUE])
    },
    ollama = {
      if (!ollama_available()) {
        cli::cli_warn("Ollama not available; returning zero vector.")
        return(numeric(768L))
      }
      base_url <- Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434")
      res <- tryCatch(
        .ollama_embed_text("nomic-embed-text", as.character(query), base_url),
        error = function(e) NULL
      )
      if (is.null(res)) numeric(768L) else as.numeric(res)
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

  # Extract IDF weights NOW, while tfidf is in scope and its private fields
  # are accessible.  text2vec 0.6+ stores IDF as a sparse diagonal Matrix in
  # private$idf (not as a public idf_vector active binding as earlier docs
  # suggested).  We extract the diagonal here so the value survives
  # serialisation as a plain numeric vector inside the model list.
  idf_weights <- tryCatch(
    {
      d <- tfidf$.__enclos_env__$private$idf
      if (
        inherits(d, "ddiMatrix") ||
          inherits(d, "Diagonal") ||
          methods::isVirtualClass("diagonalMatrix") &&
            methods::is(d, "diagonalMatrix")
      ) {
        as.numeric(Matrix::diag(d))
      } else if (is.numeric(d)) {
        as.numeric(d)
      } else {
        NULL
      }
    },
    error = function(e) NULL
  )

  # Fallback: compute smoothed IDF from document frequencies in vocab.
  # IDF(t) = log(1 + N / df(t)), where N = number of documents in the corpus.
  if (is.null(idf_weights) || length(idf_weights) == 0L) {
    n_docs <- nrow(dtm)
    idf_weights <- log(1 + n_docs / pmax(as.integer(vocab$doc_count), 1L))
  }

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
    idf_weights = idf_weights,
    n_dims = ncol(dtm_tfidf)
  )
}

