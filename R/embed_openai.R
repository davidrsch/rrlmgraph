# ---- OpenAI backend --------------------------------------------------

#' Build OpenAI embeddings for a set of nodes
#' @keywords internal
.embed_openai <- function(
  func_nodes,
  node_ids,
  texts,
  verbose = FALSE,
  cache_dir = NULL
) {
  # ---- API key validation -------------------------------------------
  key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  if (!nzchar(key)) {
    cli::cli_abort(
      c(
        "OPENAI_API_KEY environment variable is not set.",
        "i" = "Set it with {.code Sys.setenv(OPENAI_API_KEY = '<your-key>')}."
      )
    )
  }

  if (!requireNamespace("httr2", quietly = TRUE)) {
    cli::cli_abort("{.pkg httr2} is required for the OpenAI backend.")
  }

  # ---- cache set-up --------------------------------------------------
  cache_path <- .resolve_embed_cache(cache_dir, "embeddings_openai.rds")
  cache <- .load_embed_cache(cache_path)

  hashes <- vapply(texts, .text_hash, character(1))
  names(hashes) <- node_ids

  to_embed <- node_ids[!hashes %in% names(cache)]
  cached <- node_ids[hashes %in% names(cache)]

  # ---- cost estimate --------------------------------------------------
  if (verbose) {
    # text-embedding-3-small: ~$0.02 per 1M tokens; estimate ~100 tok/node
    est_tokens <- length(to_embed) * 100L
    est_cost <- round(est_tokens / 1e6 * 0.02, 6)
    cli::cli_inform(
      paste0(
        "OpenAI: {length(cached)} cached, {length(to_embed)} to embed. ",
        "Estimated cost: ${est_cost} (est. {est_tokens} tokens)."
      )
    )
  }

  # ---- embed in batches of 100 ---------------------------------------
  if (length(to_embed) > 0L) {
    batch_size <- 100L
    batches <- split(to_embed, ceiling(seq_along(to_embed) / batch_size))
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

  n <- length(node_ids)
  mat <- if (n > 0L && !is.null(emb_list[[1L]])) {
    do.call(rbind, emb_list)
  } else {
    NULL
  }

  list(
    embeddings = emb_list,
    model = list(method = "openai"),
    matrix = mat
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
    as.character(
      sum(utf8ToInt(substr(text, 1L, 500L))) *
        nchar(text)
    )
  }
}
