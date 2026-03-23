# ---- Ollama backend --------------------------------------------------

#' Check whether Ollama is available
#'
#' Returns `TRUE` if the local Ollama daemon is reachable (i.e. responds
#' to a `GET /api/tags` request within a short timeout).  This is used
#' internally to decide whether to fall back to TF-IDF when the caller
#' requests `method = "ollama"`.
#'
#' @section Configuration:
#' Set the `OLLAMA_BASE_URL` environment variable to override the default
#' endpoint (`http://localhost:11434`).
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
  base_url <- Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434")
  tryCatch(
    {
      req <- httr2::request(paste0(base_url, "/api/tags")) |>
        httr2::req_timeout(5L) |>
        httr2::req_error(is_error = function(resp) FALSE)
      resp <- httr2::req_perform(req)
      httr2::resp_status(resp) == 200L
    },
    error = function(e) FALSE
  )
}

#' Call the Ollama `/api/embed` endpoint for a single text
#' @keywords internal
.ollama_embed_text <- function(model_name, text, base_url) {
  req <- httr2::request(paste0(base_url, "/api/embed")) |>
    httr2::req_body_json(list(model = model_name, input = text)) |>
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) != 200L) {
    return(NULL)
  }
  body <- httr2::resp_body_json(resp)
  body$embeddings[[1L]]
}

#' Build Ollama embeddings for a set of nodes
#' @keywords internal
.embed_ollama <- function(
  func_nodes,
  node_ids,
  texts,
  verbose = FALSE,
  cache_dir = NULL
) {
  # ---- check availability -------------------------------------------
  if (!ollama_available()) {
    cli::cli_warn(
      "Ollama is not available; falling back to TF-IDF embeddings."
    )
    model <- .fit_tfidf(texts, node_ids, 1L, 1L)
    return(list(embeddings = model$embeddings, model = model, matrix = NULL))
  }

  base_url <- Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434")

  # ---- cache set-up --------------------------------------------------
  cache_path <- .resolve_embed_cache(cache_dir, "embeddings_ollama.rds")
  cache <- .load_embed_cache(cache_path)

  # Use a SHA-1 of the node text as the cache key
  hashes <- vapply(texts, .text_hash, character(1))
  names(hashes) <- node_ids

  to_embed <- node_ids[!hashes %in% names(cache)]
  cached <- node_ids[hashes %in% names(cache)]

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
        vec <- tryCatch(
          {
            as.numeric(
              .ollama_embed_text("nomic-embed-text", batch_texts[[i]], base_url)
            )
          },
          error = function(e) {
            cli::cli_warn(
              "Ollama embed failed for {batch_ids[[i]]}: {e$message}"
            )
            rep(0, 768L)
          }
        )
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

  n <- length(node_ids)
  mat <- if (n > 0L && !is.null(emb_list[[1L]])) {
    do.call(rbind, emb_list)
  } else {
    NULL
  }

  list(
    embeddings = emb_list,
    model = list(method = "ollama"),
    matrix = mat
  )
}
