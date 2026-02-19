#' Embed function nodes with TF-IDF vectors
#'
#' Builds a TF-IDF vocabulary from the concatenated text of all nodes
#' (`signature`, `body_text`, `roxygen_text`) and computes a dense numeric
#' embedding vector for each node.  The resulting vocabulary and transformer
#' are stored in the returned model so that [embed_query()] can project
#' free-text queries into the same vector space.
#'
#' The embedding for each node is stored as an ordinary `numeric` vector
#' (not a sparse matrix) so it can be saved with [base::saveRDS()] without
#' size inflation.
#'
#' @param func_nodes A list of node records as returned by
#'   [extract_function_nodes()].  Each record must have `node_id`,
#'   `signature`, `body_text`, and `roxygen_text` fields.
#' @param method Character(1).  Currently only `"tfidf"` is supported; future
#'   sprints will add `"ollama"` and `"openai"`.
#' @param min_term_count Integer(1).  Vocabulary pruning threshold: terms that
#'   appear fewer than this many times across all documents are dropped.
#'   Default `1L` (keep all terms).
#' @param min_doc_count Integer(1).  Terms appearing in fewer than this many
#'   documents are dropped.  Default `1L`.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{`embeddings`}{A named list of `numeric` vectors, one per node
#'       (names are `node_id` strings).}
#'     \item{`model`}{An opaque embedding model object for use with
#'       [embed_query()].  Contains the vocabulary, vectorizer, and fitted
#'       TF-IDF transformer.}
#'   }
#'
#' @seealso [embed_query()], [cosine_similarity()]
#' @export
#' @examples
#' \dontrun{
#' proj   <- detect_rproject("/path/to/mypkg")
#' nodes  <- extract_function_nodes(proj$r_files)
#' result <- embed_nodes(nodes)
#' length(result$embeddings[[1]])  # vocabulary dimension
#' }
embed_nodes <- function(
  func_nodes,
  method = "tfidf",
  min_term_count = 1L,
  min_doc_count = 1L
) {
  method <- match.arg(method, "tfidf")

  if (length(func_nodes) == 0L) {
    return(list(embeddings = list(), model = NULL))
  }

  node_ids <- vapply(func_nodes, `[[`, character(1), "node_id")
  texts <- vapply(func_nodes, .node_text, character(1))

  model <- .fit_tfidf(texts, node_ids, min_term_count, min_doc_count)

  list(
    embeddings = model$embeddings,
    model = model
  )
}

#' Embed a free-text query into the TF-IDF vector space
#'
#' Projects `query` into the same vector space produced by [embed_nodes()],
#' using the vocabulary and TF-IDF transformer stored in `model`.  Out-of-
#' vocabulary terms are silently ignored (the resulting vector has zero weight
#' for those dimensions).
#'
#' @param query  Character(1).  The free-text query string.
#' @param model  The `model` element returned by [embed_nodes()].
#' @param method Character(1).  Must match the method used when building the
#'   model.  Only `"tfidf"` is supported in Sprint 1.
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
embed_query <- function(query, model, method = "tfidf") {
  method <- match.arg(method, "tfidf")

  if (is.null(model)) {
    return(numeric(0))
  }

  tokens <- text2vec::itoken(
    as.character(query),
    tokenizer = .tokenize,
    ids = "query",
    progressbar = FALSE
  )
  dtm <- text2vec::create_dtm(tokens, model$vectorizer)
  tfidf_m <- text2vec::transform(dtm, model$tfidf)

  # Convert sparse → dense vector; ensure same dimension as node embeddings
  vec <- as.numeric(tfidf_m[1L, , drop = TRUE])
  vec
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
