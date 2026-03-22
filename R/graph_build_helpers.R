# ---- internal assembly helpers ---------------------------------------

#' @keywords internal
# audit/expert-review fix: body_text and roxygen_text were silently
# dropped here, causing NULL context in SQLite export and context_assemble.R.
.make_function_vertex_df <- function(func_nodes) {
  if (length(func_nodes) == 0L) {
    return(data.frame(
      name = character(0),
      node_type = character(0),
      file = character(0),
      line_start = integer(0),
      line_end = integer(0),
      signature = character(0),
      body_text = character(0),
      roxygen_text = character(0),
      complexity = integer(0),
      pagerank = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    name = vapply(func_nodes, `[[`, character(1), "node_id"),
    node_type = "function",
    file = vapply(
      func_nodes,
      function(n) n$file %||% NA_character_,
      character(1)
    ),
    line_start = vapply(
      func_nodes,
      function(n) as.integer(n$line_start %||% NA_integer_),
      integer(1)
    ),
    line_end = vapply(
      func_nodes,
      function(n) as.integer(n$line_end %||% NA_integer_),
      integer(1)
    ),
    signature = vapply(
      func_nodes,
      function(n) n$signature %||% "",
      character(1)
    ),
    body_text = vapply(
      func_nodes,
      function(n) n$body_text %||% NA_character_,
      character(1)
    ),
    roxygen_text = vapply(
      func_nodes,
      function(n) n$roxygen_text %||% NA_character_,
      character(1)
    ),
    complexity = vapply(
      func_nodes,
      function(n) as.integer(n$complexity %||% 1L),
      integer(1)
    ),
    pagerank = 0,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) > 0L && !is.na(x[[1L]])) x else y
}

#' @keywords internal
.assemble_edges <- function(call_edges, import_edges, test_edges, vertex_df) {
  known_nodes <- vertex_df$name

  rows <- list()

  if (nrow(call_edges) > 0L) {
    ce <- call_edges
    ce$edge_type <- "CALLS"
    # Keep only edges where both endpoints are in vertex_df
    ce <- ce[ce$from %in% known_nodes & ce$to %in% known_nodes, , drop = FALSE]
    if (nrow(ce) > 0L) {
      rows <- c(rows, list(ce[, c("from", "to", "weight", "edge_type")]))
    }
  }

  if (nrow(import_edges) > 0L) {
    ie <- import_edges[, c("from", "to", "weight"), drop = FALSE]
    ie$edge_type <- "IMPORTS"
    ie <- ie[ie$from %in% known_nodes & ie$to %in% known_nodes, , drop = FALSE]
    if (nrow(ie) > 0L) rows <- c(rows, list(ie))
  }

  if (nrow(test_edges) > 0L) {
    te <- test_edges
    te$edge_type <- "TESTS"
    te <- te[te$from %in% known_nodes & te$to %in% known_nodes, , drop = FALSE]
    if (nrow(te) > 0L) {
      rows <- c(rows, list(te[, c("from", "to", "weight", "edge_type")]))
    }
  }

  if (length(rows) == 0L) {
    return(data.frame(
      from = character(0),
      to = character(0),
      weight = numeric(0),
      edge_type = character(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

#' @keywords internal
.build_semantic_edges <- function(embeddings, threshold, max_per_node = 5L) {
  ids <- names(embeddings)
  n <- length(ids)

  empty_df <- data.frame(
    from = character(0),
    to = character(0),
    similarity = numeric(0),
    stringsAsFactors = FALSE
  )

  if (n < 2L) {
    return(empty_df)
  }

  # For very large graphs (> 300 nodes) the O(n²) pairwise loop is infeasible.
  # Disable semantic edges in that case; users can opt in by reducing
  # max_semantic_edges or by post-processing with an ANN index.
  if (n > 300L) {
    cli::cli_warn(c(
      "!
" = "Semantic edge computation skipped: {n} nodes > 300 limit.",
      "i" = "Set {.arg semantic_threshold = 1.1} to suppress this warning."
    ))
    return(empty_df)
  }

  # For dense embeddings (same-length numeric vectors) use matrix multiply to
  # compute all pairwise cosines in one operation, which is much faster than
  # iterating over pairs.
  first_emb <- embeddings[[1L]]
  is_dense <- is.numeric(first_emb) &&
    length(first_emb) > 1L &&
    all(vapply(
      embeddings,
      function(e) {
        is.numeric(e) && length(e) == length(first_emb)
      },
      logical(1L)
    ))

  if (is_dense) {
    # Build normalised matrix (rows = nodes, cols = dims)
    mat <- do.call(rbind, embeddings)
    norms <- sqrt(rowSums(mat^2))
    # Avoid division by zero for zero-norm rows (empty documents)
    norms[norms == 0] <- 1
    mat <- mat / norms # L2-normalise
    sims <- mat %*% t(mat) # n x n cosine similarities
    diag(sims) <- 0 # exclude self-similarity

    # Collect index pairs above threshold, capped per node
    per_node_count <- integer(n)
    rows <- list()
    k <- 0L
    # Iterate columns in descending similarity order for consistent top-k
    for (i in seq_len(n)) {
      if (per_node_count[[i]] >= max_per_node) {
        next
      }
      order_j <- order(sims[i, ], decreasing = TRUE)
      for (j in order_j) {
        if (j <= i) {
          next
        } # emit each pair only once
        if (per_node_count[[i]] >= max_per_node) {
          break
        }
        if (per_node_count[[j]] >= max_per_node) {
          next
        }
        s <- sims[i, j]
        if (s < threshold) {
          break
        } # rows are sorted descending
        k <- k + 1L
        rows[[k]] <- list(from = ids[[i]], to = ids[[j]], similarity = s)
        per_node_count[[i]] <- per_node_count[[i]] + 1L
        per_node_count[[j]] <- per_node_count[[j]] + 1L
      }
    }
  } else {
    # Sparse / variable-length embeddings (e.g. TF-IDF bags): fall back to
    # explicit pairwise loop with list accumulation and per-node cap.
    per_node_count <- integer(n)
    rows <- list()
    k <- 0L
    for (i in seq_len(n - 1L)) {
      if (per_node_count[[i]] >= max_per_node) {
        next
      }
      for (j in seq(i + 1L, n)) {
        if (per_node_count[[j]] >= max_per_node) {
          next
        }
        s <- tryCatch(
          cosine_similarity(embeddings[[i]], embeddings[[j]]),
          error = function(e) 0
        )
        if (s >= threshold) {
          k <- k + 1L
          rows[[k]] <- list(from = ids[[i]], to = ids[[j]], similarity = s)
          per_node_count[[i]] <- per_node_count[[i]] + 1L
          per_node_count[[j]] <- per_node_count[[j]] + 1L
        }
      }
    }
  }

  if (k == 0L) {
    return(empty_df)
  }

  data.frame(
    from = vapply(rows[seq_len(k)], `[[`, character(1L), "from"),
    to = vapply(rows[seq_len(k)], `[[`, character(1L), "to"),
    similarity = vapply(rows[seq_len(k)], `[[`, numeric(1L), "similarity"),
    stringsAsFactors = FALSE
  )
}
