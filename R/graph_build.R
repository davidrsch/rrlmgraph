# ---- build_rrlm_graph -----------------------------------------------

#' Build an RLM-Graph for an R project
#'
#' The primary user-facing function.  Orchestrates project detection, AST
#' parsing, edge construction, TF-IDF (or alternative) embedding, and igraph
#' assembly into a typed knowledge graph.
#'
#' The returned object inherits from `igraph` and carries the class
#' `"rrlm_graph"`.  Graph-level metadata is stored as igraph graph attributes
#' (accessible with `igraph::graph_attr()`).
#'
#' ## Node types
#' | Type | Description |
#' |------|-------------|
#' | `"function"` | User-defined R function |
#' | `"package"` | External package dependency |
#' | `"testfile"` | Test file that references user functions |
#'
#' ## Edge types
#' | Type | Description |
#' |------|-------------|
#' | `"CALLS"` | Intra-project function call |
#' | `"IMPORTS"` | File/project imports a package |
#' | `"TESTS"` | Test file covers a user function |
#' | `"SEMANTIC"` | Cosine similarity >= `semantic_threshold` |
#' | `"CO_CHANGES"` | Two functions co-edited in >= `min_cochanges` commits (from `build_co_change_edges()`) |
#' | `"DISPATCHES_ON"` | S4/R5 generic dispatches on a class (from `build_dispatch_edges()`) |
#' | `"EXTENDS"` | Class inherits from another class (from `build_dispatch_edges()`) |
#'
#' @param project_path Character(1).  Path to the R project root.
#'   Defaults to `"."`.
#' @param embed_method Character(1).  Embedding back-end: `"tfidf"` (default),
#'   `"ollama"`, or `"openai"`.  Only `"tfidf"` is available in Sprint 1.
#' @param include_package_nodes Logical(1).  When `TRUE` (default), one node
#'   per unique external package is added to the graph.
#' @param semantic_threshold Numeric(1).  Minimum cosine similarity for a
#'   `SEMANTIC` edge to be created.  Default `0.7`.
#' @param max_semantic_edges Integer(1).  Maximum SEMANTIC edges to create per
#'   node.  Capping at a small number (default `5L`) prevents dense graphs on
#'   large projects.  Semantic edges are disabled entirely when the graph has
#'   more than 300 function nodes.
#' @param cache Logical(1).  When `TRUE` (default), the graph is serialised
#'   to `<project_root>/.rrlmgraph/graph.rds`.
#' @param verbose Logical(1).  When `TRUE`, progress messages are printed via
#'   [cli::cli_inform()].  Default `FALSE`.
#'
#' @return An object of class `c("rrlm_graph", "igraph")`.
#'
#' @seealso [summary.rrlm_graph()], [print.rrlm_graph()],
#'   [plot.rrlm_graph()], [detect_rproject()], [extract_function_nodes()]
#' @export
#' @examples
#' \dontrun{
#' g <- build_rrlm_graph("/path/to/mypkg")
#' summary(g)
#' plot(g)
#' }
build_rrlm_graph <- function(
  project_path = ".",
  embed_method = "tfidf",
  include_package_nodes = TRUE,
  semantic_threshold = 0.7,
  max_semantic_edges = 5L,
  cache = TRUE,
  verbose = FALSE
) {
  t0 <- proc.time()[["elapsed"]]
  root <- as.character(fs::path_abs(project_path))
  .vlog <- function(...) if (verbose) cli::cli_inform(...)

  .vlog("Detecting project at {.path {root}}")
  proj <- detect_rproject(root)

  .vlog("Parsing {length(proj$r_files)} R file(s)")
  func_nodes <- extract_function_nodes(proj$r_files)

  if (length(func_nodes) == 0L) {
    cli::cli_warn(
      "No function nodes found in {.path {root}}.  Empty graph returned."
    )
  }

  # ---- 1. Build edge data frames ---------------------------------------
  .vlog("Building CALLS edges")
  call_edges <- build_call_edges(func_nodes)

  .vlog("Building IMPORT edges")
  import_edges <- build_import_edges(proj$r_files, root = root)

  .vlog("Building TEST edges")
  test_edges <- build_test_edges(func_nodes, proj$test_files)

  .vlog("Building CO_CHANGES edges from git history")
  cochange_edges <- tryCatch(
    build_co_change_edges(func_nodes, project_root = root),
    error = function(e) {
      cli::cli_warn("CO_CHANGES edge build failed: {conditionMessage(e)}")
      .empty_edge_df()
    }
  )

  .vlog("Building DISPATCHES_ON / EXTENDS edges")
  dispatch_edges <- tryCatch(
    build_dispatch_edges(func_nodes, proj$r_files),
    error = function(e) {
      cli::cli_warn("Dispatch edge build failed: {conditionMessage(e)}")
      .empty_dispatch_edge_df()
    }
  )

  # ---- 2. Vertex data frame -------------------------------------------
  fn_vertex_df <- .make_function_vertex_df(func_nodes)

  # Optional: package nodes (one per unique external package in import edges)
  pkg_vertex_df <- NULL
  if (include_package_nodes && nrow(import_edges) > 0L) {
    pkgs <- unique(import_edges$to)
    # Exclude any package name that is already a node_id (unlikely but safe)
    pkgs <- setdiff(pkgs, fn_vertex_df$name)
    if (length(pkgs) > 0L) {
      pkg_vertex_df <- data.frame(
        name = pkgs,
        node_type = "package",
        file = NA_character_,
        line_start = NA_integer_,
        line_end = NA_integer_,
        signature = pkgs,
        body_text = NA_character_,
        roxygen_text = NA_character_,
        complexity = NA_integer_,
        pagerank = NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }

  # Test-file nodes (sources of TEST edges)
  testfile_vertex_df <- NULL
  if (nrow(test_edges) > 0L) {
    test_stems <- unique(test_edges$from)
    # Only add nodes not already in fn_vertex_df
    new_stems <- setdiff(test_stems, fn_vertex_df$name)
    if (length(new_stems) > 0L) {
      testfile_vertex_df <- data.frame(
        name = new_stems,
        node_type = "testfile",
        file = NA_character_,
        line_start = NA_integer_,
        line_end = NA_integer_,
        signature = new_stems,
        body_text = NA_character_,
        roxygen_text = NA_character_,
        complexity = NA_integer_,
        pagerank = NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }

  vertex_df <- do.call(
    rbind,
    Filter(
      Negate(is.null),
      list(fn_vertex_df, pkg_vertex_df, testfile_vertex_df)
    )
  )

  if (is.null(vertex_df) || nrow(vertex_df) == 0L) {
    vertex_df <- data.frame(
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
    )
  }

  # ---- 3. Combine edge data frames for igraph -------------------------
  all_edges <- .assemble_edges(call_edges, import_edges, test_edges, vertex_df)

  # Append CO_CHANGES edges (bidirectional; weight = co-change frequency)
  if (nrow(cochange_edges) > 0L) {
    cc <- cochange_edges
    cc$edge_type <- "CO_CHANGES"
    cc <- cc[
      cc$from %in% vertex_df$name & cc$to %in% vertex_df$name,
      ,
      drop = FALSE
    ]
    if (nrow(cc) > 0L) {
      all_edges <- rbind(
        all_edges,
        cc[, c("from", "to", "weight", "edge_type")]
      )
    }
  }

  # Append DISPATCHES_ON + EXTENDS edges
  if (nrow(dispatch_edges) > 0L) {
    de <- dispatch_edges[
      dispatch_edges$from %in%
        vertex_df$name &
        dispatch_edges$to %in% vertex_df$name,
      ,
      drop = FALSE
    ]
    if (nrow(de) > 0L) {
      all_edges <- rbind(
        all_edges,
        de[, c("from", "to", "weight", "edge_type")]
      )
    }
  }

  # ---- 4. Build igraph object -----------------------------------------
  .vlog("Assembling igraph")
  g <- tryCatch(
    {
      if (nrow(all_edges) > 0L) {
        igraph::graph_from_data_frame(
          d = all_edges[, c("from", "to", "weight", "edge_type")],
          vertices = vertex_df,
          directed = TRUE
        )
      } else {
        igraph::graph_from_data_frame(
          d = data.frame(
            from = character(0),
            to = character(0),
            weight = numeric(0),
            edge_type = character(0),
            stringsAsFactors = FALSE
          ),
          vertices = vertex_df,
          directed = TRUE
        )
      }
    },
    error = function(e) {
      cli::cli_warn("igraph assembly error: {conditionMessage(e)}")
      igraph::make_empty_graph()
    }
  )

  # ---- 5. PageRank centrality ----------------------------------------
  .vlog("Computing PageRank")
  pr_scores <- tryCatch(
    igraph::page_rank(g)$vector,
    error = function(e) rep(0, igraph::vcount(g))
  )
  igraph::V(g)$pagerank <- as.numeric(pr_scores)

  # Initialise task_trace_weight to 0.0 (neutral / no prior history).
  # Using 0.0 instead of 0.5 keeps the relevance formula unbiased on fresh
  # graphs and ensures compute_relevance() returns 0 when all signals are 0.
  igraph::V(g)$task_trace_weight <- 0.0

  # ---- 6. Embeddings --------------------------------------------------
  .vlog("Embedding nodes with method '{embed_method}'")
  fn_only <- Filter(function(n) n$node_id %in% fn_vertex_df$name, func_nodes)
  embed_result <- tryCatch(
    embed_nodes(fn_only, method = embed_method),
    error = function(e) {
      cli::cli_warn("Embedding failed: {conditionMessage(e)}")
      list(embeddings = list(), model = NULL)
    }
  )

  # Store embeddings on relevant vertices
  igraph::V(g)$embedding <- vector("list", igraph::vcount(g))
  for (nid in names(embed_result$embeddings)) {
    idx <- which(igraph::V(g)$name == nid)
    if (length(idx) == 1L) {
      igraph::V(g)$embedding[[idx]] <- embed_result$embeddings[[nid]]
    }
  }

  # ---- 7. SEMANTIC_SIMILARITY edges -----------------------------------
  .vlog("Computing semantic similarity edges (threshold {semantic_threshold})")
  sem_edges <- .build_semantic_edges(
    embed_result$embeddings,
    semantic_threshold,
    max_per_node = as.integer(max_semantic_edges)
  )
  if (nrow(sem_edges) > 0L) {
    vertex_names <- igraph::V(g)$name
    from_idx <- match(sem_edges$from, vertex_names)
    to_idx <- match(sem_edges$to, vertex_names)
    edge_mat <- cbind(from_idx, to_idx)
    valid_edges <- stats::complete.cases(edge_mat)

    if (!all(valid_edges)) {
      cli::cli_warn(
        "Skipping {sum(!valid_edges)} semantic similarity edge(s) whose nodes are not present in the graph."
      )
    }

    if (any(valid_edges)) {
      edge_vec <- as.vector(t(edge_mat[valid_edges, , drop = FALSE]))
      g <- igraph::add_edges(
        g,
        edges = edge_vec,
        attr = list(
          weight = sem_edges$similarity[valid_edges],
          edge_type = rep("SEMANTIC", sum(valid_edges))
        )
      )
    }
  }

  # ---- 8. Graph metadata ----------------------------------------------
  build_time <- proc.time()[["elapsed"]] - t0
  cache_path <- as.character(fs::path(root, ".rrlmgraph", "graph.rds"))

  igraph::graph_attr(g, "project_name") <- basename(root)
  igraph::graph_attr(g, "project_root") <- root
  igraph::graph_attr(g, "project_type") <- proj$type
  igraph::graph_attr(g, "r_version") <- paste(
    R.version$major,
    R.version$minor,
    sep = "."
  )
  igraph::graph_attr(g, "build_time") <- build_time
  igraph::graph_attr(g, "build_at") <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  igraph::graph_attr(g, "embed_method") <- embed_method
  igraph::graph_attr(g, "embed_model") <- embed_result$model
  igraph::graph_attr(g, "cache_path") <- cache_path

  # ---- 9. Class assignment -------------------------------------------
  class(g) <- c("rrlm_graph", class(g))

  # ---- 10. Cache ------------------------------------------------------
  if (cache) {
    cache_dir <- dirname(cache_path)
    if (!fs::dir_exists(cache_dir)) {
      tryCatch(fs::dir_create(cache_dir, recurse = TRUE), error = function(e) {
        NULL
      })
    }
    tryCatch(saveRDS(g, cache_path), error = function(e) {
      cli::cli_warn(
        "Cache write failed: {conditionMessage(e)}"
      )
    })
  }

  .vlog(
    "Done in {round(build_time, 2)}s -- {igraph::vcount(g)} nodes, {igraph::ecount(g)} edges"
  )
  g
}

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
