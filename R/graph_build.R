#' Build CALLS edges between function nodes
#'
#' Cross-references each function node's `calls_list` against the set of
#' known user-defined node identifiers.  Each matched call produces one
#' directed edge `(from_node_id -> to_node_id)`.
#'
#' The matching strategy is:
#' 1. Exact match on bare function name (`to_name`).
#' 2. Qualified match: if `calls_list` contains `pkg::fn` or `pkg:::fn`,
#'    the bare `fn` part is matched against known node names.
#'
#' Only edges between nodes present in `func_nodes` are returned (intra-project
#' edges).  Calls to external packages are intentionally excluded here; use
#' [build_import_edges()] for package-level dependencies.
#'
#' @param func_nodes A list of node records as returned by
#'   [extract_function_nodes()].
#'
#' @return A `data.frame` with columns:
#'   \describe{
#'     \item{`from`}{Character: `node_id` of the calling function.}
#'     \item{`to`}{Character: `node_id` of the called function.}
#'     \item{`weight`}{Numeric: edge weight (default `1`).}
#'   }
#'   Zero rows if no intra-project calls are found.
#'
#' @seealso [build_import_edges()], [build_test_edges()]
#' @export
#' @examples
#' \dontrun{
#' proj  <- detect_rproject("/path/to/mypkg")
#' nodes <- extract_function_nodes(proj$r_files)
#' edges <- build_call_edges(nodes)
#' }
build_call_edges <- function(func_nodes) {
  if (length(func_nodes) == 0L) {
    return(.empty_edge_df())
  }

  # Build lookup: bare_name -> vector of node_ids (many nodes can share a name
  # across files; retain all so we keep the best match later)
  node_ids <- vapply(func_nodes, `[[`, character(1), "node_id")
  node_names <- vapply(func_nodes, `[[`, character(1), "name")
  name_to_ids <- split(node_ids, node_names)

  from_vec <- character(0)
  to_vec <- character(0)

  for (node in func_nodes) {
    calls <- unique(node$calls_list)
    if (length(calls) == 0L) {
      next
    }

    for (sym in calls) {
      # Try bare match first
      bare <- sub("^.*:::?", "", sym) # strip pkg:: or pkg:::
      candidates <- name_to_ids[[bare]]
      if (is.null(candidates)) {
        next
      }

      # Self-calls are valid edges (recursion)
      for (target_id in candidates) {
        from_vec <- c(from_vec, node$node_id)
        to_vec <- c(to_vec, target_id)
      }
    }
  }

  if (length(from_vec) == 0L) {
    return(.empty_edge_df())
  }

  unique(data.frame(
    from = from_vec,
    to = to_vec,
    weight = 1,
    stringsAsFactors = FALSE
  ))
}

#' Build IMPORTS edges from R files to package dependencies
#'
#' Discovers package dependencies at two levels:
#'
#' 1. **Explicit** -- `library(pkg)` and `require(pkg)` calls in source files.
#' 2. **Qualified** -- `pkg::fn` and `pkg:::fn` patterns (the package name is
#'    extracted as an import).
#' 3. **DESCRIPTION** -- if a `DESCRIPTION` file is found in the project root
#'    (or any ancestor up to two levels up), its `Imports:` and `Depends:`
#'    fields are parsed.
#'
#' Each discovered dependency produces an edge from the *file stem* (or the
#' project root stem) to the package name.  Base-R packages (`base`, `methods`,
#' `utils`, `stats`, `datasets`, `graphics`, `grDevices`) are included when
#' explicitly referenced but are labelled with `source = "qualified"` or
#' `source = "description"` for downstream filtering.
#'
#' @param r_files  Character vector of absolute paths to `.R` source files.
#' @param root     Optional character(1). Project root directory.  When
#'   provided, a `DESCRIPTION` file is searched there.
#'
#' @return A `data.frame` with columns:
#'   \describe{
#'     \item{`from`}{File stem (or project identifier) where the import appears.}
#'     \item{`to`}{Package name being imported.}
#'     \item{`weight`}{Numeric: edge weight (default `1`).}
#'     \item{`source`}{Character: `"library"`, `"qualified"`, or `"description"`.}
#'   }
#'
#' @seealso [build_call_edges()], [build_test_edges()]
#' @export
#' @examples
#' \dontrun{
#' proj  <- detect_rproject("/path/to/mypkg")
#' edges <- build_import_edges(proj$r_files, root = proj$root)
#' }
build_import_edges <- function(r_files, root = NULL) {
  rows <- list()

  for (fpath in r_files) {
    src <- tryCatch(readLines(fpath, warn = FALSE), error = function(e) {
      character(0)
    })
    if (length(src) == 0L) {
      next
    }

    fstem <- as.character(fs::path_ext_remove(fs::path_file(fpath)))

    # ---- 1. library() / require() ----------------------------------------
    lib_matches <- regmatches(
      src,
      gregexpr(
        "(?:library|require)\\s*\\(\\s*['\"]?([A-Za-z][A-Za-z0-9._]*)['\"]?\\s*\\)",
        src,
        perl = TRUE
      )
    )
    for (m_vec in lib_matches) {
      for (m in m_vec) {
        pkg <- sub(
          ".*(?:library|require)\\s*\\(\\s*['\"]?([A-Za-z][A-Za-z0-9._]*)['\"]?.*",
          "\\1",
          m,
          perl = TRUE
        )
        if (nchar(pkg) > 0L) {
          rows <- c(
            rows,
            list(data.frame(
              from = fstem,
              to = pkg,
              weight = 1,
              source = "library",
              stringsAsFactors = FALSE
            ))
          )
        }
      }
    }

    # ---- 2. pkg::fn qualified calls --------------------------------------
    qual_matches <- regmatches(
      src,
      gregexpr(
        "([A-Za-z][A-Za-z0-9._]*):::?[A-Za-z._][A-Za-z0-9._]*",
        src,
        perl = TRUE
      )
    )
    for (m_vec in qual_matches) {
      for (m in m_vec) {
        pkg <- sub(":::?.*$", "", m)
        if (nchar(pkg) > 0L) {
          rows <- c(
            rows,
            list(data.frame(
              from = fstem,
              to = pkg,
              weight = 1,
              source = "qualified",
              stringsAsFactors = FALSE
            ))
          )
        }
      }
    }
  }

  # ---- 3. DESCRIPTION Imports / Depends --------------------------------
  if (!is.null(root)) {
    desc_path <- .find_description(root)
    if (!is.null(desc_path)) {
      desc_rows <- .parse_description_deps(desc_path)
      rows <- c(rows, desc_rows)
    }
  }

  if (length(rows) == 0L) {
    return(data.frame(
      from = character(0),
      to = character(0),
      weight = numeric(0),
      source = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # De-duplicate, keeping first source priority: library > description > qualified
  result <- unique(do.call(rbind, rows))
  result[order(result$from, result$to), ]
}

#' Build TEST edges from test files to user-defined functions
#'
#' Parses each test file for references to user-defined function names, using
#' the same AST-walking approach used in `find_calls_in_body()`.  A `TEST`
#' edge `(test_file_stem -> function_node_id)` is emitted when a test file
#' calls a function that exists as a node in `func_nodes`.
#'
#' Test-helper symbols (`expect_*`, `test_that`, `describe`, `it`, `setup`,
#' `teardown`, `skip*`, `withr::*`) are excluded from matching to avoid false
#' edges to non-existent nodes.
#'
#' @param func_nodes A list of function node records from
#'   [extract_function_nodes()].
#' @param test_files Character vector of absolute paths to test `.R` files.
#'
#' @return A `data.frame` with columns:
#'   \describe{
#'     \item{`from`}{Test file stem, character.}
#'     \item{`to`}{`node_id` of the tested function, character.}
#'     \item{`weight`}{Numeric: edge weight (default `1`).}
#'   }
#'
#' @seealso [build_call_edges()], [build_import_edges()]
#' @export
#' @examples
#' \dontrun{
#' proj  <- detect_rproject("/path/to/mypkg")
#' nodes <- extract_function_nodes(proj$r_files)
#' edges <- build_test_edges(nodes, proj$test_files)
#' }
build_test_edges <- function(func_nodes, test_files) {
  if (length(func_nodes) == 0L || length(test_files) == 0L) {
    return(.empty_edge_df())
  }

  # Build lookup: bare_name -> node_id (prefer first match when duplicates)
  node_ids <- vapply(func_nodes, `[[`, character(1), "node_id")
  node_names <- vapply(func_nodes, `[[`, character(1), "name")
  name_to_id <- setNames(node_ids, node_names)

  # Symbols to exclude: testthat + withr helpers
  test_helpers <- c(
    "test_that",
    "describe",
    "it",
    "expect_equal",
    "expect_identical",
    "expect_true",
    "expect_false",
    "expect_null",
    "expect_na",
    "expect_error",
    "expect_warning",
    "expect_message",
    "expect_no_error",
    "expect_no_warning",
    "expect_no_message",
    "expect_length",
    "expect_named",
    "expect_type",
    "expect_s3_class",
    "expect_s4_class",
    "expect_vector",
    "expect_output",
    "expect_gt",
    "expect_gte",
    "expect_lt",
    "expect_lte",
    "expect_setequal",
    "expect_contains",
    "expect_in",
    "setup",
    "teardown",
    "skip",
    "skip_if",
    "skip_if_not",
    "skip_if_not_installed",
    "skip_on_cran",
    "skip_on_ci",
    "local_tempdir",
    "local_tempfile",
    "source",
    "library",
    "require"
  )

  from_vec <- character(0)
  to_vec <- character(0)

  for (tpath in test_files) {
    parsed <- tryCatch(
      parse(tpath, keep.source = FALSE),
      error = function(e) NULL
    )
    if (is.null(parsed)) {
      next
    }

    # Use find_calls_in_body on each expression
    calls <- character(0)
    for (expr in as.list(parsed)) {
      new_calls <- tryCatch(
        find_calls_in_body(expr),
        error = function(e) character(0)
      )
      calls <- c(calls, new_calls)
    }
    calls <- setdiff(unique(calls), test_helpers)

    fstem <- as.character(fs::path_ext_remove(fs::path_file(tpath)))

    for (sym in calls) {
      bare <- sub("^.*:::?", "", sym)
      if (!nzchar(bare) || !bare %in% names(name_to_id)) {
        next
      }
      nid <- name_to_id[[bare]]
      if (!is.null(nid)) {
        from_vec <- c(from_vec, fstem)
        to_vec <- c(to_vec, nid)
      }
    }
  }

  if (length(from_vec) == 0L) {
    return(.empty_edge_df())
  }

  unique(data.frame(
    from = from_vec,
    to = to_vec,
    weight = 1,
    stringsAsFactors = FALSE
  ))
}

# ---- internal helpers -----------------------------------------------

#' @keywords internal
.empty_edge_df <- function() {
  data.frame(
    from = character(0),
    to = character(0),
    weight = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
.find_description <- function(root) {
  # Look in root and up to 2 levels up
  for (i in 0:2) {
    candidate <- fs::path(root, strrep("../", i), "DESCRIPTION")
    norm_path <- tryCatch(fs::path_abs(candidate), error = function(e) NULL)
    if (!is.null(norm_path) && fs::file_exists(norm_path)) {
      return(as.character(norm_path))
    }
  }
  NULL
}

#' @keywords internal
.parse_description_deps <- function(desc_path) {
  tryCatch(
    {
      desc <- read.dcf(desc_path)
      fields <- c("Imports", "Depends")
      rows <- list()

      for (field in fields) {
        if (!field %in% colnames(desc)) {
          next
        }
        raw <- desc[1L, field]
        if (is.na(raw) || nchar(trimws(raw)) == 0L) {
          next
        }

        # Split on comma, extract bare package name (strip version constraint)
        pkgs <- trimws(strsplit(raw, ",")[[1L]])
        pkgs <- sub("\\s*\\(.*\\)$", "", pkgs) # remove (>= x.y.z)
        pkgs <- pkgs[nchar(pkgs) > 0L & pkgs != "R"]

        project_stem <- as.character(
          fs::path_ext_remove(fs::path_file(desc_path))
        )
        for (pkg in pkgs) {
          rows <- c(
            rows,
            list(data.frame(
              from = project_stem,
              to = pkg,
              weight = 1,
              source = "description",
              stringsAsFactors = FALSE
            ))
          )
        }
      }
      rows
    },
    error = function(e) list()
  )
}

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
#'
#' @param project_path Character(1).  Path to the R project root.
#'   Defaults to `"."`.
#' @param embed_method Character(1).  Embedding back-end: `"tfidf"` (default),
#'   `"ollama"`, or `"openai"`.  Only `"tfidf"` is available in Sprint 1.
#' @param include_package_nodes Logical(1).  When `TRUE` (default), one node
#'   per unique external package is added to the graph.
#' @param semantic_threshold Numeric(1).  Minimum cosine similarity for a
#'   `SEMANTIC` edge to be created.  Default `0.7`.
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
      complexity = integer(0),
      pagerank = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  # ---- 3. Combine edge data frames for igraph -------------------------
  all_edges <- .assemble_edges(call_edges, import_edges, test_edges, vertex_df)

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
    semantic_threshold
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
.make_function_vertex_df <- function(func_nodes) {
  if (length(func_nodes) == 0L) {
    return(data.frame(
      name = character(0),
      node_type = character(0),
      file = character(0),
      line_start = integer(0),
      line_end = integer(0),
      signature = character(0),
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
.build_semantic_edges <- function(embeddings, threshold) {
  ids <- names(embeddings)
  n <- length(ids)
  if (n < 2L) {
    return(data.frame(
      from = character(0),
      to = character(0),
      similarity = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  from_v <- character(0)
  to_v <- character(0)
  sim_v <- numeric(0)

  for (i in seq_len(n - 1L)) {
    for (j in seq(i + 1L, n)) {
      sim <- tryCatch(
        cosine_similarity(embeddings[[i]], embeddings[[j]]),
        error = function(e) 0
      )
      if (sim >= threshold) {
        from_v <- c(from_v, ids[[i]])
        to_v <- c(to_v, ids[[j]])
        sim_v <- c(sim_v, sim)
      }
    }
  }

  data.frame(
    from = from_v,
    to = to_v,
    similarity = sim_v,
    stringsAsFactors = FALSE
  )
}
