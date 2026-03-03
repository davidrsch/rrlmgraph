# R/cache.R
# Graph caching and SQLite export for rrlmgraph.
# Covers rrlmgraph issues #14 (cache) and #15 (SQLite bridge).

# ---- save_graph_cache ------------------------------------------------

#' Save an rrlm_graph to a local cache directory
#'
#' Serialises the graph to \file{.rrlmgraph/graph.rds} and writes a
#' human-readable \file{.rrlmgraph/config.yml} alongside a
#' \file{.rrlmgraph/.gitignore} that keeps the RDS out of version
#' control while tracking \file{config.yml} and
#' \file{task_trace.jsonl}.
#'
#' @param graph An \code{rrlm_graph} / \code{igraph} object.
#' @param cache_dir Character(1) or \code{NULL}.  Path to the cache
#'   directory.  Defaults to \code{.rrlmgraph/} inside the
#'   \code{"project_root"} graph attribute, or the current working
#'   directory when the attribute is absent.
#'
#' @return \code{cache_dir}, invisibly.
#' @seealso [load_graph_cache()], [is_cache_stale()]
#' @export
#' @examples
#' \dontrun{
#' g <- build_rrlm_graph("mypkg")
#' save_graph_cache(g)
#' }
save_graph_cache <- function(graph, cache_dir = NULL) {
  if (!inherits(graph, "igraph")) {
    cli::cli_abort("{.arg graph} must be an igraph / rrlm_graph object.")
  }

  cache_dir <- .resolve_cache_dir(graph, cache_dir)
  fs::dir_create(cache_dir)

  # --- graph.rds -------------------------------------------------------
  rds_path <- fs::path(cache_dir, "graph.rds")
  saveRDS(graph, file = rds_path, compress = "xz")

  # --- config.yml ------------------------------------------------------
  yml_path <- fs::path(cache_dir, "config.yml")
  meta <- .graph_metadata_list(graph)
  yml_lines <- .write_yaml_lines(meta)
  writeLines(yml_lines, yml_path)

  # --- .gitignore -------------------------------------------------------
  gi_path <- fs::path(cache_dir, ".gitignore")
  if (!fs::file_exists(gi_path)) {
    writeLines(
      c(
        "# rrlmgraph cache -- generated automatically",
        "*.rds",
        "!config.yml",
        "!task_trace.jsonl"
      ),
      gi_path
    )
  }

  cli::cli_inform("Graph cached at {.path {cache_dir}}")
  invisible(cache_dir)
}

# ---- load_graph_cache ------------------------------------------------

#' Load an rrlm_graph from a local cache
#'
#' Reads \file{.rrlmgraph/graph.rds} from \code{project_path}, validates
#' compatibility with the installed rrlmgraph version, refreshes task-trace
#' weights via \code{update_task_weights()}, and returns the graph.
#'
#' @param project_path Character(1).  Root directory of the R project
#'   (parent of \file{.rrlmgraph/}).
#'
#' @return An \code{rrlm_graph} object, or \code{NULL} invisibly when no
#'   cache exists.
#' @seealso [save_graph_cache()], [is_cache_stale()]
#' @export
#' @examples
#' \dontrun{
#' g <- load_graph_cache("mypkg")
#' }
load_graph_cache <- function(project_path = ".") {
  cache_dir <- file.path(project_path, ".rrlmgraph")
  rds_path <- file.path(cache_dir, "graph.rds")

  if (!file.exists(rds_path)) {
    cli::cli_warn("No cache found at {.path {rds_path}}.")
    return(invisible(NULL))
  }

  graph <- tryCatch(
    readRDS(rds_path),
    error = function(e) {
      cli::cli_abort("Failed to load cache: {conditionMessage(e)}")
    }
  )

  if (!inherits(graph, "igraph")) {
    cli::cli_abort(
      "Cached object at {.path {rds_path}} is not an igraph object."
    )
  }

  # Validate version compatibility
  .validate_cache_version(cache_dir)

  # Refresh task-trace weights
  graph <- tryCatch(
    update_task_weights(graph, useful_nodes = character(0)),
    error = function(e) graph
  )

  cli::cli_inform("Graph loaded from {.path {cache_dir}}")
  graph
}

# ---- is_cache_stale --------------------------------------------------

#' Check whether the graph cache is stale
#'
#' Compares the modification time of the cached \file{graph.rds} against
#' the modification times of all \file{.R} files under
#' \code{project_path}.  Returns \code{TRUE} if any R file is newer than
#' the cache.
#'
#' @param project_path Character(1).  Root directory of the R project.
#'
#' @return Logical(1): \code{TRUE} if the cache is stale or absent.
#' @seealso [save_graph_cache()], [load_graph_cache()]
#' @export
#' @examples
#' \dontrun{
#' if (is_cache_stale("mypkg")) {
#'   g <- build_rrlm_graph("mypkg")
#'   save_graph_cache(g)
#' }
#' }
is_cache_stale <- function(project_path = ".") {
  rds_path <- file.path(project_path, ".rrlmgraph", "graph.rds")

  if (!file.exists(rds_path)) {
    return(TRUE)
  }

  cache_mtime <- file.info(rds_path)$mtime

  r_files <- tryCatch(
    fs::dir_ls(project_path, regexp = "\\.R$", recurse = TRUE),
    error = function(e) character(0)
  )

  if (length(r_files) == 0L) {
    return(FALSE)
  }

  # Exclude non-source directories that contain .R files but should not
  # invalidate the cache: renv/, packrat/, _targets/, .rrlmgraph/, and
  # the auto-generated test snapshot dirs.
  exclude_rx <- paste(
    c(
      "/renv/",
      "/packrat/",
      "/_targets/",
      "/\\.rrlmgraph/",
      "/node_modules/",
      "/\\.git/"
    ),
    collapse = "|"
  )
  r_files <- r_files[!grepl(exclude_rx, r_files, perl = TRUE)]

  r_mtimes <- file.info(as.character(r_files))$mtime
  any(!is.na(r_mtimes) & r_mtimes > cache_mtime)
}

