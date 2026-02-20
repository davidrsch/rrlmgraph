#' Detect an R project type and discover its files
#'
#' Inspects a directory and returns a named list describing the project type,
#' all discovered R/Rmd/qmd source files, and (if applicable) a parsed
#' `DESCRIPTION`.  Directories that are typically not user-authored
#' (`renv`, `packrat`, `.Rproj.user`, `node_modules`, `.git`) are excluded
#' from file discovery.
#'
#' @param path `character(1)`. Path to the project root.  Defaults to the
#'   current working directory.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{`type`}{Character(1): one of `"package"`, `"shiny"`, `"quarto"`,
#'       `"rmarkdown"`, `"script"`.}
#'     \item{`r_files`}{Character vector of absolute paths to `.R` files.}
#'     \item{`test_files`}{Character vector of absolute paths to files under
#'       `tests/testthat/`.}
#'     \item{`rmd_files`}{Character vector of absolute paths to `.Rmd` files.}
#'     \item{`qmd_files`}{Character vector of absolute paths to `.qmd` files.}
#'     \item{`description`}{Named list of DESCRIPTION fields (or `NULL`).}
#'     \item{`root`}{Character(1): the absolute path of the project root.}
#'   }
#'
#' @seealso [extract_function_nodes()], [build_rrlm_graph()]
#' @export
#' @examples
#' \dontrun{
#' # Detect the type of a package on disk
#' detect_rproject("/path/to/mypackage")
#' }
detect_rproject <- function(path = ".") {
  root <- fs::path_abs(path)
  if (!fs::dir_exists(root)) {
    cli::cli_abort("{.path {root}} is not an existing directory.")
  }

  # Build exclude regexp: any path component matching these names is excluded
  exclude_segments <- c(
    "renv",
    "packrat",
    "\\.Rproj\\.user",
    "node_modules",
    "\\.git"
  )
  exclude_re <- paste0("/(", paste(exclude_segments, collapse = "|"), ")(/|$)")

  # Discover files (all extensions, then partition)
  r_files <- .ls_filtered(root, "\\.R$", exclude_re)
  rmd_files <- .ls_filtered(root, "\\.Rmd$", exclude_re)
  qmd_files <- .ls_filtered(root, "\\.qmd$", exclude_re)

  # test_files: subset of r_files under tests/testthat/
  test_root <- as.character(fs::path(root, "tests", "testthat"))
  test_files <- r_files[startsWith(r_files, test_root)]

  # Detect project type (most specific signal wins)
  type <- .detect_type(root)

  # Parse DESCRIPTION for packages
  description <- NULL
  desc_path <- fs::path(root, "DESCRIPTION")
  if (type == "package" && fs::file_exists(desc_path)) {
    description <- as.list(read.dcf(desc_path)[1L, , drop = TRUE])
  }

  list(
    type = type,
    r_files = r_files,
    test_files = test_files,
    rmd_files = rmd_files,
    qmd_files = qmd_files,
    description = description,
    root = as.character(root)
  )
}

# ---- internal helpers -----------------------------------------------

#' @keywords internal
.ls_filtered <- function(root, ext_re, exclude_re) {
  tryCatch(
    {
      all_paths <- fs::dir_ls(
        root,
        recurse = TRUE,
        regexp = ext_re,
        fail = FALSE
      )
      # Normalise backslashes (Windows) for consistent regex matching
      normalised <- gsub("\\\\", "/", as.character(all_paths))
      kept <- normalised[!grepl(exclude_re, normalised, perl = TRUE)]
      as.character(fs::path_abs(kept))
    },
    error = function(e) {
      cli::cli_warn(
        "File discovery failed in {.path {root}}: {conditionMessage(e)}"
      )
      character(0)
    }
  )
}

#' @keywords internal
.detect_type <- function(root) {
  has_file <- function(...) fs::file_exists(fs::path(root, ...))

  if (has_file("DESCRIPTION")) {
    return("package")
  }

  if (has_file("app.R") || (has_file("ui.R") && has_file("server.R"))) {
    return("shiny")
  }

  if (
    has_file("_quarto.yml") ||
      length(fs::dir_ls(root, regexp = "\\.qmd$", fail = FALSE)) > 0
  ) {
    return("quarto")
  }

  if (length(fs::dir_ls(root, regexp = "\\.Rmd$", fail = FALSE)) > 0) {
    return("rmarkdown")
  }

  "script"
}
