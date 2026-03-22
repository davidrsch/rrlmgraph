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
.empty_dispatch_edge_df <- function() {
  data.frame(
    from = character(0),
    to = character(0),
    weight = numeric(0),
    edge_type = character(0),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
.git_available <- function(project_root) {
  git_dir <- file.path(as.character(project_root), ".git")
  if (!file.exists(git_dir)) {
    return(FALSE)
  }
  tryCatch(
    identical(
      system2(
        "git",
        c(
          "-C",
          shQuote(as.character(project_root)),
          "rev-parse",
          "--is-inside-work-tree"
        ),
        stdout = FALSE,
        stderr = FALSE
      ),
      0L
    ),
    error = function(e) FALSE
  )
}

#' @keywords internal
.parse_git_log_files <- function(lines) {
  result <- list()
  current_hash <- NULL
  current_files <- character(0)
  past_blank <- FALSE

  for (line in lines) {
    trimmed <- trimws(line)
    if (grepl("^[0-9a-f]{40}$", trimmed, perl = TRUE)) {
      if (!is.null(current_hash) && length(current_files) > 0L) {
        result <- c(result, list(current_files))
      }
      current_hash <- trimmed
      current_files <- character(0)
      past_blank <- FALSE
    } else if (nchar(trimmed) == 0L) {
      past_blank <- TRUE
    } else if (past_blank && nchar(trimmed) > 0L) {
      current_files <- c(current_files, trimmed)
    }
  }
  if (!is.null(current_hash) && length(current_files) > 0L) {
    result <- c(result, list(current_files))
  }
  result
}

#' Extract pairs of regex capture groups from all matches in \code{text}.
#' \code{pattern} must have exactly two capturing groups.
#' Returns a list of \code{character(2)} vectors, one per match.
#' @keywords internal
.regex_all_captures2 <- function(text, pattern) {
  matches <- gregexpr(pattern, text, perl = TRUE)
  result <- list()
  for (i in seq_along(text)) {
    m <- matches[[i]]
    if (length(m) == 0L || m[[1L]] == -1L) {
      next
    }
    starts <- attr(m, "capture.start")
    lengths <- attr(m, "capture.length")
    if (is.null(starts) || ncol(starts) < 2L) {
      next
    }
    for (j in seq_len(nrow(starts))) {
      g1 <- substr(
        text[[i]],
        starts[j, 1L],
        starts[j, 1L] + lengths[j, 1L] - 1L
      )
      g2 <- substr(
        text[[i]],
        starts[j, 2L],
        starts[j, 2L] + lengths[j, 2L] - 1L
      )
      if (nzchar(g1) && nzchar(g2)) {
        result <- c(result, list(c(g1, g2)))
      }
    }
  }
  result
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
