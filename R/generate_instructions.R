# R/generate_instructions.R
# AI coding-assistant instruction generator for rrlmgraph.
# Covers rrlmgraph issue #21 (Sprint 3).

# ---- generate_instructions -------------------------------------------

#' Generate a Copilot-style instruction file from the project graph
#'
#' Extracts key information from the \code{rrlm_graph} and writes a
#' Markdown instruction document suitable for AI coding assistants
#' (e.g. \file{.github/copilot-instructions.md}).  The output summarises
#' the project type, R version, top functions by PageRank, inferred coding
#' conventions, and installed package inventory.
#'
#' @param graph An \code{rrlm_graph} / \code{igraph} object.
#' @param output_path Character(1) or \code{NULL}.  Destination file path.
#'   When \code{NULL} (default) the file is written to
#'   \file{.github/copilot-instructions.md} inside the project root stored
#'   in \code{graph_attr(graph, "project_root")}.
#' @param max_tokens Integer(1).  Soft upper bound on output size in tokens
#'   (approximated as \code{nchar / 4}).  Content is trimmed to stay under
#'   this limit.  Default \code{2000L}.
#'
#' @return The path to the written file, invisibly.
#'
#' @seealso [build_rrlm_graph()], [query_context()]
#' @export
#' @examples
#' \dontrun{
#' g <- build_rrlm_graph("mypkg")
#' generate_instructions(g)
#' }
generate_instructions <- function(
    graph,
    output_path = NULL,
    max_tokens = 2000L
) {
    # ---------- validate -------------------------------------------------
    if (!inherits(graph, "igraph")) {
        cli::cli_abort("{.arg graph} must be an igraph / rrlm_graph object.")
    }
    max_tokens <- as.integer(max_tokens)
    if (is.na(max_tokens) || max_tokens < 100L) {
        cli::cli_abort("{.arg max_tokens} must be a positive integer >= 100.")
    }

    # ---------- 1. Extract project metadata ------------------------------
    root <- igraph::graph_attr(graph, "project_root") %||% getwd()
    proj_type <- .gi_project_type(root)
    r_version <- paste(R.Version()$major, R.Version()$minor, sep = ".")
    pkg_name <- .gi_pkg_name(root)
    build_date <- format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC")

    # ---------- 2. Top 15 functions by PageRank --------------------------
    top_fns <- .gi_top_functions(graph, n = 15L)

    # ---------- 3. Infer coding conventions ------------------------------
    conventions <- .gi_conventions(graph, root)

    # ---------- 4. Package inventory ------------------------------------
    pkg_inventory <- .gi_package_inventory(root)

    # ---------- 5. Assemble Markdown -------------------------------------
    md <- .gi_render_markdown(
        pkg_name = pkg_name,
        build_date = build_date,
        proj_type = proj_type,
        r_version = r_version,
        top_fns = top_fns,
        conventions = conventions,
        pkg_inventory = pkg_inventory,
        max_tokens = max_tokens
    )

    # ---------- 6. Determine output path ---------------------------------
    if (is.null(output_path)) {
        gh_dir <- file.path(root, ".github")
        if (!dir.exists(gh_dir)) {
            dir.create(gh_dir, recursive = TRUE)
        }
        output_path <- file.path(gh_dir, "copilot-instructions.md")
    }

    writeLines(md, con = output_path, useBytes = FALSE)
    cli::cli_inform("Instruction file written to {.path {output_path}}.")

    invisible(output_path)
}

# ---- internal helpers ------------------------------------------------

#' Classify the project type
#' @keywords internal
.gi_project_type <- function(root) {
    if (file.exists(file.path(root, "DESCRIPTION"))) {
        desc <- tryCatch(
            read.dcf(file.path(root, "DESCRIPTION")),
            error = function(e) NULL
        )
        if (!is.null(desc)) {
            type_field <- desc[1L, "Type"]
            if (!is.na(type_field) && nzchar(type_field)) {
                return(type_field)
            }
        }
        return("R package")
    }
    if (
        file.exists(file.path(root, "app.R")) ||
            file.exists(file.path(root, "server.R"))
    ) {
        return("Shiny application")
    }
    if (
        length(list.files(
            root,
            pattern = "\\.Rmd$|\\.qmd$",
            recursive = FALSE
        )) >
            0L
    ) {
        return("R Markdown / Quarto project")
    }
    "R project"
}

#' Extract package name from DESCRIPTION
#' @keywords internal
.gi_pkg_name <- function(root) {
    desc_path <- file.path(root, "DESCRIPTION")
    if (!file.exists(desc_path)) {
        return(basename(root))
    }
    desc <- tryCatch(read.dcf(desc_path), error = function(e) NULL)
    if (is.null(desc)) {
        return(basename(root))
    }
    nm <- desc[1L, "Package"]
    if (is.na(nm) || !nzchar(nm)) basename(root) else nm
}

#' Return top-n function records sorted by PageRank
#' @keywords internal
.gi_top_functions <- function(graph, n = 15L) {
    if (igraph::vcount(graph) == 0L) {
        return(data.frame(
            name = character(0),
            pagerank = numeric(0),
            stringsAsFactors = FALSE
        ))
    }

    pr <- igraph::vertex_attr(graph, "pagerank")
    nms <- igraph::V(graph)$name
    types <- igraph::vertex_attr(graph, "node_type")

    fn_mask <- is.null(types) | types %in% c("function", NA)
    if (!is.null(types)) {
        fn_mask <- types %in% c("function", NA_character_)
    }

    pr_num <- as.numeric(pr)
    pr_num[is.na(pr_num)] <- 0

    ord <- order(pr_num, decreasing = TRUE)
    ord <- ord[fn_mask[ord]]
    ord <- utils::head(ord, n)

    data.frame(
        name = nms[ord],
        pagerank = round(pr_num[ord], 6),
        stringsAsFactors = FALSE
    )
}

#' Infer coding conventions from graph vertex attributes and source files
#' @keywords internal
.gi_conventions <- function(graph, root) {
    conventions <- character(0)

    # Naming style: inspect function names
    nms <- igraph::V(graph)$name
    nms <- sub("^.*::", "", nms) # strip pkg::
    nms <- nms[nzchar(nms)]

    if (length(nms) > 5L) {
        has_snake <- mean(grepl("_", nms)) > 0.4
        has_dot <- mean(grepl("\\.", nms)) > 0.4
        has_camel <- mean(grepl("[a-z][A-Z]", nms)) > 0.2

        if (has_snake && !has_camel) {
            conventions <- c(conventions, "Naming: snake_case")
        } else if (has_camel && !has_snake) {
            conventions <- c(conventions, "Naming: camelCase")
        } else if (has_dot) {
            conventions <- c(conventions, "Naming: dot.case (S3 convention)")
        }
    }

    # Test framework: check tests/ directory
    test_files <- list.files(
        file.path(root, "tests"),
        recursive = TRUE,
        pattern = "\\.R$",
        full.names = FALSE
    )
    if (length(test_files) > 0L) {
        conventions <- c(conventions, "Tests: testthat")
    }

    # Pipe operator: scan R files for |> vs %>%
    r_files <- list.files(
        file.path(root, "R"),
        pattern = "\\.R$",
        full.names = TRUE
    )
    if (length(r_files) > 0L) {
        src <- tryCatch(
            unlist(lapply(r_files, readLines, warn = FALSE)),
            error = function(e) character(0)
        )
        native_pipe <- sum(grepl("|>", src, fixed = TRUE))
        magrittr_pipe <- sum(grepl("%>%", src, fixed = TRUE))
        if (native_pipe > 0L || magrittr_pipe > 0L) {
            pipe_style <- if (native_pipe >= magrittr_pipe) {
                "native |>"
            } else {
                "magrittr %>%"
            }
            conventions <- c(conventions, paste0("Pipe: ", pipe_style))
        }
    }

    if (length(conventions) == 0L) {
        conventions <- "No strong conventions detected"
    }

    conventions
}

#' List installed packages referenced in DESCRIPTION
#' @keywords internal
.gi_package_inventory <- function(root) {
    desc_path <- file.path(root, "DESCRIPTION")
    if (!file.exists(desc_path)) {
        return(data.frame(
            package = character(0),
            version = character(0),
            stringsAsFactors = FALSE
        ))
    }
    desc <- tryCatch(read.dcf(desc_path), error = function(e) NULL)
    if (is.null(desc)) {
        return(data.frame(
            package = character(0),
            version = character(0),
            stringsAsFactors = FALSE
        ))
    }

    # Collect package names from Imports + Suggests + Depends
    fields <- c("Imports", "Suggests", "Depends", "LinkingTo")
    pkg_raw <- character(0)
    for (f in fields) {
        if (f %in% colnames(desc) && !is.na(desc[1L, f])) {
            raw <- desc[1L, f]
            # Split on comma, strip version constraints and whitespace
            parts <- strsplit(raw, ",")[[1L]]
            parts <- sub("\\s*\\(.*\\)", "", parts)
            parts <- trimws(parts)
            parts <- parts[nzchar(parts) & parts != "R"]
            pkg_raw <- c(pkg_raw, parts)
        }
    }
    pkg_raw <- unique(pkg_raw)

    if (length(pkg_raw) == 0L) {
        return(data.frame(
            package = character(0),
            version = character(0),
            stringsAsFactors = FALSE
        ))
    }

    versions <- vapply(
        pkg_raw,
        function(p) {
            tryCatch(
                as.character(utils::packageVersion(p)),
                error = function(e) "not installed"
            )
        },
        character(1)
    )

    data.frame(
        package = pkg_raw,
        version = versions,
        stringsAsFactors = FALSE
    )
}

#' Render the Markdown document and trim to token budget
#' @keywords internal
.gi_render_markdown <- function(
    pkg_name,
    build_date,
    proj_type,
    r_version,
    top_fns,
    conventions,
    pkg_inventory,
    max_tokens
) {
    lines <- character(0)

    # Header
    lines <- c(
        lines,
        paste0("# ", pkg_name, " -- Copilot Instructions"),
        paste0("> Auto-generated by rrlmgraph on ", build_date),
        ""
    )

    # Project overview
    lines <- c(
        lines,
        "## Project Overview",
        "",
        paste0("- **Type**: ", proj_type),
        paste0("- **R version**: ", r_version),
        paste0("- **Package**: `", pkg_name, "`"),
        ""
    )

    # Top functions
    lines <- c(lines, "## Top Functions (by PageRank)", "")
    if (nrow(top_fns) > 0L) {
        rows <- apply(top_fns, 1L, function(r) {
            paste0("- `", r[["name"]], "` (score: ", r[["pagerank"]], ")")
        })
        lines <- c(lines, rows)
    } else {
        lines <- c(lines, "_No functions found._")
    }
    lines <- c(lines, "")

    # Coding conventions
    lines <- c(lines, "## Coding Conventions", "")
    for (cv in conventions) {
        lines <- c(lines, paste0("- ", cv))
    }
    lines <- c(lines, "")

    # Package inventory
    lines <- c(lines, "## Package Dependencies", "")
    if (nrow(pkg_inventory) > 0L) {
        rows <- apply(pkg_inventory, 1L, function(r) {
            paste0("- **", r[["package"]], "** ", r[["version"]])
        })
        lines <- c(lines, rows)
    } else {
        lines <- c(lines, "_No dependencies found._")
    }
    lines <- c(lines, "")

    # MCP server pointer
    lines <- c(
        lines,
        "## MCP Server",
        "",
        paste0(
            "This project is indexed by the **rrlmgraph** MCP server.  ",
            "Connect Claude Desktop or another MCP-aware client to ",
            "`rrlmgraph-mcp` for live context retrieval."
        ),
        ""
    )

    # Token budget: approx 4 chars per token
    budget_chars <- max_tokens * 4L
    full_text <- paste(lines, collapse = "\n")

    if (nchar(full_text) > budget_chars) {
        full_text <- substr(full_text, 1L, budget_chars)
        # Ensure we don't cut mid-line
        last_nl <- max(gregexpr("\n", full_text)[[1L]])
        if (last_nl > 0L) {
            full_text <- substr(full_text, 1L, last_nl)
        }
        full_text <- paste0(
            full_text,
            "\n\n_[Content trimmed to token budget]_\n"
        )
    }

    full_text
}
