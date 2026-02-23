# tests/testthat/test-generate-instructions.R
# Unit tests for generate_instructions()
# rrlmgraph issue #21 acceptance criteria.

skip_if_not_installed("igraph")

# ---- fixture helpers ------------------------------------------------

make_gen_graph <- function(project_root = NULL) {
  verts <- data.frame(
    name = c(
      "mypkg::load_data",
      "mypkg::clean",
      "mypkg::model_fit",
      "mypkg::predict_outcome",
      "mypkg::evaluate"
    ),
    node_type = "function",
    pagerank = c(0.30, 0.25, 0.20, 0.15, 0.10),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = c("mypkg::load_data", "mypkg::clean", "mypkg::model_fit"),
    to = c("mypkg::clean", "mypkg::model_fit", "mypkg::predict_outcome"),
    weight = 1.0,
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    d = edges,
    vertices = verts,
    directed = TRUE
  )
  igraph::graph_attr(g, "project_name") <- "mypkg"
  if (!is.null(project_root)) {
    igraph::graph_attr(g, "project_root") <- project_root
  }
  class(g) <- c("rrlm_graph", class(g))
  g
}

# Minimal DESCRIPTION for tests
write_description <- function(dir, pkg = "mypkg", imports = "cli, fs") {
  writeLines(
    c(
      paste0("Package: ", pkg),
      "Version: 0.0.1",
      paste0("Imports: ", imports),
      "Type: Package"
    ),
    file.path(dir, "DESCRIPTION")
  )
}

# ---- basic output ---------------------------------------------------

test_that("generate_instructions writes a file", {
  tmp <- withr::local_tempdir()
  write_description(tmp)
  g <- make_gen_graph(project_root = tmp)
  out <- generate_instructions(g, output_path = file.path(tmp, "test.md"))
  expect_true(file.exists(out))
})

test_that("generate_instructions returns path invisibly", {
  tmp <- withr::local_tempdir()
  g <- make_gen_graph(project_root = tmp)
  res <- withVisible(
    generate_instructions(g, output_path = file.path(tmp, "test.md"))
  )
  expect_false(res$visible)
  expect_true(file.exists(res$value))
})

# ---- Markdown structure ---------------------------------------------

test_that("output contains project type section", {
  tmp <- withr::local_tempdir()
  write_description(tmp, pkg = "testpkg")
  g <- make_gen_graph(project_root = tmp)
  out_path <- file.path(tmp, "instr.md")
  generate_instructions(g, output_path = out_path)
  content <- paste(readLines(out_path), collapse = "\n")
  expect_match(content, "Project Overview", ignore.case = TRUE)
  expect_match(content, "Type", ignore.case = FALSE)
})

test_that("output contains R version", {
  tmp <- withr::local_tempdir()
  g <- make_gen_graph(project_root = tmp)
  out <- file.path(tmp, "instr.md")
  generate_instructions(g, output_path = out)
  content <- paste(readLines(out), collapse = "\n")
  expect_match(content, R.Version()$major)
})

test_that("output contains top functions section", {
  tmp <- withr::local_tempdir()
  g <- make_gen_graph(project_root = tmp)
  out <- file.path(tmp, "instr.md")
  generate_instructions(g, output_path = out)
  content <- paste(readLines(out), collapse = "\n")
  expect_match(content, "Top Functions", ignore.case = TRUE)
  expect_match(content, "mypkg::load_data")
})

test_that("output contains package dependencies section", {
  tmp <- withr::local_tempdir()
  write_description(tmp, imports = "cli, fs")
  g <- make_gen_graph(project_root = tmp)
  out <- file.path(tmp, "instr.md")
  generate_instructions(g, output_path = out)
  content <- paste(readLines(out), collapse = "\n")
  expect_match(content, "Package Dependencies", ignore.case = TRUE)
  expect_match(content, "cli")
  expect_match(content, "fs")
})

test_that("output contains MCP server pointer", {
  tmp <- withr::local_tempdir()
  g <- make_gen_graph(project_root = tmp)
  out <- file.path(tmp, "instr.md")
  generate_instructions(g, output_path = out)
  content <- paste(readLines(out), collapse = "\n")
  expect_match(content, "MCP", ignore.case = FALSE)
  expect_match(content, "rrlmgraph")
})

# ---- token budget ---------------------------------------------------

test_that("output stays within max_tokens * 4 chars", {
  tmp <- withr::local_tempdir()
  g <- make_gen_graph(project_root = tmp)
  out <- file.path(tmp, "instr.md")
  max_tok <- 400L
  generate_instructions(g, output_path = out, max_tokens = max_tok)
  content <- paste(readLines(out), collapse = "\n")
  # Allow slight overshoot due to trimming footer
  expect_lte(nchar(content), max_tok * 4L + 100L)
})

test_that("output without max_tokens constraint is valid Markdown", {
  tmp <- withr::local_tempdir()
  g <- make_gen_graph(project_root = tmp)
  out <- file.path(tmp, "instr.md")
  generate_instructions(g, output_path = out, max_tokens = 2000L)
  content <- readLines(out)
  # First line should be a Markdown heading
  expect_match(content[1L], "^#")
})

# ---- default output path -------------------------------------------

test_that("default output path is .github/copilot-instructions.md", {
  tmp <- withr::local_tempdir()
  g <- make_gen_graph(project_root = tmp)
  out <- generate_instructions(g)
  expect_equal(
    normalizePath(out, winslash = "/"),
    normalizePath(
      file.path(tmp, ".github", "copilot-instructions.md"),
      winslash = "/"
    )
  )
  expect_true(file.exists(out))
})

# ---- overwrite flag -------------------------------------------------

test_that("generate_instructions overwrites existing file with fresh header", {
  tmp <- withr::local_tempdir()
  g <- make_gen_graph(project_root = tmp)
  out <- file.path(tmp, "instr.md")

  writeLines("OLD CONTENT", out)
  generate_instructions(g, output_path = out)

  content <- paste(readLines(out), collapse = "\n")
  expect_false(grepl("OLD CONTENT", content))
  expect_match(content, "^#")
})

# ---- argument validation --------------------------------------------

test_that("generate_instructions rejects non-igraph input", {
  expect_error(generate_instructions(list()), "igraph")
})

test_that("generate_instructions rejects max_tokens < 100", {
  g <- make_gen_graph()
  expect_error(generate_instructions(g, max_tokens = 10L), "max_tokens")
})

# ---- .gi_project_type internal --------------------------------------

test_that(".gi_project_type detects Shiny app via app.R", {
  tmp <- withr::local_tempdir()
  writeLines("shinyApp(ui, server)", file.path(tmp, "app.R"))
  result <- rrlmgraph:::.gi_project_type(tmp)
  expect_equal(result, "Shiny application")
})

test_that(".gi_project_type detects Shiny app via server.R", {
  tmp <- withr::local_tempdir()
  writeLines("function(input, output) {}", file.path(tmp, "server.R"))
  result <- rrlmgraph:::.gi_project_type(tmp)
  expect_equal(result, "Shiny application")
})

test_that(".gi_project_type detects R Markdown project", {
  tmp <- withr::local_tempdir()
  writeLines("# Analysis\nSome text.", file.path(tmp, "analysis.Rmd"))
  result <- rrlmgraph:::.gi_project_type(tmp)
  expect_equal(result, "R Markdown / Quarto project")
})

test_that(".gi_project_type returns generic 'R project' when no structure found", {
  tmp <- withr::local_tempdir() # empty dir
  result <- rrlmgraph:::.gi_project_type(tmp)
  expect_equal(result, "R project")
})

test_that(".gi_project_type respects explicit Type field in DESCRIPTION", {
  tmp <- withr::local_tempdir()
  writeLines(
    c("Package: mypkg", "Version: 0.1.0", "Type: Package"),
    file.path(tmp, "DESCRIPTION")
  )
  result <- rrlmgraph:::.gi_project_type(tmp)
  expect_equal(result, "Package")
})

# ---- .gi_conventions internal ----------------------------------------

test_that(".gi_conventions detects camelCase naming", {
  verts <- data.frame(
    name = c(
      "pkg::loadData",
      "pkg::cleanData",
      "pkg::fitModel",
      "pkg::predictOutcome",
      "pkg::evaluateModel",
      "pkg::buildGraph",
      "pkg::runBenchmark",
      "pkg::createNode"
    ),
    node_type = "function",
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    d = data.frame(
      from = character(0),
      to = character(0),
      stringsAsFactors = FALSE
    ),
    vertices = verts,
    directed = TRUE
  )
  tmp <- withr::local_tempdir()
  result <- rrlmgraph:::.gi_conventions(g, tmp)
  expect_true(any(grepl("camel", result, ignore.case = TRUE)))
})

test_that(".gi_conventions detects magrittr pipe in R files", {
  tmp <- withr::local_tempdir()
  r_dir <- file.path(tmp, "R")
  dir.create(r_dir)
  writeLines(
    c("library(dplyr)", "f <- function(x) x %>% filter(!is.na(x))"),
    file.path(r_dir, "utils.R")
  )
  # Graph with snake_case names so naming convention doesn't trigger
  verts <- data.frame(
    name = c(
      "pkg::foo",
      "pkg::bar",
      "pkg::baz",
      "pkg::qux",
      "pkg::quux",
      "pkg::corge"
    ),
    node_type = "function",
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    d = data.frame(
      from = character(0),
      to = character(0),
      stringsAsFactors = FALSE
    ),
    vertices = verts,
    directed = TRUE
  )
  result <- rrlmgraph:::.gi_conventions(g, tmp)
  expect_true(any(
    grepl("magrittr", result, ignore.case = TRUE) |
      grepl("pipe", result, ignore.case = TRUE)
  ))
})

test_that(".gi_conventions returns fallback message when no conventions detected", {
  # Very small graph (<=5 names) and empty dirs → no conventions detected
  tmp <- withr::local_tempdir()
  verts <- data.frame(
    name = c("pkg::a", "pkg::b"),
    node_type = "function",
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    d = data.frame(
      from = character(0),
      to = character(0),
      stringsAsFactors = FALSE
    ),
    vertices = verts,
    directed = TRUE
  )
  result <- rrlmgraph:::.gi_conventions(g, tmp)
  expect_true(any(grepl("No strong", result, ignore.case = TRUE)))
})

# ---- .gi_package_inventory internal --------------------------------------

test_that(".gi_package_inventory includes Suggests field packages", {
  tmp <- withr::local_tempdir()
  writeLines(
    c(
      "Package: mypkg",
      "Version: 0.1.0",
      "Imports: cli",
      "Suggests: testthat, withr"
    ),
    file.path(tmp, "DESCRIPTION")
  )
  result <- rrlmgraph:::.gi_package_inventory(tmp)
  expect_s3_class(result, "data.frame")
  expect_true("testthat" %in% result$package)
  expect_true("withr" %in% result$package)
})

test_that(".gi_package_inventory returns empty df when no DESCRIPTION", {
  tmp <- withr::local_tempdir()
  result <- rrlmgraph:::.gi_package_inventory(tmp)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that(".gi_package_inventory returns empty df when DESCRIPTION has no deps", {
  tmp <- withr::local_tempdir()
  writeLines(
    c("Package: mypkg", "Version: 0.1.0"),
    file.path(tmp, "DESCRIPTION")
  )
  result <- rrlmgraph:::.gi_package_inventory(tmp)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

# ---- .gi_render_markdown edge cases ---------------------------------

test_that(".gi_render_markdown shows '_No functions found._' for empty top_fns", {
  md <- rrlmgraph:::.gi_render_markdown(
    pkg_name = "testpkg",
    build_date = "2024-01-01",
    proj_type = "R package",
    r_version = "4.4.0",
    top_fns = data.frame(
      name = character(0),
      pagerank = numeric(0),
      stringsAsFactors = FALSE
    ),
    conventions = "snake_case",
    pkg_inventory = data.frame(
      package = character(0),
      version = character(0),
      stringsAsFactors = FALSE
    ),
    max_tokens = 2000L
  )
  expect_match(md, "No functions found", ignore.case = TRUE)
})

test_that(".gi_render_markdown shows '_No dependencies found._' for empty inventory", {
  md <- rrlmgraph:::.gi_render_markdown(
    pkg_name = "testpkg",
    build_date = "2024-01-01",
    proj_type = "R package",
    r_version = "4.4.0",
    top_fns = data.frame(
      name = character(0),
      pagerank = numeric(0),
      stringsAsFactors = FALSE
    ),
    conventions = "snake_case",
    pkg_inventory = data.frame(
      package = character(0),
      version = character(0),
      stringsAsFactors = FALSE
    ),
    max_tokens = 2000L
  )
  expect_match(md, "No dependencies found", ignore.case = TRUE)
})

test_that(".gi_render_markdown trims to token budget and appends notice", {
  md <- rrlmgraph:::.gi_render_markdown(
    pkg_name = "testpkg",
    build_date = "2024-01-01",
    proj_type = "R package",
    r_version = "4.4.0",
    top_fns = data.frame(
      name = "mypkg::fn",
      pagerank = 0.5,
      stringsAsFactors = FALSE
    ),
    conventions = "snake_case",
    pkg_inventory = data.frame(
      package = character(0),
      version = character(0),
      stringsAsFactors = FALSE
    ),
    max_tokens = 100L # very tight: forces trim
  )
  expect_match(md, "trimmed", ignore.case = TRUE)
})
