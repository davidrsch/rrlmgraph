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
