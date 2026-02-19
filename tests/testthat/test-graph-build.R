# ---- shared fixture -------------------------------------------------
# Build a small set of fake nodes simulating mini_ds_project output

make_fake_nodes <- function() {
  list(
    list(
      node_id = "data_prep::load_data",
      name = "load_data",
      file = "/fake/R/data_prep.R",
      calls_list = c("file.exists", "utils::read.csv", "stop"),
      signature = "load_data(path)",
      body_text = "if (!file.exists(path)) stop() ; utils::read.csv(path)",
      roxygen_text = "#' Load data"
    ),
    list(
      node_id = "data_prep::clean_data",
      name = "clean_data",
      file = "/fake/R/data_prep.R",
      calls_list = c("dplyr::filter", "dplyr::mutate", "mean", "sd"),
      signature = "clean_data(df)",
      body_text = "df |> dplyr::filter(!is.na(score)) |> dplyr::mutate(score_z = (score - mean(score)) / sd(score))",
      roxygen_text = "#' Clean data"
    ),
    list(
      node_id = "data_prep::split_data",
      name = "split_data",
      file = "/fake/R/data_prep.R",
      calls_list = c("UseMethod"),
      signature = "split_data(df, ratio = 0.8, ...)",
      body_text = "UseMethod('split_data')",
      roxygen_text = ""
    ),
    list(
      node_id = "data_prep::split_data.data.frame",
      name = "split_data.data.frame",
      file = "/fake/R/data_prep.R",
      calls_list = c("nrow", "floor", "seq_len"),
      signature = "split_data.data.frame(df, ratio = 0.8, ...)",
      body_text = "n <- nrow(df); train_n <- floor(n * ratio); idx <- seq_len(n); list(train=df[idx[seq_len(train_n)],], test=df[idx[seq(train_n+1,n)],])",
      roxygen_text = ""
    ),
    list(
      node_id = "models::fit_model",
      name = "fit_model",
      file = "/fake/R/models.R",
      calls_list = c("stats::lm"),
      signature = "fit_model(train_data, formula)",
      body_text = "model <- stats::lm(formula=formula, data=train_data); class(model) <- c('lm_result', class(model)); model",
      roxygen_text = "#' Fit model"
    ),
    list(
      node_id = "models::compute_rmse",
      name = "compute_rmse",
      file = "/fake/R/models.R",
      calls_list = c("sqrt", "mean"),
      signature = "compute_rmse(predicted, actual)",
      body_text = "sqrt(mean((predicted - actual)^2))",
      roxygen_text = "#' Compute RMSE"
    ),
    list(
      node_id = "benchmark::run_benchmark",
      name = "run_benchmark",
      file = "/fake/R/benchmark.R",
      calls_list = c("load_data", "clean_data", "split_data", "fit_model"),
      signature = "run_benchmark(data_path, formula = score ~ .)",
      body_text = "raw <- load_data(data_path); clean <- clean_data(raw); splits <- split_data(clean, ratio = 0.8); fit_model(splits$train, formula)",
      roxygen_text = "#' Run benchmark"
    ),
    list(
      node_id = "benchmark::calculate_mrr",
      name = "calculate_mrr",
      file = "/fake/R/benchmark.R",
      calls_list = c("mean"),
      signature = "calculate_mrr(ranked_results)",
      body_text = "rr <- vapply(ranked_results, function(r) { first_hit <- which(as.logical(r))[1L]; if (is.na(first_hit)) 0 else 1 / first_hit }, numeric(1)); mean(rr)",
      roxygen_text = "#' Calculate MRR"
    )
  )
}

# ---- build_call_edges -----------------------------------------------

test_that("build_call_edges returns empty data frame for empty input", {
  result <- build_call_edges(list())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("from", "to", "weight"))
})

test_that("build_call_edges finds intra-project call edge", {
  nodes <- make_fake_nodes()
  edges <- build_call_edges(nodes)

  # run_benchmark calls load_data, clean_data, split_data, fit_model
  expect_true(
    any(
      edges$from == "benchmark::run_benchmark" &
        edges$to == "data_prep::load_data"
    )
  )
  expect_true(
    any(
      edges$from == "benchmark::run_benchmark" &
        edges$to == "models::fit_model"
    )
  )
})

test_that("build_call_edges does NOT create edges to external packages", {
  nodes <- make_fake_nodes()
  edges <- build_call_edges(nodes)

  # dplyr, stats, utils are external — must not appear as a to= value
  expect_false(any(edges$to == "dplyr"))
  expect_false(any(edges$to == "stats::lm"))
  expect_false(any(edges$to == "utils::read.csv"))
})

test_that("build_call_edges edges are deduplicated", {
  nodes <- make_fake_nodes()
  edges <- build_call_edges(nodes)

  n_dupes <- sum(duplicated(paste(edges$from, edges$to)))
  expect_equal(n_dupes, 0L)
})

test_that("build_call_edges result has correct columns and types", {
  edges <- build_call_edges(make_fake_nodes())

  expect_named(edges, c("from", "to", "weight"))
  expect_type(edges$from, "character")
  expect_type(edges$to, "character")
  expect_type(edges$weight, "double")
})

# ---- build_import_edges ---------------------------------------------

test_that("build_import_edges returns empty frame for empty input", {
  result <- build_import_edges(character(0))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("build_import_edges detects library() calls", {
  f <- withr::local_tempfile(fileext = ".R")
  writeLines(c("library(dplyr)", "library(ggplot2)", "x <- 1"), f)

  edges <- build_import_edges(f)
  pkgs <- edges$to

  expect_true("dplyr" %in% pkgs)
  expect_true("ggplot2" %in% pkgs)
})

test_that("build_import_edges detects require() calls", {
  f <- withr::local_tempfile(fileext = ".R")
  writeLines("require(jsonlite)", f)

  edges <- build_import_edges(f)
  expect_true("jsonlite" %in% edges$to)
})

test_that("build_import_edges detects pkg:: qualified calls", {
  f <- withr::local_tempfile(fileext = ".R")
  writeLines("out <- fs::path_abs('.')", f)

  edges <- build_import_edges(f)
  expect_true("fs" %in% edges$to)
})

test_that("build_import_edges parses DESCRIPTION Imports", {
  root <- withr::local_tempdir()
  writeLines(
    c(
      "Package: mypkg",
      "Version: 0.1.0",
      "Imports: dplyr, ggplot2 (>= 3.0.0), purrr"
    ),
    file.path(root, "DESCRIPTION")
  )
  f <- withr::local_tempfile(fileext = ".R")
  writeLines("f <- function(x) x", f)

  edges <- build_import_edges(f, root = root)
  desc_pkgs <- edges$to[edges$source == "description"]

  expect_true("dplyr" %in% desc_pkgs)
  expect_true("ggplot2" %in% desc_pkgs)
  expect_true("purrr" %in% desc_pkgs)
  expect_false("R" %in% desc_pkgs)
})

test_that("build_import_edges source column has valid values", {
  f <- withr::local_tempfile(fileext = ".R")
  writeLines(c("library(dplyr)", "fs::path_abs('.')"), f)

  edges <- build_import_edges(f)
  expect_true(all(edges$source %in% c("library", "qualified", "description")))
})

# ---- build_test_edges -----------------------------------------------

test_that("build_test_edges returns empty frame for empty input", {
  result <- build_test_edges(list(), character(0))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("build_test_edges links test file to tested function", {
  nodes <- make_fake_nodes()

  f <- withr::local_tempfile(fileext = ".R")
  writeLines(
    c(
      "test_that('compute_rmse is zero for perfect predictions', {",
      "  expect_equal(compute_rmse(1:3, 1:3), 0)",
      "})"
    ),
    f
  )

  edges <- build_test_edges(nodes, f)

  expect_true(
    any(edges$to == "models::compute_rmse")
  )
})

test_that("build_test_edges does NOT link testthat helpers as edges", {
  nodes <- make_fake_nodes()

  f <- withr::local_tempfile(fileext = ".R")
  writeLines(
    c(
      "test_that('x', {",
      "  expect_true(TRUE)",
      "})"
    ),
    f
  )

  edges <- build_test_edges(nodes, f)
  bad <- c("test_that", "expect_true", "expect_equal")
  expect_false(any(edges$to %in% bad))
  expect_false(any(edges$from %in% bad))
})

test_that("build_test_edges result has correct columns", {
  nodes <- make_fake_nodes()
  f <- withr::local_tempfile(fileext = ".R")
  writeLines("test_that('x', load_data('p'))", f)

  edges <- build_test_edges(nodes, f)
  if (nrow(edges) > 0L) {
    expect_named(edges, c("from", "to", "weight"))
  }
})

# ---- ground-truth recall test (integration) -------------------------

test_that("call edges recall >= 85% on mini_ds_project fixture", {
  bench_root <- system.file(
    "projects/mini_ds_project",
    package = "rrlmgraphbench",
    mustWork = FALSE
  )
  skip_if(
    nchar(bench_root) == 0L,
    "rrlmgraphbench not installed — skipping ground-truth recall test"
  )

  gt_path <- system.file(
    "ground_truth/call_edges.rds",
    package = "rrlmgraphbench"
  )
  gt_edges <- readRDS(gt_path)

  proj <- detect_rproject(bench_root)
  nodes <- extract_function_nodes(proj$r_files)
  edges <- build_call_edges(nodes)

  # Count how many ground-truth intra-project edges are in our output
  gt_intra <- gt_edges[
    gt_edges$to %in%
      vapply(nodes, `[[`, character(1L), "node_id"),
  ]
  if (nrow(gt_intra) == 0L) {
    skip("No intra-project edges in ground truth")
  }

  matched <- mapply(
    function(f, t) {
      any(edges$from == f & edges$to == t)
    },
    gt_intra$from,
    gt_intra$to
  )

  recall <- mean(matched)
  expect_gte(
    recall,
    0.85,
    label = sprintf("Call edge recall %.0f%% < 85%%", recall * 100)
  )
})
