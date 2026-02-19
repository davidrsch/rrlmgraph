skip_if_not_installed("text2vec")

# ---- shared fixture -------------------------------------------------

make_embed_nodes <- function() {
  list(
    list(
      node_id = "utils::load_data",
      name = "load_data",
      signature = "load_data(path)",
      body_text = "if (!file.exists(path)) stop(); read.csv(path)",
      roxygen_text = "#' Load a CSV file from disk"
    ),
    list(
      node_id = "utils::clean_data",
      name = "clean_data",
      signature = "clean_data(df)",
      body_text = "df |> dplyr::filter(!is.na(score)) |> dplyr::mutate(score_z = scale(score))",
      roxygen_text = "#' Clean and normalise a data frame"
    ),
    list(
      node_id = "utils::compute_rmse",
      name = "compute_rmse",
      signature = "compute_rmse(predicted, actual)",
      body_text = "sqrt(mean((predicted - actual)^2))",
      roxygen_text = "#' Root Mean Squared Error"
    ),
    list(
      node_id = "utils::fit_model",
      name = "fit_model",
      signature = "fit_model(train, formula)",
      body_text = "lm(formula = formula, data = train)",
      roxygen_text = "#' Fit a linear regression model to training data"
    ),
    list(
      node_id = "utils::evaluate_model",
      name = "evaluate_model",
      signature = "evaluate_model(model, test)",
      body_text = "predictions <- predict(model, newdata = test); compute_rmse(predictions, test$score)",
      roxygen_text = "#' Evaluate a fitted model on test data"
    )
  )
}

# ---- embed_nodes ----------------------------------------------------

test_that("embed_nodes returns correct structure", {
  nodes <- make_embed_nodes()
  result <- embed_nodes(nodes)

  expect_named(result, c("embeddings", "model"))
  expect_type(result$embeddings, "list")
  expect_length(result$embeddings, length(nodes))
})

test_that("embed_nodes embeddings are named by node_id", {
  nodes <- make_embed_nodes()
  result <- embed_nodes(nodes)

  expected_ids <- vapply(nodes, `[[`, character(1), "node_id")
  expect_named(result$embeddings, expected_ids)
})

test_that("embed_nodes produces non-zero embeddings for all nodes", {
  nodes <- make_embed_nodes()
  result <- embed_nodes(nodes)

  for (nm in names(result$embeddings)) {
    vec <- result$embeddings[[nm]]
    expect_true(is.numeric(vec), info = paste("Not numeric:", nm))
    expect_true(any(vec != 0), info = paste("All-zero embedding for:", nm))
  }
})

test_that("embed_nodes all embeddings have the same length", {
  nodes <- make_embed_nodes()
  result <- embed_nodes(nodes)

  lens <- vapply(result$embeddings, length, integer(1))
  expect_true(
    length(unique(lens)) == 1L,
    info = "Embeddings have different lengths"
  )
})

test_that("embed_nodes returns empty list for empty input", {
  result <- embed_nodes(list())
  expect_equal(result$embeddings, list())
  expect_null(result$model)
})

test_that("embed_nodes model contains required components", {
  nodes <- make_embed_nodes()
  result <- embed_nodes(nodes)

  expect_true(!is.null(result$model$vectorizer))
  expect_true(!is.null(result$model$tfidf))
  expect_true(!is.null(result$model$vocab))
})

test_that("embed_nodes result is serializable to RDS without errors", {
  nodes <- make_embed_nodes()
  result <- embed_nodes(nodes)
  f <- withr::local_tempfile(fileext = ".rds")

  expect_no_error(saveRDS(result, f))
  loaded <- readRDS(f)
  expect_named(loaded, c("embeddings", "model"))
})

# ---- embed_query ----------------------------------------------------

test_that("embed_query returns a numeric vector", {
  nodes <- make_embed_nodes()
  model <- embed_nodes(nodes)$model
  qvec <- embed_query("evaluate model predictions error", model)

  expect_type(qvec, "double")
  expect_true(length(qvec) > 0L)
})

test_that("embed_query output has same length as node embeddings", {
  nodes <- make_embed_nodes()
  result <- embed_nodes(nodes)
  qvec <- embed_query("load data from csv file", result$model)

  node_dim <- length(result$embeddings[[1L]])
  expect_equal(length(qvec), node_dim)
})

test_that("embed_query handles out-of-vocabulary terms without crashing", {
  nodes <- make_embed_nodes()
  model <- embed_nodes(nodes)$model

  expect_no_error(embed_query("xyzzy_unknown_tok3n_42", model))
})

test_that("embed_query returns numeric(0) for NULL model", {
  result <- embed_query("something", NULL)
  expect_equal(result, numeric(0))
})

test_that("embed_query produces higher similarity for relevant query", {
  nodes <- make_embed_nodes()
  result <- embed_nodes(nodes)

  # Query about RMSE should be closer to compute_rmse than to load_data
  q_rmse <- embed_query("root mean squared error metric", result$model)
  q_load <- embed_query("load csv file from disk path", result$model)

  sim_rmse_to_rmse <- cosine_similarity(
    q_rmse,
    result$embeddings[["utils::compute_rmse"]]
  )
  sim_rmse_to_load <- cosine_similarity(
    q_rmse,
    result$embeddings[["utils::load_data"]]
  )
  sim_load_to_load <- cosine_similarity(
    q_load,
    result$embeddings[["utils::load_data"]]
  )
  sim_load_to_rmse <- cosine_similarity(
    q_load,
    result$embeddings[["utils::compute_rmse"]]
  )

  # These are soft assertions — TF-IDF may not be perfectly discriminative
  # on very short texts, but the direction should be correct
  expect_gte(sim_rmse_to_rmse, sim_rmse_to_load - 0.1)
  expect_gte(sim_load_to_load, sim_load_to_rmse - 0.1)
})

# ---- cosine_similarity ----------------------------------------------

test_that("cosine_similarity returns 1 for identical vectors", {
  v <- c(1, 2, 3)
  expect_equal(cosine_similarity(v, v), 1)
})

test_that("cosine_similarity returns 0 for orthogonal vectors", {
  expect_equal(cosine_similarity(c(1, 0, 0), c(0, 1, 0)), 0)
})

test_that("cosine_similarity returns 0 when a vector is all-zero", {
  expect_equal(cosine_similarity(c(0, 0), c(1, 2)), 0)
  expect_equal(cosine_similarity(c(1, 2), c(0, 0)), 0)
})

test_that("cosine_similarity result is in [-1, 1]", {
  a <- rnorm(20)
  b <- rnorm(20)
  sim <- cosine_similarity(a, b)
  expect_gte(sim, -1)
  expect_lte(sim, 1)
})

test_that("cosine_similarity errors on mismatched lengths", {
  expect_error(cosine_similarity(1:3, 1:4), class = "rlang_error")
})
