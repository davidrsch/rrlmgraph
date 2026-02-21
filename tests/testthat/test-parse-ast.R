# ---- helpers --------------------------------------------------------

write_r_file <- function(content) {
  f <- tempfile(fileext = ".R")
  withr::defer(unlink(f), envir = parent.frame())
  writeLines(content, f)
  f
}

# ---- extract_function_nodes ----------------------------------------

test_that("extract_function_nodes handles empty input", {
  expect_equal(extract_function_nodes(character(0)), list())
})

test_that("finds a simple function assignment", {
  f <- write_r_file("my_fn <- function(x, y) x + y")
  nodes <- extract_function_nodes(f)

  expect_length(nodes, 1L)
  node <- nodes[[1L]]
  expect_equal(node$name, "my_fn")
  expect_equal(node$signature, "my_fn(x, y)")
  expect_true(grepl("::", node$node_id))
  expect_equal(node$line_start, 1L)
})

test_that("finds multiple functions in one file", {
  f <- write_r_file(c(
    "f1 <- function(a) a",
    "f2 <- function(b) b + 1",
    "f3 <- function(c, d) c * d"
  ))
  nodes <- extract_function_nodes(f)

  expect_length(nodes, 3L)
  names_found <- vapply(nodes, `[[`, character(1), "name")
  expect_setequal(names_found, c("f1", "f2", "f3"))
})

test_that("node_id format is file_stem::fn_name", {
  f <- write_r_file("calculate <- function(x) sqrt(x)")
  nodes <- extract_function_nodes(f)

  node_id <- nodes[[1L]]$node_id
  parts <- strsplit(node_id, "::")[[1L]]
  expect_length(parts, 2L)
  expect_equal(parts[[2L]], "calculate")
})

test_that("extracts roxygen comment block", {
  f <- write_r_file(c(
    "#' Compute the answer",
    "#' @param x numeric input",
    "#' @return numeric",
    "answer <- function(x) x * 42"
  ))
  nodes <- extract_function_nodes(f)

  expect_true(grepl("Compute the answer", nodes[[1L]]$roxygen_text))
})

test_that("complexity >=1 and increases with control flow", {
  simple <- write_r_file("simple <- function(x) x + 1")
  complex <- write_r_file(c(
    "complex_fn <- function(x, n) {",
    "  if (x > 0) {",
    "    for (i in seq_len(n)) {",
    "      x <- x + i",
    "    }",
    "  }",
    "  x",
    "}"
  ))

  nodes_s <- extract_function_nodes(simple)
  nodes_c <- extract_function_nodes(complex)

  expect_gte(nodes_s[[1L]]$complexity, 1L)
  expect_gt(nodes_c[[1L]]$complexity, nodes_s[[1L]]$complexity)
})

test_that("calls_list is populated from function body", {
  f <- write_r_file("my_fn <- function(x) sqrt(log(x))")
  nodes <- extract_function_nodes(f)

  expect_true(
    "sqrt" %in% nodes[[1L]]$calls_list || "log" %in% nodes[[1L]]$calls_list
  )
})

# ---- S3/S4/R6 function types ----------------------------------------

test_that("detects S3 generic and method", {
  f <- write_r_file(c(
    "my_generic <- function(x, ...) UseMethod('my_generic')",
    "my_generic.foo <- function(x, ...) x$value"
  ))
  nodes <- extract_function_nodes(f)

  names_found <- vapply(nodes, `[[`, character(1), "name")
  expect_true("my_generic" %in% names_found)
  expect_true("my_generic.foo" %in% names_found)
})

test_that("detects S4 setGeneric and setMethod without error", {
  f <- write_r_file(c(
    "setGeneric('area', function(shape, ...) standardGeneric('area'))",
    "setMethod('area', 'circle', function(shape, ...) pi * shape@r^2)"
  ))
  expect_no_error(extract_function_nodes(f))
})

test_that("detects R6 class public methods", {
  f <- write_r_file(c(
    "MyClass <- R6::R6Class('MyClass',",
    "  public = list(",
    "    initialize = function(x) {",
    "      self$x <- x",
    "    },",
    "    greet = function() {",
    "      cat('Hello', self$x)",
    "    }",
    "  )",
    ")"
  ))
  nodes <- extract_function_nodes(f)
  names_found <- vapply(nodes, `[[`, character(1), "name")

  # Should find at least the R6 class definition methods
  expect_true(length(names_found) > 0L)
})

test_that("gracefully handles parse errors without crashing", {
  f <- write_r_file("this is not valid R code <<< }")
  expect_no_error(extract_function_nodes(f))
  result <- extract_function_nodes(f)
  expect_equal(result, list())
})

test_that("gracefully handles non-existent file", {
  expect_warning(
    result <- extract_function_nodes("/non/existent/file.R"),
    regexp = NULL # any warning is acceptable
  )
  expect_equal(result, list())
})

# ---- find_calls_in_body --------------------------------------------

test_that("find_calls_in_body returns character(0) for non-call input", {
  expect_equal(find_calls_in_body(1L), character(0))
  expect_equal(find_calls_in_body(NULL), character(0))
})

test_that("identifies direct function calls", {
  fn <- function(x) sqrt(log(abs(x)))
  res <- find_calls_in_body(body(fn))

  expect_true("sqrt" %in% res)
  expect_true("log" %in% res)
  expect_true("abs" %in% res)
})

test_that("does NOT include NSE column names from dplyr verbs", {
  skip_if_not_installed("dplyr")
  fn <- function(df) {
    dplyr::mutate(df, new_col = old_col * 2)
  }
  res <- find_calls_in_body(body(fn))

  expect_false("old_col" %in% res)
  expect_false("new_col" %in% res)
})

test_that("handles pkg::fn notation and returns qualified name", {
  fn <- function(x) fs::path_abs(x)
  res <- find_calls_in_body(body(fn))

  expect_true(any(grepl("^fs::", res)))
})

test_that("pkg:::fn notation does not crash", {
  code <- parse(text = "function(x) base:::sum(x)")[[1L]][[3L]]
  expect_no_error(find_calls_in_body(code))
})

test_that("result is sorted and deduplicated", {
  fn <- function(x) sqrt(sqrt(sqrt(x)))
  res <- find_calls_in_body(body(fn))

  expect_equal(res, sort(unique(res)))
  expect_equal(sum(res == "sqrt"), 1L)
})

test_that(".data$ pattern does not produce false edge to 'data'", {
  skip_if_not_installed("dplyr")
  fn <- function(df) dplyr::mutate(df, z = .data$x + .data$y)
  res <- find_calls_in_body(body(fn))

  expect_false("x" %in% res)
  expect_false("y" %in% res)
})
