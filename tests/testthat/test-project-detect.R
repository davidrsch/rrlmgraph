test_that("detect_rproject errors on non-existent path", {
  expect_error(
    detect_rproject(tempfile("no_such_dir")),
    class = "rlang_error"
  )
})

# ---- helpers: build fixture directories in temp space ---------------

make_package_fixture <- function() {
  root <- tempfile("rrlm_pkg_test_")
  dir.create(root, recursive = TRUE)
  writeLines(
    c("Package: mypkg", "Version: 0.1.0", "License: MIT"),
    file.path(root, "DESCRIPTION")
  )
  dir.create(file.path(root, "R"), showWarnings = FALSE)
  writeLines("f <- function(x) x + 1", file.path(root, "R", "fn.R"))
  dir.create(
    file.path(root, "tests", "testthat"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  writeLines(
    "test_that('x', expect_true(TRUE))",
    file.path(root, "tests", "testthat", "test-fn.R")
  )
  root
}

make_shiny_fixture <- function() {
  root <- tempfile("rrlm_shiny_test_")
  dir.create(root, recursive = TRUE)
  writeLines("shinyApp(ui, server)", file.path(root, "app.R"))
  root
}

make_quarto_fixture <- function() {
  root <- tempfile("rrlm_quarto_test_")
  dir.create(root, recursive = TRUE)
  writeLines("project:\n  type: default", file.path(root, "_quarto.yml"))
  root
}

make_rmd_fixture <- function() {
  root <- tempfile("rrlm_rmd_test_")
  dir.create(root, recursive = TRUE)
  writeLines(
    "---\ntitle: Test\n---\n```{r}\n1+1\n```",
    file.path(root, "report.Rmd")
  )
  root
}

make_script_fixture <- function() {
  tmpdir <- tempfile("rrlm_script_test_")
  dir.create(tmpdir, recursive = TRUE)
  writeLines("x <- 1 + 1", file.path(tmpdir, "analysis.R"))
  tmpdir
}

# ---- type detection -------------------------------------------------

test_that("detect_rproject returns 'package' for DESCRIPTION root", {
  root <- make_package_fixture()
  result <- detect_rproject(root)

  expect_equal(result$type, "package")
  expect_equal(result$root, as.character(fs::path_abs(root)))
  expect_true(!is.null(result$description))
  expect_equal(result$description[["Package"]], "mypkg")
})

test_that("detect_rproject returns 'shiny' for app.R root", {
  root <- make_shiny_fixture()
  result <- detect_rproject(root)

  expect_equal(result$type, "shiny")
})

test_that("detect_rproject returns 'quarto' for _quarto.yml root", {
  root <- make_quarto_fixture()
  result <- detect_rproject(root)

  expect_equal(result$type, "quarto")
})

test_that("detect_rproject returns 'rmarkdown' for Rmd root (no quarto)", {
  root <- make_rmd_fixture()
  result <- detect_rproject(root)

  expect_equal(result$type, "rmarkdown")
})

test_that("detect_rproject returns 'script' for plain R files", {
  root <- make_script_fixture()
  result <- detect_rproject(root)

  expect_equal(result$type, "script")
})

# ---- file discovery -------------------------------------------------

test_that("r_files and test_files are populated correctly", {
  root <- make_package_fixture()
  result <- detect_rproject(root)

  expect_true(any(grepl("fn\\.R$", result$r_files)))
  expect_true(any(grepl("test-fn\\.R$", result$test_files)))
  # test_files must be a subset of r_files
  expect_true(all(result$test_files %in% result$r_files))
})

test_that("rmd_files and qmd_files are empty when none exist", {
  root <- make_package_fixture()
  result <- detect_rproject(root)

  expect_equal(result$rmd_files, character(0))
  expect_equal(result$qmd_files, character(0))
})

# ---- exclusion of vendor directories --------------------------------

test_that("renv directory is excluded from r_files", {
  root <- withr::local_tempdir()
  writeLines("Package: mypkg\nVersion: 0.1.0", file.path(root, "DESCRIPTION"))
  dir.create(file.path(root, "R"), showWarnings = FALSE)
  writeLines("f <- function(x) x", file.path(root, "R", "fn.R"))
  dir.create(
    file.path(root, "renv", "library"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  writeLines(
    "g <- function(y) y",
    file.path(root, "renv", "library", "leaked.R")
  )

  result <- detect_rproject(root)

  expect_false(any(grepl("renv", result$r_files)))
})

test_that("node_modules directory is excluded from r_files", {
  root <- withr::local_tempdir()
  writeLines("Package: mypkg\nVersion: 0.1.0", file.path(root, "DESCRIPTION"))
  dir.create(
    file.path(root, "node_modules", "some_pkg"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  writeLines(
    "h <- function() 1",
    file.path(root, "node_modules", "some_pkg", "shim.R")
  )

  result <- detect_rproject(root)

  expect_false(any(grepl("node_modules", result$r_files)))
})

# ---- return types ---------------------------------------------------

test_that("all returned paths are absolute", {
  root <- make_package_fixture()
  result <- detect_rproject(root)

  check_absolute <- function(paths) {
    if (length(paths) == 0L) {
      return(invisible(NULL))
    }
    lapply(paths, function(p) {
      expect_true(grepl("^([A-Za-z]:|/)", p), info = paste("Not absolute:", p))
    })
  }

  check_absolute(result$r_files)
  check_absolute(result$test_files)
  check_absolute(result$rmd_files)
  check_absolute(result$qmd_files)
  expect_true(grepl("^([A-Za-z]:|/)", result$root))
})

test_that("detect_rproject returns all required list elements", {
  root <- make_package_fixture()
  result <- detect_rproject(root)

  expected_names <- c(
    "type",
    "r_files",
    "test_files",
    "rmd_files",
    "qmd_files",
    "description",
    "root"
  )
  expect_named(result, expected_names, ignore.order = TRUE)
})
