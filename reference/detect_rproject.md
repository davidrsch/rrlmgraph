# Detect an R project type and discover its files

Inspects a directory and returns a named list describing the project
type, all discovered R/Rmd/qmd source files, and (if applicable) a
parsed `DESCRIPTION`. Directories that are typically not user-authored
(`renv`, `packrat`, `.Rproj.user`, `node_modules`, `.git`) are excluded
from file discovery.

## Usage

``` r
detect_rproject(path = ".")
```

## Arguments

- path:

  `character(1)`. Path to the project root. Defaults to the current
  working directory.

## Value

A named list with elements:

- `type`:

  Character(1): one of `"package"`, `"shiny"`, `"quarto"`,
  `"rmarkdown"`, `"script"`.

- `r_files`:

  Character vector of absolute paths to `.R` files.

- `test_files`:

  Character vector of absolute paths to files under `tests/testthat/`.

- `rmd_files`:

  Character vector of absolute paths to `.Rmd` files.

- `qmd_files`:

  Character vector of absolute paths to `.qmd` files.

- `description`:

  Named list of DESCRIPTION fields (or `NULL`).

- `root`:

  Character(1): the absolute path of the project root.

## See also

[`extract_function_nodes()`](https://davidrsch.github.io/rrlmgraph/reference/extract_function_nodes.md),
[`build_rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/build_rrlm_graph.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Detect the type of a package on disk
detect_rproject("/path/to/mypackage")
} # }
```
