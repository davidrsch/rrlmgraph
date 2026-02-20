# Build IMPORTS edges from R files to package dependencies

Discovers package dependencies at two levels:

## Usage

``` r
build_import_edges(r_files, root = NULL)
```

## Arguments

- r_files:

  Character vector of absolute paths to `.R` source files.

- root:

  Optional character(1). Project root directory. When provided, a
  `DESCRIPTION` file is searched there.

## Value

A `data.frame` with columns:

- `from`:

  File stem (or project identifier) where the import appears.

- `to`:

  Package name being imported.

- `weight`:

  Numeric: edge weight (default `1`).

- `source`:

  Character: `"library"`, `"qualified"`, or `"description"`.

## Details

1.  **Explicit** – [`library(pkg)`](https://rdrr.io/r/base/library.html)
    and [`require(pkg)`](https://rdrr.io/r/base/library.html) calls in
    source files.

2.  **Qualified** – `pkg::fn` and `pkg:::fn` patterns (the package name
    is extracted as an import).

3.  **DESCRIPTION** – if a `DESCRIPTION` file is found in the project
    root (or any ancestor up to two levels up), its `Imports:` and
    `Depends:` fields are parsed.

Each discovered dependency produces an edge from the *file stem* (or the
project root stem) to the package name. Base-R packages (`base`,
`methods`, `utils`, `stats`, `datasets`, `graphics`, `grDevices`) are
included when explicitly referenced but are labelled with
`source = "qualified"` or `source = "description"` for downstream
filtering.

## See also

[`build_call_edges()`](https://davidrsch.github.io/rrlmgraph/reference/build_call_edges.md),
[`build_test_edges()`](https://davidrsch.github.io/rrlmgraph/reference/build_test_edges.md)

## Examples

``` r
if (FALSE) { # \dontrun{
proj  <- detect_rproject("/path/to/mypkg")
edges <- build_import_edges(proj$r_files, root = proj$root)
} # }
```
