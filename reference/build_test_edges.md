# Build TEST edges from test files to user-defined functions

Parses each test file for references to user-defined function names,
using the same AST-walking approach used in
[`find_calls_in_body()`](https://davidrsch.github.io/rrlmgraph/reference/find_calls_in_body.md).
A `TEST` edge `(test_file_stem -> function_node_id)` is emitted when a
test file calls a function that exists as a node in `func_nodes`.

## Usage

``` r
build_test_edges(func_nodes, test_files)
```

## Arguments

- func_nodes:

  A list of function node records from
  [`extract_function_nodes()`](https://davidrsch.github.io/rrlmgraph/reference/extract_function_nodes.md).

- test_files:

  Character vector of absolute paths to test `.R` files.

## Value

A `data.frame` with columns:

- `from`:

  Test file stem, character.

- `to`:

  `node_id` of the tested function, character.

- `weight`:

  Numeric: edge weight (default `1`).

## Details

Test-helper symbols (`expect_*`, `test_that`, `describe`, `it`, `setup`,
`teardown`, `skip*`, `withr::*`) are excluded from matching to avoid
false edges to non-existent nodes.

## See also

[`build_call_edges()`](https://davidrsch.github.io/rrlmgraph/reference/build_call_edges.md),
[`build_import_edges()`](https://davidrsch.github.io/rrlmgraph/reference/build_import_edges.md)

## Examples

``` r
if (FALSE) { # \dontrun{
proj  <- detect_rproject("/path/to/mypkg")
nodes <- extract_function_nodes(proj$r_files)
edges <- build_test_edges(nodes, proj$test_files)
} # }
```
