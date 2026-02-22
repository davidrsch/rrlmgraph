# Build DISPATCHES_ON and EXTENDS edges for OOP patterns

Scans R source files for S4 generics/methods, R5 reference classes, and
R6 classes. Emits:

- `EXTENDS`:

  From child class node to parent, detected via `setClass(contains=)`,
  `R6Class(inherit=)`, and `setRefClass(contains=)`.

- `DISPATCHES_ON`:

  From a concrete S4 method node to the corresponding generic node when
  both appear in the parsed node set.

## Usage

``` r
build_dispatch_edges(func_nodes, r_files)
```

## Arguments

- func_nodes:

  A list of node records from
  [`extract_function_nodes()`](https://davidrsch.github.io/rrlmgraph/reference/extract_function_nodes.md).

- r_files:

  Character vector of absolute paths to `.R` source files.

## Value

A `data.frame` with columns `from`, `to`, `weight`, and `edge_type`.
Zero rows when no OOP patterns are detected.

## See also

[`build_call_edges()`](https://davidrsch.github.io/rrlmgraph/reference/build_call_edges.md),
[`build_rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/build_rrlm_graph.md)

## Examples

``` r
if (FALSE) { # \dontrun{
proj  <- detect_rproject("/path/to/mypkg")
nodes <- extract_function_nodes(proj$r_files)
edges <- build_dispatch_edges(nodes, proj$r_files)
} # }
```
