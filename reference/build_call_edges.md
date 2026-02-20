# Build CALLS edges between function nodes

Cross-references each function node's `calls_list` against the set of
known user-defined node identifiers. Each matched call produces one
directed edge `(from_node_id -> to_node_id)`.

## Usage

``` r
build_call_edges(func_nodes)
```

## Arguments

- func_nodes:

  A list of node records as returned by
  [`extract_function_nodes()`](https://davidrsch.github.io/rrlmgraph/reference/extract_function_nodes.md).

## Value

A `data.frame` with columns:

- `from`:

  Character: `node_id` of the calling function.

- `to`:

  Character: `node_id` of the called function.

- `weight`:

  Numeric: edge weight (default `1`).

Zero rows if no intra-project calls are found.

## Details

The matching strategy is:

1.  Exact match on bare function name (`to_name`).

2.  Qualified match: if `calls_list` contains `pkg::fn` or `pkg:::fn`,
    the bare `fn` part is matched against known node names.

Only edges between nodes present in `func_nodes` are returned
(intra-project edges). Calls to external packages are intentionally
excluded here; use
[`build_import_edges()`](https://davidrsch.github.io/rrlmgraph/reference/build_import_edges.md)
for package-level dependencies.

## See also

[`build_import_edges()`](https://davidrsch.github.io/rrlmgraph/reference/build_import_edges.md),
[`build_test_edges()`](https://davidrsch.github.io/rrlmgraph/reference/build_test_edges.md)

## Examples

``` r
if (FALSE) { # \dontrun{
proj  <- detect_rproject("/path/to/mypkg")
nodes <- extract_function_nodes(proj$r_files)
edges <- build_call_edges(nodes)
} # }
```
