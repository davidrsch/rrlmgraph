# Incrementally update the graph for changed files

Re-parses only the files that have changed and merges the resulting
nodes and edges back into the existing graph, avoiding a full rebuild.
PageRank and embeddings are recomputed only for the affected portion of
the graph, making single-file updates substantially faster than
[`build_rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/build_rrlm_graph.md).

## Usage

``` r
update_graph_incremental(
  graph,
  changed_files,
  embed_method = NULL,
  verbose = FALSE
)
```

## Arguments

- graph:

  An `rrlm_graph` / `igraph` object previously built by
  [`build_rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/build_rrlm_graph.md).

- changed_files:

  Character vector of absolute file paths that have been added, edited,
  or deleted since the last full build.

- embed_method:

  Character(1) or `NULL`. Embedding method passed to
  [`embed_nodes()`](https://davidrsch.github.io/rrlmgraph/reference/embed_nodes.md).
  `NULL` (default) uses the method stored in
  `graph_attr(graph, "embed_method")`, falling back to `"tfidf"`.

- verbose:

  Logical(1). Emit progress messages via cli. Default `FALSE`.

## Value

The updated `rrlm_graph` object. The graph retains all original vertex /
edge attributes; only nodes from `changed_files` are touched.

## See also

[`build_rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/build_rrlm_graph.md),
[`build_call_edges()`](https://davidrsch.github.io/rrlmgraph/reference/build_call_edges.md),
[`embed_nodes()`](https://davidrsch.github.io/rrlmgraph/reference/embed_nodes.md),
[`save_graph_cache()`](https://davidrsch.github.io/rrlmgraph/reference/save_graph_cache.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- build_rrlm_graph("mypkg")
# After editing R/utils.R:
g <- update_graph_incremental(g, changed_files = "mypkg/R/utils.R")
} # }
```
