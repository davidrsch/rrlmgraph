# Load an rrlm_graph from a local cache

Reads `.rrlmgraph/graph.rds` from `project_path`, validates
compatibility with the installed rrlmgraph version, refreshes task-trace
weights via
[`update_task_weights()`](https://davidrsch.github.io/rrlmgraph/reference/update_task_weights.md),
and returns the graph.

## Usage

``` r
load_graph_cache(project_path = ".")
```

## Arguments

- project_path:

  Character(1). Root directory of the R project (parent of
  `.rrlmgraph/`).

## Value

An `rrlm_graph` object, or `NULL` invisibly when no cache exists.

## See also

[`save_graph_cache()`](https://davidrsch.github.io/rrlmgraph/reference/save_graph_cache.md),
[`is_cache_stale()`](https://davidrsch.github.io/rrlmgraph/reference/is_cache_stale.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- load_graph_cache("mypkg")
} # }
```
