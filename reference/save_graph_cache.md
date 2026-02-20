# Save an rrlm_graph to a local cache directory

Serialises the graph to `.rrlmgraph/graph.rds` and writes a
human-readable `.rrlmgraph/config.yml` alongside a
`.rrlmgraph/.gitignore` that keeps the RDS out of version control while
tracking `config.yml` and `task_trace.jsonl`.

## Usage

``` r
save_graph_cache(graph, cache_dir = NULL)
```

## Arguments

- graph:

  An `rrlm_graph` / `igraph` object.

- cache_dir:

  Character(1) or `NULL`. Path to the cache directory. Defaults to
  `.rrlmgraph/` inside the `"project_root"` graph attribute, or the
  current working directory when the attribute is absent.

## Value

`cache_dir`, invisibly.

## See also

[`load_graph_cache()`](https://davidrsch.github.io/rrlmgraph/reference/load_graph_cache.md),
[`is_cache_stale()`](https://davidrsch.github.io/rrlmgraph/reference/is_cache_stale.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- build_rrlm_graph("mypkg")
save_graph_cache(g)
} # }
```
