# Check whether the graph cache is stale

Compares the modification time of the cached `graph.rds` against the
modification times of all `.R` files under `project_path`. Returns
`TRUE` if any R file is newer than the cache.

## Usage

``` r
is_cache_stale(project_path = ".")
```

## Arguments

- project_path:

  Character(1). Root directory of the R project.

## Value

Logical(1): `TRUE` if the cache is stale or absent.

## See also

[`save_graph_cache()`](https://davidrsch.github.io/rrlmgraph/reference/save_graph_cache.md),
[`load_graph_cache()`](https://davidrsch.github.io/rrlmgraph/reference/load_graph_cache.md)

## Examples

``` r
if (FALSE) { # \dontrun{
if (is_cache_stale("mypkg")) {
  g <- build_rrlm_graph("mypkg")
  save_graph_cache(g)
}
} # }
```
