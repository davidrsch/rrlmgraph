# Detailed summary of an rrlm_graph

Prints node counts by type, edge counts by type, the top-5 nodes by
PageRank centrality, and selected graph metadata (embed method, cache
path, build time).

## Usage

``` r
# S3 method for class 'rrlm_graph'
summary(object, ...)
```

## Arguments

- object:

  An `rrlm_graph` object.

- ...:

  Ignored.

## Value

`object` invisibly.

## See also

[`print.rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/print.rrlm_graph.md),
[`plot.rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/plot.rrlm_graph.md),
[`build_rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/build_rrlm_graph.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- build_rrlm_graph("mypkg")
summary(g)
} # }
```
