# Plot an rrlm_graph

Renders the top-`n_hubs` nodes (by PageRank) using
[`igraph::plot.igraph()`](https://r.igraph.org/reference/plot.igraph.html).
Node colour reflects the node type:

- User functions – `"steelblue"`

- Package nodes – `"grey70"`

- Test files – `"seagreen3"`

- Other/unknown – `"lightyellow"`

## Usage

``` r
# S3 method for class 'rrlm_graph'
plot(
  x,
  n_hubs = 30L,
  layout = igraph::layout_with_fr,
  vertex.size = 8,
  vertex.label.cex = 0.7,
  edge.arrow.size = 0.4,
  ...
)
```

## Arguments

- x:

  An `rrlm_graph` object.

- n_hubs:

  Integer(1). Number of top-PageRank hub nodes to include in the
  sub-graph. Default `30`.

- layout:

  Function. igraph layout function. Defaults to
  [igraph::layout_with_fr](https://r.igraph.org/reference/layout_with_fr.html).

- vertex.size:

  Numeric(1). Vertex size passed to
  [`igraph::plot.igraph()`](https://r.igraph.org/reference/plot.igraph.html).
  Default `8`.

- vertex.label.cex:

  Numeric(1). Label size. Default `0.7`.

- edge.arrow.size:

  Numeric(1). Arrow size. Default `0.4`.

- ...:

  Additional arguments forwarded to
  [`igraph::plot.igraph()`](https://r.igraph.org/reference/plot.igraph.html).

## Value

`x` invisibly.

## See also

[`print.rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/print.rrlm_graph.md),
[`summary.rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/summary.rrlm_graph.md),
[`build_rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/build_rrlm_graph.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- build_rrlm_graph("mypkg")
plot(g)
plot(g, n_hubs = 10L)
} # }
```
