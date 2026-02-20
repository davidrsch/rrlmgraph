# Plot an rrlm_graph

Renders the top-`n_hubs` function nodes (by PageRank) using
[`igraph::plot.igraph()`](https://r.igraph.org/reference/plot.igraph.html).
Package and test-file nodes are included only when they are connected to
the selected hubs; their labels are suppressed to avoid clutter. Vertex
size scales with relative PageRank importance.

## Usage

``` r
# S3 method for class 'rrlm_graph'
plot(
  x,
  n_hubs = 15L,
  layout = NULL,
  vertex.label.cex = 0.75,
  edge.arrow.size = 0.3,
  ...
)
```

## Arguments

- x:

  An `rrlm_graph` object.

- n_hubs:

  Integer(1). Number of top-PageRank *function* hub nodes to show.
  Default `15`.

- layout:

  Function. igraph layout function. Defaults to
  [igraph::layout_nicely](https://r.igraph.org/reference/layout_nicely.html)
  when `NULL` (auto-selects a stable algorithm for the graph topology,
  avoiding degenerate coordinates).

- vertex.label.cex:

  Numeric(1). Label size for function nodes. Default `0.75`.

- edge.arrow.size:

  Numeric(1). Arrow size. Default `0.3`.

- ...:

  Additional arguments forwarded to
  [`igraph::plot.igraph()`](https://r.igraph.org/reference/plot.igraph.html).

## Value

`x` invisibly.

## Details

Node colours:

- User functions – `"steelblue"`

- Package nodes – `"#C8D8E8"` (pale blue, smaller)

- Test files – `"seagreen3"`

- Other/unknown – `"lightyellow"`

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
