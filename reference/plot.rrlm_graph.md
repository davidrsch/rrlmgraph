# Plot an rrlm_graph

Renders the top-`n_hubs` function nodes (by PageRank) and their one-hop
neighbours as an interactive **Graphviz** widget via
[`DiagrammeR::grViz()`](https://rich-iannone.github.io/DiagrammeR/reference/grViz.html).
Nodes are grouped into dashed sub-graph boxes by source file and
coloured by node type; node width scales with relative PageRank
importance. In an interactive session the widget appears in the RStudio
Viewer or a browser tab, supporting pan and zoom.

## Usage

``` r
# S3 method for class 'rrlm_graph'
plot(
  x,
  n_hubs = 15L,
  layout = c("dot", "neato", "fdp", "sfdp", "circo"),
  file = NULL,
  width = 1400L,
  height = 900L,
  ...
)
```

## Arguments

- x:

  An `rrlm_graph` object.

- n_hubs:

  Integer(1). Number of top-PageRank *function* hub nodes to show.
  Default `15L`.

- layout:

  Character(1). Graphviz layout engine passed to the DOT `layout`
  attribute. One of `"dot"` (hierarchical, default), `"neato"`, `"fdp"`,
  `"sfdp"`, or `"circo"`.

- file:

  Character(1) or `NULL`. Optional output file path.

  - `.html` – saved via
    [`htmlwidgets::saveWidget()`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html)
    (requires the **htmlwidgets** package).

  - `.png`, `.pdf`, `.svg` – rendered by
    [`webshot2::webshot()`](https://rstudio.github.io/webshot2/reference/webshot.html)
    after writing a temporary HTML file (requires both **htmlwidgets**
    and **webshot2**).

- width, height:

  Integer(1). Used **only** when `file` is a raster or vector path
  (`.png`, `.pdf`, `.svg`): sets the viewport size in pixels that
  [`webshot2::webshot()`](https://rstudio.github.io/webshot2/reference/webshot.html)
  renders. Has no effect on the interactive widget, which fills 100\\
  pan-and-zoom via viz.js. Defaults `1400L` x `900L`.

- ...:

  Ignored; kept for S3 dispatch compatibility.

## Value

When `file` is `NULL` (default), an `htmlwidget` from
[`DiagrammeR::grViz()`](https://rich-iannone.github.io/DiagrammeR/reference/grViz.html)
is returned visibly so it prints in the viewer. The widget fills 100\\
regardless of graph size. When `file` is supplied, `x` is returned
invisibly.

## Details

Node colours:

- User functions – `"#4682B4"` (steelblue)

- Package nodes – `"#C8D8E8"` (pale blue, smaller)

- Test files – `"#3CB371"` (seagreen3)

## See also

[`print.rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/print.rrlm_graph.md),
[`summary.rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/summary.rrlm_graph.md),
[`build_rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/build_rrlm_graph.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- build_rrlm_graph("mypkg")
plot(g)
plot(g, n_hubs = 10L, layout = "neato")
plot(g, file = "graph.html")
plot(g, file = "graph.png", width = 1600L, height = 1000L)
} # }
```
