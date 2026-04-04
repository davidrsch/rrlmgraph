# Detect entry-point function nodes in an rrlm graph

Determines which function nodes represent the callable surface of an R
project. The strategy depends on the detected project type:

## Usage

``` r
detect_entry_points(graph, project)
```

## Arguments

- graph:

  An `rrlm_graph` / `igraph` object created by
  [`build_rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/dev/reference/build_rrlm_graph.md).

- project:

  The list returned by
  [`detect_rproject()`](https://davidrsch.github.io/rrlmgraph/dev/reference/detect_rproject.md).
  Must contain at least `project$type` and `project$root`.

## Value

Character vector of node names (`node_id` values) that are entry points.
May be empty only when the graph has no function nodes.

## Details

- `"package"`:

  Parses the `NAMESPACE` file for `export(...)` lines. Falls back to
  zero-CALLS-in-degree when `NAMESPACE` is absent or yields no matches.

- `"shiny"`:

  Zero CALLS in-degree from project-internal `CALLS` edges: the Shiny
  framework (not user code) invokes `server` / `ui` directly.

- `"script"`, `"quarto"`, `"rmarkdown"`:

  Same as Shiny: functions not called by any other project function are
  top-level entry points.

All candidates are first restricted to `scope_level == 0` nodes (i.e.
top-level function definitions). If detection yields no results for the
given type, the fallback is all `scope_level == 0` function nodes.

## See also

[`build_rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/dev/reference/build_rrlm_graph.md),
[`query_context()`](https://davidrsch.github.io/rrlmgraph/dev/reference/query_context.md)

## Examples

``` r
if (FALSE) { # \dontrun{
proj <- detect_rproject("/path/to/mypkg")
g    <- build_rrlm_graph("/path/to/mypkg")
eps  <- detect_entry_points(g, proj)
} # }
```
