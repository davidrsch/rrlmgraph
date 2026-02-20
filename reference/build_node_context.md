# Build a text representation of a single graph node

Returns a character string representing `node_name`. Two modes are
supported:

## Usage

``` r
build_node_context(node_name, graph, mode = c("auto", "full", "compressed"))
```

## Arguments

- node_name:

  Character(1). The `node_id` of the vertex (e.g.\\
  `"utils::load_data"`).

- graph:

  An `rrlm_graph` / `igraph` object.

- mode:

  Character(1): `"auto"` (default), `"full"`, or `"compressed"`. In
  `"auto"` the function returns full source; the caller should pass
  `"compressed"` explicitly for non-seed nodes.

## Value

Character(1). The text context for the node. Empty string if the node is
not found.

## Details

- **full** (seed / primary nodes):

  Full function signature, Roxygen comment block (if present), and body
  text. Used for the core nodes that the LLM must understand in detail.

- **compressed** (supporting nodes / package nodes):

  A compact representation consisting of the signature, a one-line
  description, a *Calls* list, and a *Called by* list. Targets a
  compression ratio \\\geq 3\times\\ vs.\\ the full source (measured in
  approximate token units).

Package nodes (type `"package"`) always use compressed mode, regardless
of the `mode` argument.

## See also

[`assemble_context_string()`](https://davidrsch.github.io/rrlmgraph/reference/assemble_context_string.md),
[`build_rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/build_rrlm_graph.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g   <- build_rrlm_graph("mypkg")
cat(build_node_context("utils::load_data", g, mode = "full"))
cat(build_node_context("utils::clean_data", g, mode = "compressed"))
} # }
```
