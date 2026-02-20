# Generate a Copilot-style instruction file from the project graph

Extracts key information from the `rrlm_graph` and writes a Markdown
instruction document suitable for AI coding assistants (e.g.
`.github/copilot-instructions.md`). The output summarises the project
type, R version, top functions by PageRank, inferred coding conventions,
and installed package inventory.

## Usage

``` r
generate_instructions(graph, output_path = NULL, max_tokens = 2000L)
```

## Arguments

- graph:

  An `rrlm_graph` / `igraph` object.

- output_path:

  Character(1) or `NULL`. Destination file path. When `NULL` (default)
  the file is written to `.github/copilot-instructions.md` inside the
  project root stored in `graph_attr(graph, "project_root")`.

- max_tokens:

  Integer(1). Soft upper bound on output size in tokens (approximated as
  `nchar / 4`). Content is trimmed to stay under this limit. Default
  `2000L`.

## Value

The path to the written file, invisibly.

## See also

[`build_rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/build_rrlm_graph.md),
[`query_context()`](https://davidrsch.github.io/rrlmgraph/reference/query_context.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- build_rrlm_graph("mypkg")
generate_instructions(g)
} # }
```
