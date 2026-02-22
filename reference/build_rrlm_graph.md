# Build an RLM-Graph for an R project

The primary user-facing function. Orchestrates project detection, AST
parsing, edge construction, TF-IDF (or alternative) embedding, and
igraph assembly into a typed knowledge graph.

## Usage

``` r
build_rrlm_graph(
  project_path = ".",
  embed_method = "tfidf",
  include_package_nodes = TRUE,
  semantic_threshold = 0.7,
  max_semantic_edges = 5L,
  cache = TRUE,
  verbose = FALSE
)
```

## Arguments

- project_path:

  Character(1). Path to the R project root. Defaults to `"."`.

- embed_method:

  Character(1). Embedding back-end: `"tfidf"` (default), `"ollama"`, or
  `"openai"`. Only `"tfidf"` is available in Sprint 1.

- include_package_nodes:

  Logical(1). When `TRUE` (default), one node per unique external
  package is added to the graph.

- semantic_threshold:

  Numeric(1). Minimum cosine similarity for a `SEMANTIC` edge to be
  created. Default `0.7`.

- max_semantic_edges:

  Integer(1). Maximum SEMANTIC edges to create per node. Capping at a
  small number (default `5L`) prevents dense graphs on large projects.
  Semantic edges are disabled entirely when the graph has more than 300
  function nodes.

- cache:

  Logical(1). When `TRUE` (default), the graph is serialised to
  `<project_root>/.rrlmgraph/graph.rds`.

- verbose:

  Logical(1). When `TRUE`, progress messages are printed via
  [`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html).
  Default `FALSE`.

## Value

An object of class `c("rrlm_graph", "igraph")`.

## Details

The returned object inherits from `igraph` and carries the class
`"rrlm_graph"`. Graph-level metadata is stored as igraph graph
attributes (accessible with
[`igraph::graph_attr()`](https://r.igraph.org/reference/graph_attr.html)).

### Node types

|              |                                          |
|--------------|------------------------------------------|
| Type         | Description                              |
| `"function"` | User-defined R function                  |
| `"package"`  | External package dependency              |
| `"testfile"` | Test file that references user functions |

### Edge types

|                   |                                                                                                                                                                     |
|-------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Type              | Description                                                                                                                                                         |
| `"CALLS"`         | Intra-project function call                                                                                                                                         |
| `"IMPORTS"`       | File/project imports a package                                                                                                                                      |
| `"TESTS"`         | Test file covers a user function                                                                                                                                    |
| `"SEMANTIC"`      | Cosine similarity \>= `semantic_threshold`                                                                                                                          |
| `"CO_CHANGES"`    | Two functions co-edited in \>= `min_cochanges` commits (from [`build_co_change_edges()`](https://davidrsch.github.io/rrlmgraph/reference/build_co_change_edges.md)) |
| `"DISPATCHES_ON"` | S4/R5 generic dispatches on a class (from [`build_dispatch_edges()`](https://davidrsch.github.io/rrlmgraph/reference/build_dispatch_edges.md))                      |
| `"EXTENDS"`       | Class inherits from another class (from [`build_dispatch_edges()`](https://davidrsch.github.io/rrlmgraph/reference/build_dispatch_edges.md))                        |

## See also

[`summary.rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/summary.rrlm_graph.md),
[`print.rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/print.rrlm_graph.md),
[`plot.rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/plot.rrlm_graph.md),
[`detect_rproject()`](https://davidrsch.github.io/rrlmgraph/reference/detect_rproject.md),
[`extract_function_nodes()`](https://davidrsch.github.io/rrlmgraph/reference/extract_function_nodes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- build_rrlm_graph("/path/to/mypkg")
summary(g)
plot(g)
} # }
```
