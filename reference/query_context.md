# Query the rrlm graph and retrieve a token-budgeted context

Performs a relevance-guided breadth-first search starting from a *seed
node* and builds a context string that fits within `budget_tokens`. The
budget is a **hard constraint**: the function never returns more tokens
than requested.

## Usage

``` r
query_context(
  graph,
  query,
  seed_node = NULL,
  budget_tokens = 2000L,
  min_relevance = 0.1,
  max_nodes = 20L,
  method = NULL,
  verbose = FALSE
)
```

## Arguments

- graph:

  An `rrlm_graph` / `igraph` object created by
  [`build_rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/build_rrlm_graph.md).

- query:

  Character(1). User query string.

- seed_node:

  Character(1) or `NULL`. Name of the vertex to start traversal from.
  `NULL` (default) triggers automatic selection: the function-type node
  with the highest PageRank.

- budget_tokens:

  Integer(1). Hard token limit. Default `2000L`.

- min_relevance:

  Numeric(1). Minimum relevance score \\\[0, 1\]\\ for a node to be
  admitted. Default `0.1`.

- max_nodes:

  Integer(1). Maximum number of nodes (excluding the seed) to absorb.
  Default `20L`.

- method:

  Character(1) or `NULL`. Embedding method passed to
  [`embed_query()`](https://davidrsch.github.io/rrlmgraph/reference/embed_query.md).
  `NULL` (default) reads the `"embed_model"` graph attribute.

- verbose:

  Logical(1). Print progress messages via `cli_inform()`. Default
  `FALSE`.

## Value

A named list with class `c("rrlm_context", "list")`:

- `nodes`:

  Character vector of absorbed node names, relevance-ordered, seed
  first.

- `context_string`:

  Character(1) assembled by
  [`assemble_context_string()`](https://davidrsch.github.io/rrlmgraph/reference/assemble_context_string.md).

- `tokens_used`:

  Integer(1). Approximate token count of `context_string`.

- `budget_tokens`:

  Integer(1). The budget that was used.

- `seed_node`:

  Character(1). The seed node name.

- `relevance_scores`:

  Named numeric vector of relevance scores for every absorbed node (seed
  included).

## Algorithm

1.  Embed `query` with
    [`embed_query()`](https://davidrsch.github.io/rrlmgraph/reference/embed_query.md).

2.  Identify the seed node: if `seed_node` is `NULL`, select the
    function-type vertex with the highest pre-computed PageRank;
    otherwise validate and use the supplied name.

3.  Initialise `visited = {seed}` and `frontier = neighbours(seed)`.

4.  BFS loop while `tokens_used < budget_tokens` and `frontier` is
    non-empty:

    - Score every frontier node with
      [`compute_relevance()`](https://davidrsch.github.io/rrlmgraph/reference/compute_relevance.md).

    - Select the best-scoring node with score `>= min_relevance`.

    - Compute its token cost (`.count_tokens()`).

    - Accept the node only if adding it stays within the budget;
      otherwise skip and try the next-best.

    - Mark as visited; expand its neighbours into the frontier.

5.  Call
    [`update_task_weights()`](https://davidrsch.github.io/rrlmgraph/reference/update_task_weights.md)
    to update the learning trace.

6.  Assemble the final context string with
    [`assemble_context_string()`](https://davidrsch.github.io/rrlmgraph/reference/assemble_context_string.md).

## See also

[`compute_relevance()`](https://davidrsch.github.io/rrlmgraph/reference/compute_relevance.md),
[`build_node_context()`](https://davidrsch.github.io/rrlmgraph/reference/build_node_context.md),
[`assemble_context_string()`](https://davidrsch.github.io/rrlmgraph/reference/assemble_context_string.md),
[`update_task_weights()`](https://davidrsch.github.io/rrlmgraph/reference/update_task_weights.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g   <- build_rrlm_graph("mypkg")
ctx <- query_context(g, "load training data", budget_tokens = 1000L)
cat(ctx$context_string)
} # }
```
