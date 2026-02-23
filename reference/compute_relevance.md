# Compute composite relevance score for a graph node

Calculate a composite relevance score for `node` given a query embedding
and the traversal state. The score is a weighted linear combination of
four signals:

## Usage

``` r
compute_relevance(
  node,
  query_vec,
  visited = character(0),
  graph,
  weights = getOption("rrlmgraph.weights")
)
```

## Arguments

- node:

  Character(1). Name of the vertex (i.e., `node_id`) to score.

- query_vec:

  Numeric vector. Dense embedding of the user query (from
  [`embed_query()`](https://davidrsch.github.io/rrlmgraph/reference/embed_query.md)).
  Pass `numeric(0)` to disable the semantic signal.

- visited:

  Character vector. Node names already visited in the current traversal
  session. Used to compute the co-change signal. Defaults to
  `character(0)`.

- graph:

  An `rrlm_graph` / `igraph` object.

- weights:

  Named list or `NULL`. If non-`NULL`, overrides the corresponding
  default weight(s). Recognised names: `semantic`, `pagerank`,
  `task_trace`, `cochange`. Falls back to
  `getOption("rrlmgraph.weights")`.

## Value

Numeric(1) in \\\[0, 1\]\\.

## Details

\$\$ \text{relevance} = 0.40 \cdot \text{sem\\sim} + 0.25 \cdot
\text{pagerank} + 0.25 \cdot \text{task\\trace\\weight} + 0.10 \cdot
\text{cochange\\score} \$\$

The weights can be overridden globally via
`options(rrlmgraph.weights = list(semantic=, pagerank=, task_trace=, cochange=))`.

## Signal definitions

- sem_sim:

  Cosine similarity between the node's TF-IDF (or other) embedding and
  `query_vec`. Clamped to \\\[0, 1\]\\.

- pagerank:

  Pre-computed `pagerank` vertex attribute, min-max normalised across
  the full graph to \\\[0, 1\]\\.

- task_trace_weight:

  Vertex attribute set by
  [`update_task_weights()`](https://davidrsch.github.io/rrlmgraph/reference/update_task_weights.md)
  (issue \#13). Defaults to \\0.5\\ (neutral) when the attribute is
  absent or `NA`.

- cochange_score:

  Mean weight of `CO_CHANGES` edges connecting this node to
  already-visited nodes. Zero when `visited` is empty or no such edges
  exist.

## See also

[`build_rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/build_rrlm_graph.md),
[`embed_query()`](https://davidrsch.github.io/rrlmgraph/reference/embed_query.md),
[`cosine_similarity()`](https://davidrsch.github.io/rrlmgraph/reference/cosine_similarity.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g   <- build_rrlm_graph("mypkg")
emb <- embed_nodes(extract_function_nodes(detect_rproject("mypkg")$r_files))
q   <- embed_query("load training data", emb$model)
score <- compute_relevance("data::load_data", q, graph = g)
} # }
```
