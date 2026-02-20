# Update task-trace weights on graph vertices

Reads all entries from `.rrlmgraph/task_trace.jsonl`, applies
exponential decay with a 30-day half-life \\(w = 2^{-\Delta d / 30})\\,
and accumulates per-node weights as \\\sum_i w_i \cdot (1 +
\text{polarity}\_i)\\. The result is min-max-normalised to \\\[0, 1\]\\
and stored in the `task_trace_weight` vertex attribute.

## Usage

``` r
update_task_weights(
  graph,
  useful_nodes = NULL,
  alpha = 0.3,
  decay = 0.99,
  trace_file = NULL
)
```

## Arguments

- graph:

  An `rrlm_graph` / `igraph` object.

- useful_nodes:

  Character vector of node names that were helpful in the current
  traversal. Also accepts `NULL` or `character(0)` to skip the EMA
  boost.

- alpha:

  Numeric(1). EMA learning rate \\(0, 1)\\. Default `0.3`. Used only for
  the in-memory EMA boost; ignored when a JSONL file is present.

- decay:

  Numeric(1). Multiplicative decay applied to all other nodes in the EMA
  fallback path. Default `0.99`.

- trace_file:

  Character(1) or `NULL`. Explicit path to the JSONL file. `NULL`
  (default) infers the path from the `"project_root"` graph attribute.

## Value

The graph with updated `task_trace_weight` vertex attributes.

## Details

When no trace file exists (empty graph or first run), falls back to the
exponential moving-average (EMA) stub so that
[`query_context()`](https://davidrsch.github.io/rrlmgraph/reference/query_context.md)
can still boost `useful_nodes` in memory.

## See also

[`log_task_trace()`](https://davidrsch.github.io/rrlmgraph/reference/log_task_trace.md),
[`update_task_polarity()`](https://davidrsch.github.io/rrlmgraph/reference/update_task_polarity.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- build_rrlm_graph("mypkg")
g <- update_task_weights(g, useful_nodes = c("pkg::load_data"))
} # }
```
