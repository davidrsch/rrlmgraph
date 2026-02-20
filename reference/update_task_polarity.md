# Update the polarity of recent task-trace entries

Finds trace entries whose node sets overlap with `context$nodes` and
rewrites their `polarity` field. Only the most recent `n_recent`
matching entries are updated (default: 3).

## Usage

``` r
update_task_polarity(graph, context, polarity, n_recent = 3L)
```

## Arguments

- graph:

  An `rrlm_graph` / `igraph` object. Used to resolve the project root.

- context:

  An `rrlm_context` object (output of
  [`query_context()`](https://davidrsch.github.io/rrlmgraph/reference/query_context.md)).
  The `$nodes` field is used to match trace entries.

- polarity:

  Numeric(1) in \\\[-1, 1\]\\. New polarity value to apply.

- n_recent:

  Integer(1). Maximum number of recent matching entries to update.
  Default `3L`.

## Value

The graph, invisibly (no in-memory state change; the JSONL file is
rewritten).

## See also

[`log_task_trace()`](https://davidrsch.github.io/rrlmgraph/reference/log_task_trace.md),
[`update_task_weights()`](https://davidrsch.github.io/rrlmgraph/reference/update_task_weights.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g   <- build_rrlm_graph("mypkg")
ctx <- query_context(g, "parse CSV files")
# User says context was helpful:
update_task_polarity(g, ctx, polarity = 0.8)
} # }
```
