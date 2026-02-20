# Log a task-trace entry to the JSONL feedback file

Appends a single JSONL entry to `.rrlmgraph/task_trace.jsonl` under the
project root. The file is created automatically. Each entry records the
query, the nodes that were surfaced, the feedback polarity, a timestamp,
and the current session identifier.

## Usage

``` r
log_task_trace(query, nodes, graph, polarity = 0)
```

## Arguments

- query:

  Character(1). The user query.

- nodes:

  Character vector. Node names that were part of the context returned.

- graph:

  An `rrlm_graph` / `igraph` object. Used to resolve the project root
  from the `"project_root"` graph attribute.

- polarity:

  Numeric(1) in \\\[-1, 1\]\\. Feedback signal: \\\> 0\\ means the
  context was helpful, \\\< 0\\ means it was not, \\0\\ (default) means
  neutral.

## Value

The path to `task_trace.jsonl`, invisibly.

## See also

[`update_task_weights()`](https://davidrsch.github.io/rrlmgraph/reference/update_task_weights.md),
[`update_task_polarity()`](https://davidrsch.github.io/rrlmgraph/reference/update_task_polarity.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- build_rrlm_graph("mypkg")
log_task_trace("How does load_data work?", c("pkg::load_data"), g)
} # }
```
