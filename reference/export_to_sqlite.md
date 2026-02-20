# Export an rrlm_graph to a SQLite database

Writes nodes, edges, graph metadata, and task traces to a SQLite file
readable directly by better-sqlite3 in the TypeScript MCP server. All
operations are idempotent (upsert via `INSERT OR REPLACE`).

## Usage

``` r
export_to_sqlite(graph, db_path)
```

## Arguments

- graph:

  An `rrlm_graph` / `igraph` object.

- db_path:

  Character(1). Path to the `.sqlite` file to create or update.

## Value

`db_path`, invisibly.

## Schema

- `nodes`:

  node_id (PK), name, file, node_type, signature, body_text,
  roxygen_text, complexity, pagerank, task_weight, embedding (JSON),
  pkg_name, pkg_version

- `edges`:

  edge_id (PK AUTOINCREMENT), source_id, target_id, edge_type, weight,
  metadata (JSON)

- `task_traces`:

  trace_id (PK AUTOINCREMENT), query, nodes_json, polarity, session_id,
  created_at

- `graph_metadata`:

  key (PK), value

## See also

[`save_graph_cache()`](https://davidrsch.github.io/rrlmgraph/reference/save_graph_cache.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- build_rrlm_graph("mypkg")
export_to_sqlite(g, "mypkg.sqlite")
} # }
```
