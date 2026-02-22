# Export an rrlm_graph to a SQLite database

Writes nodes, edges, graph metadata, task traces, and the TF-IDF
vocabulary to a SQLite file readable directly by better-sqlite3 in the
TypeScript MCP server. All operations are idempotent:
nodes/edges/metadata are replaced on each call; task-trace rows are
inserted with `INSERT OR IGNORE` (deduplication on `query`,
`session_id`, `created_at`).

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
  roxygen_text, complexity, pagerank, task_weight, embedding (JSON text
  array of floats), pkg_name, pkg_version

- `edges`:

  edge_id (PK AUTOINCREMENT), source_id, target_id, edge_type, weight,
  metadata (JSON)

- `task_traces`:

  trace_id (PK AUTOINCREMENT), query, nodes_json, polarity, session_id,
  created_at. Unique on (query, session_id, created_at) to prevent
  duplicate imports.

- `tfidf_vocab`:

  term (PK), idf, doc_count, term_count. Only populated when
  `embed_method = "tfidf"`. Allows the TypeScript MCP server to encode
  queries in the same vector space without calling back into R.

- `graph_metadata`:

  key (PK), value

## Embedding format

The `embedding` column in `nodes` is stored as a JSON text array of
floating-point numbers, e.g.\\ `[0.12, -0.34, ...]`. In TypeScript
(better-sqlite3):

    const emb: number[] = JSON.parse(row.embedding ?? "[]");

## See also

[`save_graph_cache()`](https://davidrsch.github.io/rrlmgraph/reference/save_graph_cache.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- build_rrlm_graph("mypkg")
export_to_sqlite(g, "mypkg.sqlite")
} # }
```
