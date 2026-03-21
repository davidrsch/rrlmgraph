# Read task-trace rows from a SQLite `task_traces` table.

Returns a data.frame with columns: `query`, `nodes_json`, `polarity`,
`session_id`, `created_at`. Returns `NULL` silently on any failure.

## Usage

``` r
.read_traces_sqlite(sqlite_path)
```
