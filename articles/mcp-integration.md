# MCP Server Integration

The **rrlmgraph-mcp** companion package exposes the project knowledge
graph via the [Model Context Protocol](https://modelcontextprotocol.io/)
(MCP), enabling any MCP-capable editor or AI agent to query graph
context without running R code directly.

## Architecture

    ┌─────────────────┐        JSON-RPC / stdio        ┌─────────────────┐
    │   VS Code /     │ ◄────────────────────────────► │  rrlmgraph-mcp  │
    │  GitHub Copilot │                                 │  (Node.js MCP   │
    │   (MCP client)  │                                 │    server)      │
    └─────────────────┘                                 └────────┬────────┘
                                                                 │  SQLite
                                                        ┌────────▼────────┐
                                                        │  .rrlmgraph/    │
                                                        │  graph.sqlite   │
                                                        └─────────────────┘

The SQLite database is written by
[`export_to_sqlite()`](https://davidrsch.github.io/rrlmgraph/reference/export_to_sqlite.md)
in **rrlmgraph** and read at query time by the MCP server — no R process
needs to be running during IDE sessions.

## Installation

Install the MCP server globally via npm:

``` r
# In a terminal:
# npm install -g rrlmgraph-mcp
```

Or use `npx` for a one-off run:

``` r
# npx rrlmgraph-mcp --db-path path/to/.rrlmgraph/graph.sqlite
```

## Exporting the graph to SQLite

``` r
library(rrlmgraph)

graph <- build_rrlm_graph("path/to/your/project")
export_to_sqlite(graph, "path/to/your/project/.rrlmgraph/graph.sqlite")
```

## Configuring VS Code

Add the following to `.vscode/mcp.json` (or your user-level
`settings.json` under `github.copilot.chat.mcp.servers`):

``` json
{
  "servers": {
    "rrlmgraph": {
      "type": "stdio",
      "command": "npx",
      "args": [
        "rrlmgraph-mcp",
        "--db-path",
        "${workspaceFolder}/.rrlmgraph/graph.sqlite"
      ]
    }
  }
}
```

After saving, GitHub Copilot Chat will list **rrlmgraph** as an
available MCP tool. Ask it questions like:

> *“Which functions call `prepare_data()`?”* *“What context is relevant
> for the data validation pipeline?”*

## Available MCP tools

| Tool             | Description                                                         |
|------------------|---------------------------------------------------------------------|
| `query_context`  | Relevance-guided BFS context window for a coding task               |
| `get_node_info`  | Full metadata, callers, callees, and source for a named node        |
| `list_functions` | Functions ranked by PageRank, with optional file-path filter        |
| `find_callers`   | All functions that call a given function (incoming CALLS edges)     |
| `find_callees`   | All functions called by a given function (outgoing CALLS edges)     |
| `search_nodes`   | Full-text keyword search across node names, bodies, and docs        |
| `add_task_trace` | Record accepted / rejected task outcome to improve future retrieval |
| `rebuild_graph`  | Trigger an Rscript rebuild of the project graph in-process          |

## Keeping the database fresh

Re-run
[`export_to_sqlite()`](https://davidrsch.github.io/rrlmgraph/reference/export_to_sqlite.md)
after editing R source files, or use
[`update_graph_incremental()`](https://davidrsch.github.io/rrlmgraph/reference/update_graph_incremental.md)
followed by
[`export_to_sqlite()`](https://davidrsch.github.io/rrlmgraph/reference/export_to_sqlite.md)
for large projects:

``` r
graph <- update_graph_incremental(
  graph,
  changed_files = "R/my_function.R"
)
export_to_sqlite(graph, "path/to/your/project/.rrlmgraph/graph.sqlite")
```

A [targets](https://docs.ropensci.org/targets/) pipeline or a Git
pre-commit hook that runs this two-step command keeps the graph in sync
with every commit.

## Further reading

- [rrlmgraph-mcp source](https://github.com/davidrsch/rrlmgraph-mcp)
- [Model Context Protocol
  specification](https://modelcontextprotocol.io/specification)
- [VS Code MCP
  documentation](https://code.visualstudio.com/docs/copilot/chat/mcp-servers)
