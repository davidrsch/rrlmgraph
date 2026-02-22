# rrlmgraph

The goal of `rrlmgraph` is to give large language models the *right*
context about your R project — not the whole codebase. It builds a typed
knowledge graph that encodes functions, files, packages, and tests as
nodes, then performs a relevance-guided traversal to extract a
token-budgeted context window for every LLM query. Compared to pasting
entire source files, `rrlmgraph` reduces hallucinations by grounding
responses in verified code and cuts token consumption by orders of
magnitude.

## Installation

You can install the development version of `rrlmgraph` from GitHub with:

``` r
# install.packages("pak")
pak::pak("davidrsch/rrlmgraph")
```

## Example

### Example 1: Build a knowledge graph for your R project

[`build_rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/build_rrlm_graph.md)
parses every R source file in your project, extracts function
definitions, resolves cross-file calls and package imports, computes
TF-IDF embeddings for each node, and assembles everything into an
`igraph` object annotated with PageRank scores.

``` r
library(rrlmgraph)

# Point it at any R project — package, Shiny app, or script directory.
g <- build_rrlm_graph("path/to/mypkg")
#> ✔ Detected R package: mypkg
#> ✔ Parsed 14 source files — 38 function nodes, 6 package nodes, 9 test files
#> ✔ Built CALLS (52), IMPORTS (31), TESTS (9), SEMANTIC (17) edges
#> ✔ TF-IDF embeddings computed (vocab size: 1 204)
#> ✔ PageRank computed
#> ✔ Graph cached at path/to/mypkg/.rrlmgraph/

# The object is a plain igraph, so all igraph tools work.
igraph::vcount(g)
#> [1] 53
igraph::ecount(g)
#> [1] 109

# A typed summary is available via print/summary.
g
#> <rrlm_graph>  mypkg 0.1.0
#>   Nodes : 53  (38 function, 6 package, 9 testfile)
#>   Edges : 109 (52 CALLS, 31 IMPORTS, 9 TESTS, 17 SEMANTIC)
#>   Embed : tfidf  (vocab 1 204)
#>   Cached: path/to/mypkg/.rrlmgraph/graph.rds
```

### Example 2: Query context for a coding task

[`query_context()`](https://davidrsch.github.io/rrlmgraph/reference/query_context.md)
performs a relevance-guided breadth-first search from the most
informative seed node. The token budget is a **hard constraint** — the
returned context string is guaranteed to fit within it.

``` r
# Ask for the most relevant code for a specific task.
ctx <- query_context(
  g,
  query        = "How does the model fitting pipeline handle missing values?",
  budget_tokens = 2000L
)

# ctx is a list with class "rrlm_context".
ctx$seed_node
#> [1] "preprocess_data"

ctx$nodes
#> [1] "preprocess_data" "impute_missing"  "fit_model"       "validate_inputs"
#> [5] "load_data"

ctx$tokens_used
#> [1] 1847

# The assembled context string is ready to paste into any LLM prompt.
cat(ctx$context_string)
#> === preprocess_data (function | data_pipeline.R) ===
#> Signature: preprocess_data(df, strategy = c("median", "knn", "drop"))
#> Calls: impute_missing, validate_inputs
#> Called by: fit_model, run_cv_fold
#>
#> Prepares a raw data frame for modelling. Missing values are handled
#> according to `strategy`. Runs `validate_inputs()` first.
#>
#> preprocess_data <- function(df, strategy = c("median", "knn", "drop")) {
#>   strategy <- match.arg(strategy)
#>   df <- validate_inputs(df)
#>   impute_missing(df, strategy = strategy)
#> }
#> ...
```

You can inspect the relevance scores to understand why each node was
selected:

``` r
sort(ctx$relevance_scores, decreasing = TRUE)
#>  preprocess_data   impute_missing        fit_model validate_inputs
#>        0.8341           0.7129            0.6803            0.5512
#>    load_data
#>        0.3204
```

### Example 3: Chat with an LLM grounded in project code

[`chat_with_context()`](https://davidrsch.github.io/rrlmgraph/reference/chat_with_context.md)
wraps
[`query_context()`](https://davidrsch.github.io/rrlmgraph/reference/query_context.md)
and sends a grounded prompt to an LLM via the `ellmer` package. The
model is instructed to answer only from the provided code snippets and
cite node names. Supports OpenAI, Anthropic, GitHub Models, and Ollama.

``` r
# GitHub Models — uses GITHUB_PAT, no extra secret needed.
answer <- chat_with_context(
  g,
  message       = "Refactor preprocess_data() to support a 'mode' imputation strategy.",
  provider      = "github",
  budget_tokens = 3000L
)
cat(answer)
#> Based on `preprocess_data` (data_pipeline.R) and `impute_missing`
#> (impute.R), here is a minimal refactor:
#>
#> ```r
#> preprocess_data <- function(df,
#>                              strategy = c("median", "knn", "drop", "mode")) {
#>   strategy <- match.arg(strategy)
#>   df <- validate_inputs(df)
#>   impute_missing(df, strategy = strategy)
#> }
#> ```
#>
#> `impute_missing()` already dispatches on `strategy` via a switch statement
#> (impute.R line 34), so adding `"mode"` there is the only other change needed.

# Ollama (local, no API key required).
# answer <- chat_with_context(g, "...", provider = "ollama", model = "llama3.2")
```

### Example 4: Export to SQLite for the rrlmgraph-mcp server

[`export_to_sqlite()`](https://davidrsch.github.io/rrlmgraph/reference/export_to_sqlite.md)
writes the full graph — nodes with body text, embeddings, and PageRank,
edges with types and weights, and the TF-IDF vocabulary — to a single
SQLite file that the
[rrlmgraph-mcp](https://github.com/davidrsch/rrlmgraph-mcp) TypeScript
server reads directly.

``` r
# Export once after building (or rebuilding) the graph.
export_to_sqlite(g, db_path = "path/to/mypkg/.rrlmgraph/graph.sqlite")
#> ✔ Graph exported to path/to/mypkg/.rrlmgraph/graph.sqlite

# Then point the MCP server at the file.
# In ~/.cursor/mcp.json (or equivalent):
# {
#   "mcpServers": {
#     "rrlmgraph": {
#       "command": "node",
#       "args": [
#         "/path/to/rrlmgraph-mcp/dist/index.js",
#         "--db-path", "path/to/mypkg/.rrlmgraph/graph.sqlite"
#       ]
#     }
#   }
# }
```

The SQLite file is self-contained and can be committed alongside your
project so teammates benefit from graph context without running R.

## Learn more

- [Getting
  Started](https://davidrsch.github.io/rrlmgraph/articles/getting-started.md)
  — end-to-end walkthrough
- [Reference](https://davidrsch.github.io/rrlmgraph/reference/index.md)
  — full function documentation
- [rrlmgraph-mcp](https://github.com/davidrsch/rrlmgraph-mcp) — MCP
  server for Cursor, Claude Desktop, and other MCP hosts
