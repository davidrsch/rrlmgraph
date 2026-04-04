# Getting Started with rrlmgraph

`rrlmgraph` builds a typed knowledge graph of an R project and uses it
to supply large language models (LLMs) with precise, token-budgeted
context. This vignette walks through the full workflow on a small
self-contained example.

## Installation

``` r
install.packages("rrlmgraph")

# For optional LLM chat integration:
install.packages("ellmer")
```

## 1. Building your first graph

[`build_rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/dev/reference/build_rrlm_graph.md)
accepts a path to any R project – a package, a scripts folder, or a
Shiny app – and returns an `rrlm_graph` object.

``` r
library(rrlmgraph)

# Build the graph from the bundled demo project -- eight real functions with
# genuine CALLS relationships guarantees a non-trivial graph in every
# environment without needing any external files.
graph <- build_rrlm_graph(demo_dir, verbose = TRUE)
#> Detecting project at /home/runner/work/_temp/Library/rrlmgraph/extdata/demo
#> Parsing 3 R file(s)
#> Warning: <anonymous>: ... may be used in an incorrect context: 'fit_model(raw, ...)' (/home/runner/work/_temp/Library/rrlmgraph/extdata/demo/predict.R:51)
#> Building CALLS edges
#> Building IMPORT edges
#> Building TEST edges
#> Building CO_CHANGES edges from git history
#> Warning: [rrlmgraph] CO_CHANGE edges skipped: git is unavailable or
#> '/home/runner/work/_temp/Library/rrlmgraph/extdata/demo' is not inside a git
#> repository. Graphs built without git will have no CO_CHANGES signal. Relevance
#> scores from this graph will differ from those built in an environment where git
#> history is accessible.
#> Building DISPATCHES_ON / EXTENDS edges
#> Assembling igraph
#> Computing PageRank
#> Detecting entry points for project type 'script'
#> Embedding nodes with method 'tfidf'
#> Computing semantic similarity edges (threshold 0.7)
#> Done in 0.61s -- 27 nodes, 11 edges
```

The function:

1.  Detects the project type (package / scripts / Shiny app)
2.  Parses every `.R` file into function nodes via AST analysis
3.  Resolves CALLS, IMPORTS, and TEST edges between nodes
4.  Fits TF-IDF embeddings for semantic similarity

## 2. Inspecting the graph

``` r
summary(graph)
#> === rrlm_graph: demo ===
#> Root:  /home/runner/work/_temp/Library/rrlmgraph/extdata/demo
#> Built: 2026-04-04 21:33:00
#> Build time: 0.61 s
#> 
#> Nodes (27 total):
#>   package: 18
#>   function: 9
#> 
#> Edges (11 total):
#>   CALLS: 11
#> 
#> Top-5 nodes by PageRank:
#>   1. demo/data_prep::clean_data (0.11842)
#>   2. demo/data_prep::validate_inputs (0.051313)
#>   3. demo/data_prep::prepare_data (0.048062)
#>   4. demo/model::select_features (0.041498)
#>   5. demo/model::tune_hyperparams (0.041498)
#> 
#> Metadata:
#>   Embed method: tfidf
#>   Cache path:   /home/runner/work/_temp/Library/rrlmgraph/extdata/demo/.rrlmgraph/graph.rds
```

``` r
print(graph)
#> <rrlm_graph> demo | 27 nodes | 11 edges | embed: tfidf
```

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws a
force-directed layout coloured by node type:

- **steelblue** – user-defined functions (size scales with PageRank)
- **pale blue** – package / imported dependencies
- **seagreen** – test files
- Labels on function nodes only; arrows show CALLS direction

``` r
plot(graph)
```

## 3. Querying context

[`query_context()`](https://davidrsch.github.io/rrlmgraph/dev/reference/query_context.md)
performs relevance-guided BFS from the most relevant nodes and returns a
token-budgeted context window ideal for LLM prompts.

``` r
ctx <- query_context(
  graph,
  query = "How does the data preparation and validation pipeline work?",
  budget_tokens = 400L,
  verbose = TRUE
)
#> Seed node: "demo/predict::run_pipeline"
#> + "demo/data_prep::prepare_data" (score=0.201, tokens=32)
#> + "demo/data_prep::clean_data" (score=0.24, tokens=24)
#> + "demo/model::fit_model" (score=0.164, tokens=34)
#> + "demo/model::tune_hyperparams" (score=0.157, tokens=21)
#> + "demo/predict::predict_results" (score=0.144, tokens=24)
#> + "demo/predict::evaluate_predictions" (score=0.14, tokens=16)
#> + "demo/data_prep::validate_inputs" (score=0.138, tokens=16)
#> + "demo/model::select_features" (score=0.133, tokens=24)
#> Warning: Context truncated to 1397 chars to fit budget (400 tokens).

# Nodes selected for the context window
ctx$nodes
#> [1] "demo/predict::run_pipeline"         "demo/data_prep::prepare_data"      
#> [3] "demo/data_prep::clean_data"         "demo/model::fit_model"             
#> [5] "demo/model::tune_hyperparams"       "demo/predict::predict_results"     
#> [7] "demo/predict::evaluate_predictions" "demo/data_prep::validate_inputs"   
#> [9] "demo/model::select_features"

# Number of tokens used
ctx$tokens_used
#> [1] 400
```

The assembled context string – ready to paste into a system prompt:

``` r
cat(ctx$context_string)
#> # rrlm_graph Context
#> # Project: demo | R 4.5.3 | ~743 tokens
#> # Query: How does the data preparation and validation pipeline work?
#> 
#> ## CORE FUNCTIONS
#> ---
#> ### demo/predict::run_pipeline
#> #' Run the full modelling pipeline
#> #'
#> #' Convenience wrapper that prepares data, fits a model, generates
#> #' predictions, and evaluates them in a single call.
#> #'
#> #' @param raw A data.frame with columns \code{x} and \code{y}.
#> #' @param ... Additional arguments forwarded to \code{\link{fit_model}}.
#> #' @return A list with elements \code{model} (the fitted \code{lm}),
#> #'   \code{predictions} (numeric vector), and \code{metrics} (named numeric
#> #'   vector from \code{\link{evaluate_predictions}}).
#> #' @seealso \code{\link{prepare_data}}, \code{\link{fit_model}},
#> #'   \code{\link{predict_results}}, \code{\link{evaluate_predictions}}
#> #' @export
#> run_pipeline(raw, ...) {
#> run_pipeline <- function(raw, ...) {
#>   df <- prepare_data(raw)
#>   model <- fit_model(raw, ...)
#>   preds <- predict_results(model, df)
#>   metrics <- evaluate_predictions(preds, df$y)
#>   list(model = model, predictions = preds, metrics = metrics)
#> }
#> }
#> 
#> ## SUPPORTING FUNCTIONS
#> ---
#> ### demo/data_prep::prepare_data
#> prepare_data(raw, scale_x)
#> Prepare a data.frame for modelling
#> Calls: demo/data_prep::validate_inputs, demo/data_prep::clean_data
#> Called by: demo/model::fit_model, demo/predict::run_pipeline
#> 
#> ### demo/data_prep::clean_data
#> clean_data(raw)
```

## 4. Chatting with context (LLM required)

[`chat_with_context()`](https://davidrsch.github.io/rrlmgraph/dev/reference/chat_with_context.md)
assembles the context and sends it to an LLM. Supported providers via
the `ellmer` package:

``` r
# OpenAI (default) -- requires OPENAI_API_KEY
answer <- chat_with_context(
  graph,
  "How does prepare_data() ensure clean input?"
)
cat(answer)

# GitHub Models Marketplace -- requires GITHUB_PAT
answer <- chat_with_context(
  graph,
  "What does fit_model() return?",
  provider = "github",
  model    = "gpt-4o-mini"
)

# Local Ollama -- no API key needed
answer <- chat_with_context(
  graph,
  "Walk me through the prediction pipeline.",
  provider = "ollama",
  model    = "llama3.2"
)

# Anthropic Claude -- requires ANTHROPIC_API_KEY
answer <- chat_with_context(
  graph,
  "Which function calls prepare_data()?",
  provider = "anthropic"
)
```

Each call automatically logs the query, nodes used, and a response
excerpt to `.rrlmgraph/task_trace.jsonl` inside the project root. This
trace is used by
[`update_task_weights()`](https://davidrsch.github.io/rrlmgraph/dev/reference/update_task_weights.md)
to boost frequently-referenced nodes in future relevance scoring.

## 5. Incremental updates

After editing source files you do not need to rebuild the full graph.
[`update_graph_incremental()`](https://davidrsch.github.io/rrlmgraph/dev/reference/update_graph_incremental.md)
re-parses only the changed files:

``` r
# Simulate editing a file
writeLines(c(
  "#' Prepare data (updated)",
  "#'",
  "#' @param raw A data.frame of raw values.",
  "#' @param remove_na Logical. Remove rows with NAs. Default TRUE.",
  "#' @return A cleaned data.frame.",
  "#' @export",
  "prepare_data <- function(raw, remove_na = TRUE) {",
  "  if (remove_na) raw <- raw[complete.cases(raw), ]",
  "  raw",
  "}"
), file.path(proj_dir, "R", "data_prep.R"))

graph_small <- update_graph_incremental(
  graph_small,
  changed_files = file.path(proj_dir, "R", "data_prep.R"),
  verbose = TRUE
)
#> 
#> ── Incremental graph update ──
#> 
#> Changed files: /tmp/RtmpKQemTu/mypkg_demo/R/data_prep.R
#> Removing 1 stale node(s).
#> Re-parsing 1 file(s).
#> Embedding 1 new node(s) using method 'tfidf'.
#> Graph now has 2 nodes, 0 edges.
#> Recomputing PageRank.
#> Persisting cache to /tmp/RtmpKQemTu/mypkg_demo.
#> Graph cached at /tmp/RtmpKQemTu/mypkg_demo/.rrlmgraph

summary(graph_small)
#> IGRAPH 6dc1841 DNW- 2 0 -- 
#> + attr: project_name (g/c), project_root (g/c), project_type (g/c),
#> | r_version (g/c), build_time (g/n), build_at (g/c), embed_method
#> | (g/c), embed_model (g/x), cache_path (g/c), name (v/c), node_type
#> | (v/c), file (v/c), line_start (v/n), line_end (v/n), signature (v/c),
#> | body_text (v/c), roxygen_text (v/c), complexity (v/n), pagerank
#> | (v/n), scope_level (v/n), entry_point (v/l), api_depth (v/n),
#> | task_trace_weight (v/n), embedding (v/x), label (v/c), pkg (v/c), doc
#> | (v/c), weight (e/n), edge_type (e/c)
```

## 6. Caching the graph

For large projects, save the built graph to avoid re-parsing on every
session:

``` r
# Save to <project>/.rrlmgraph/graph.rds
save_graph_cache(graph)

# Restore in a later session
graph <- load_graph_cache(proj_dir)
```

## 7. Generating Copilot instructions

[`generate_instructions()`](https://davidrsch.github.io/rrlmgraph/dev/reference/generate_instructions.md)
writes a `.github/copilot-instructions.md` file that primes GitHub
Copilot with project-specific context derived from the graph:

``` r
generate_instructions(graph, max_tokens = 3000L)
```

The generated file describes the project structure, key functions, and
their call relationships, giving Copilot accurate context without manual
maintenance.

## 8. Using the MCP server

For integration with VS Code and other MCP-capable editors, see the
companion package **rrlmgraph-mcp**:
<https://github.com/davidrsch/rrlmgraph-mcp>

The MCP server exposes the SQLite-persisted graph (written by
[`export_to_sqlite()`](https://davidrsch.github.io/rrlmgraph/dev/reference/export_to_sqlite.md))
to any client that supports the Model Context Protocol, enabling
IDE-level context injection independent of the R console.
