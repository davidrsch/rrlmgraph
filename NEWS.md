# rrlmgraph 0.1.0

First release.

### Improvements

* `plot.rrlm_graph()`: labels now placed **outside** nodes (`vertex.label.dist = 1.5`),
  colour changed to `"gray15"` (was white) for readability on all backgrounds,
  cex increased to `0.85`, default layout changed to Kamada-Kawai
  (`layout_with_kk`) for better node separation on sparse graphs (#29).
* Vignette figure size increased to 10 × 8 inches.

### Original first-release notes

* `R CMD check --as-cran`: zero errors, zero warnings verified across
  ubuntu/windows/macos x R release/oldrel/devel (#25).
* `DESCRIPTION`: added `BugReports` URL and `spelling` to `Suggests`.
* `inst/WORDLIST`: package-specific technical terms registered to pass
  `spelling::spell_check_package()` cleanly.
* CI: added spelling check step after `devtools::document()` (#25).

### Breaking changes

* `chat_with_context()`: the `model` parameter is now `NULL` by default (was
  `"gpt-4o-mini"`).  A per-provider sensible default is applied automatically.
  Code that passed `model` explicitly is unaffected.

### Improvements

* `chat_with_context()` gains a `provider` argument (`"openai"`, `"ollama"`,
  `"github"`, `"anthropic"`).  When `ellmer` is installed the appropriate
  `ellmer::chat_*()` backend is used; the `httr2` fallback is retained for
  `provider = "openai"` when `ellmer` is absent (#41).
* `ollama_available()` no longer requires the `ollamar` package.  It now pings
  the Ollama REST API directly via `httr2` (`GET /api/tags`).  Set
  `OLLAMA_BASE_URL` to override the default `http://localhost:11434` (#40).
* Ollama embedding calls (`embed_nodes()`, `embed_query()`) now hit
  `POST /api/embed` directly via `httr2`, removing the `ollamar` dependency
  entirely (#40).

### Bug fixes

* `R/parse_ast.R`: `as.character(expr[[1L]])` now guarded with an extra
  `[[1L]]` so that namespace-qualified calls (`pkg::fn`) no longer produce a
  length-3 character vector, which previously caused a "condition has length > 1"
  crash when building the graph.  Same guard added in `.collect_nse_symbols()`
  and `.find_qualified_calls()`.  R6Class detection changed to use `%in%` rather
  than `==` for robustness against multi-element vectors.
* `R/graph_build.R`: `build_test_edges()` now skips unresolvable bare names
  instead of throwing "subscript out of bounds" when a called symbol does not
  appear in the function-node index.
* `NAMESPACE`: restored `S3method()` registrations for `plot.rrlm_graph`,
  `print.rrlm_graph`, `summary.rrlm_graph`, `print.rrlm_context`, and
  `summary.rrlm_context`.  Previously the file listed plain `export()` entries,
  which broke S3 dispatch so igraph methods were called instead.  A new
  `R/imports.R` carrying all `@importFrom` roxygen tags ensures
  `devtools::document()` regenerates the full `importFrom()` block.
* `print.rrlm_graph()`, `summary.rrlm_graph()`: replaced `cli::cli_*()` calls
  with `cat()` so output is captured by `capture.output()` in tests and pipe
  operators work correctly.
* `inst/WORDLIST`: added `centres`, `cex`, `Kamada`, `Kawai` to silence
  `spelling::spell_check_package()` failures in CI.
* `tests/testthat/test-parse-ast.R`: fixed `write_r_file()` helper so the
  temporary file outlives the helper call (was deleted immediately by
  `withr::local_tempfile()`; now uses `tempfile()` +
  `withr::defer(..., envir = parent.frame())`).
* `tests/testthat/test-s3-methods.R`: replaced `igraph::V(g)$attr <- NULL`
  with `igraph::delete_vertex_attr()` / `igraph::delete_edge_attr()` to match
  the igraph 2.x API.

### New features

**Graph construction**

* `build_rrlm_graph()` -- build a typed knowledge graph for any R project.
* `update_graph_incremental()` -- re-parse only changed files for fast
  single-file updates.
* `extract_function_nodes()` -- AST-based function node extraction supporting
  standard assignments, S4 generics/methods, and R5/R6 classes.
* `find_calls_in_body()` -- enumerate global symbol references in a function
  body, filtering NSE false-positives.
* `build_call_edges()` -- intra-project CALLS edges.
* `build_import_edges()` -- IMPORTS edges from source files to package
  dependencies (library calls, qualified calls, and DESCRIPTION fields).
* `build_test_edges()` -- TEST edges from test files to user-defined functions.

**Querying and context assembly**

* `query_context()` -- relevance-guided BFS producing a token-budgeted context
  object for use with an LLM.
* `compute_relevance()` -- composite relevance score (semantic, PageRank,
  task-trace, co-change).
* `build_node_context()` -- full or compressed text representation of a single
  graph node.
* `assemble_context_string()` -- structured, LLM-ready prompt string from
  ranked node hits.

**Embedding**

* `embed_nodes()` -- TF-IDF (default), Ollama, or OpenAI dense embeddings.
* `embed_query()` -- project a free-text query into the model's vector space.
* `cosine_similarity()` -- cosine similarity between two numeric vectors.
* `ollama_available()` -- runtime check for a reachable Ollama daemon.

**LLM interface**

* `chat_with_context()` -- chat with an LLM using graph-derived context; uses
  `ellmer` when installed, falls back to `httr2`.

**Task learning**

* `log_task_trace()` -- append a JSONL feedback entry to
  `.rrlmgraph/task_trace.jsonl`.
* `update_task_weights()` -- apply exponential decay and accumulate per-node
  weights from the trace file.
* `update_task_polarity()` -- rewrite polarity values of recent trace entries.

**Cache and export**

* `save_graph_cache()` -- serialise graph to `.rrlmgraph/graph.rds`.
* `load_graph_cache()` -- restore graph from cache, refreshing task weights.
* `is_cache_stale()` -- compare cache mtime against R source files.
* `export_to_sqlite()` -- write nodes, edges, and traces to a SQLite database
  for the TypeScript MCP server.

**Project detection**

* `detect_rproject()` -- detect project type and discover all R source files.

**Instructions**

* `generate_instructions()` -- generate a Copilot-style instruction Markdown
  file from the project graph.

**S3 methods**

* `print.rrlm_graph()`, `summary.rrlm_graph()`, `plot.rrlm_graph()`
* `print.rrlm_context()`, `summary.rrlm_context()`
