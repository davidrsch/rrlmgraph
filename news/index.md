# Changelog

## rrlmgraph 0.1.0

First release.

#### Improvements

- [`plot.rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/plot.rrlm_graph.md):
  labels now placed **outside** nodes (`vertex.label.dist = 1.5`),
  colour changed to `"gray15"` (was white) for readability on all
  backgrounds, cex increased to `0.85`, default layout changed to
  Kamada-Kawai (`layout_with_kk`) for better node separation on sparse
  graphs ([\#29](https://github.com/davidrsch/rrlmgraph/issues/29)).
- Vignette figure size increased to 10 × 8 inches.

#### Original first-release notes

- `R CMD check --as-cran`: zero errors, zero warnings verified across
  ubuntu/windows/macos x R release/oldrel/devel
  ([\#25](https://github.com/davidrsch/rrlmgraph/issues/25)).
- `DESCRIPTION`: added `BugReports` URL and `spelling` to `Suggests`.
- `inst/WORDLIST`: package-specific technical terms registered to pass
  [`spelling::spell_check_package()`](https://docs.ropensci.org/spelling//reference/spell_check_package.html)
  cleanly.
- CI: added spelling check step after
  [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
  ([\#25](https://github.com/davidrsch/rrlmgraph/issues/25)).

#### Breaking changes

- [`chat_with_context()`](https://davidrsch.github.io/rrlmgraph/reference/chat_with_context.md):
  the `model` parameter is now `NULL` by default (was `"gpt-4o-mini"`).
  A per-provider sensible default is applied automatically. Code that
  passed `model` explicitly is unaffected.

#### Improvements

- [`chat_with_context()`](https://davidrsch.github.io/rrlmgraph/reference/chat_with_context.md)
  gains a `provider` argument (`"openai"`, `"ollama"`, `"github"`,
  `"anthropic"`). When `ellmer` is installed the appropriate
  `ellmer::chat_*()` backend is used; the `httr2` fallback is retained
  for `provider = "openai"` when `ellmer` is absent
  ([\#41](https://github.com/davidrsch/rrlmgraph/issues/41)).
- [`ollama_available()`](https://davidrsch.github.io/rrlmgraph/reference/ollama_available.md)
  no longer requires the `ollamar` package. It now pings the Ollama REST
  API directly via `httr2` (`GET /api/tags`). Set `OLLAMA_BASE_URL` to
  override the default `http://localhost:11434`
  ([\#40](https://github.com/davidrsch/rrlmgraph/issues/40)).
- Ollama embedding calls
  ([`embed_nodes()`](https://davidrsch.github.io/rrlmgraph/reference/embed_nodes.md),
  [`embed_query()`](https://davidrsch.github.io/rrlmgraph/reference/embed_query.md))
  now hit `POST /api/embed` directly via `httr2`, removing the `ollamar`
  dependency entirely
  ([\#40](https://github.com/davidrsch/rrlmgraph/issues/40)).

#### New features

**Graph construction**

- [`build_rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/build_rrlm_graph.md)
  – build a typed knowledge graph for any R project.
- [`update_graph_incremental()`](https://davidrsch.github.io/rrlmgraph/reference/update_graph_incremental.md)
  – re-parse only changed files for fast single-file updates.
- [`extract_function_nodes()`](https://davidrsch.github.io/rrlmgraph/reference/extract_function_nodes.md)
  – AST-based function node extraction supporting standard assignments,
  S4 generics/methods, and R5/R6 classes.
- [`find_calls_in_body()`](https://davidrsch.github.io/rrlmgraph/reference/find_calls_in_body.md)
  – enumerate global symbol references in a function body, filtering NSE
  false-positives.
- [`build_call_edges()`](https://davidrsch.github.io/rrlmgraph/reference/build_call_edges.md)
  – intra-project CALLS edges.
- [`build_import_edges()`](https://davidrsch.github.io/rrlmgraph/reference/build_import_edges.md)
  – IMPORTS edges from source files to package dependencies (library
  calls, qualified calls, and DESCRIPTION fields).
- [`build_test_edges()`](https://davidrsch.github.io/rrlmgraph/reference/build_test_edges.md)
  – TEST edges from test files to user-defined functions.

**Querying and context assembly**

- [`query_context()`](https://davidrsch.github.io/rrlmgraph/reference/query_context.md)
  – relevance-guided BFS producing a token-budgeted context object for
  use with an LLM.
- [`compute_relevance()`](https://davidrsch.github.io/rrlmgraph/reference/compute_relevance.md)
  – composite relevance score (semantic, PageRank, task-trace,
  co-change).
- [`build_node_context()`](https://davidrsch.github.io/rrlmgraph/reference/build_node_context.md)
  – full or compressed text representation of a single graph node.
- [`assemble_context_string()`](https://davidrsch.github.io/rrlmgraph/reference/assemble_context_string.md)
  – structured, LLM-ready prompt string from ranked node hits.

**Embedding**

- [`embed_nodes()`](https://davidrsch.github.io/rrlmgraph/reference/embed_nodes.md)
  – TF-IDF (default), Ollama, or OpenAI dense embeddings.
- [`embed_query()`](https://davidrsch.github.io/rrlmgraph/reference/embed_query.md)
  – project a free-text query into the model’s vector space.
- [`cosine_similarity()`](https://davidrsch.github.io/rrlmgraph/reference/cosine_similarity.md)
  – cosine similarity between two numeric vectors.
- [`ollama_available()`](https://davidrsch.github.io/rrlmgraph/reference/ollama_available.md)
  – runtime check for a reachable Ollama daemon.

**LLM interface**

- [`chat_with_context()`](https://davidrsch.github.io/rrlmgraph/reference/chat_with_context.md)
  – chat with an LLM using graph-derived context; uses `ellmer` when
  installed, falls back to `httr2`.

**Task learning**

- [`log_task_trace()`](https://davidrsch.github.io/rrlmgraph/reference/log_task_trace.md)
  – append a JSONL feedback entry to `.rrlmgraph/task_trace.jsonl`.
- [`update_task_weights()`](https://davidrsch.github.io/rrlmgraph/reference/update_task_weights.md)
  – apply exponential decay and accumulate per-node weights from the
  trace file.
- [`update_task_polarity()`](https://davidrsch.github.io/rrlmgraph/reference/update_task_polarity.md)
  – rewrite polarity values of recent trace entries.

**Cache and export**

- [`save_graph_cache()`](https://davidrsch.github.io/rrlmgraph/reference/save_graph_cache.md)
  – serialise graph to `.rrlmgraph/graph.rds`.
- [`load_graph_cache()`](https://davidrsch.github.io/rrlmgraph/reference/load_graph_cache.md)
  – restore graph from cache, refreshing task weights.
- [`is_cache_stale()`](https://davidrsch.github.io/rrlmgraph/reference/is_cache_stale.md)
  – compare cache mtime against R source files.
- [`export_to_sqlite()`](https://davidrsch.github.io/rrlmgraph/reference/export_to_sqlite.md)
  – write nodes, edges, and traces to a SQLite database for the
  TypeScript MCP server.

**Project detection**

- [`detect_rproject()`](https://davidrsch.github.io/rrlmgraph/reference/detect_rproject.md)
  – detect project type and discover all R source files.

**Instructions**

- [`generate_instructions()`](https://davidrsch.github.io/rrlmgraph/reference/generate_instructions.md)
  – generate a Copilot-style instruction Markdown file from the project
  graph.

**S3 methods**

- [`print.rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/print.rrlm_graph.md),
  [`summary.rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/summary.rrlm_graph.md),
  [`plot.rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/plot.rrlm_graph.md)
- [`print.rrlm_context()`](https://davidrsch.github.io/rrlmgraph/reference/print.rrlm_context.md),
  [`summary.rrlm_context()`](https://davidrsch.github.io/rrlmgraph/reference/summary.rrlm_context.md)
