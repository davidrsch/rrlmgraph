# Changelog

## rrlmgraph (development version)

#### CI fix (adversarial audit)

- `precompute-vignettes.yml`: Removed `continue-on-error: true` from the
  “Trigger pkgdown rebuild” step. Previously, a failure to dispatch the
  pkgdown workflow (e.g. insufficient token permissions or missing
  workflow file) was silently swallowed, causing the precompute workflow
  to report green even though the published site would not be refreshed
  (rrlmgraph#115).

#### Bug fixes (adversarial audit)

- `graph_traverse.R`
  [`query_context()`](https://davidrsch.github.io/rrlmgraph/dev/reference/query_context.md):
  Seed-context overflow truncation now uses 3.5 chars/token (matching
  the final-assembly truncation fixed in
  [\#114](https://github.com/davidrsch/rrlmgraph/issues/114)) instead of
  4 chars/token
  ([\#118](https://github.com/davidrsch/rrlmgraph/issues/118)).
- `graph_traverse.R`
  [`query_context()`](https://davidrsch.github.io/rrlmgraph/dev/reference/query_context.md):
  Hard-truncation ceiling now uses 3.5 chars/token (matching
  `context_assemble.R`, `generate_instructions.R`, and `rrlmgraph-mcp`)
  instead of the previous 4 chars/token
  ([\#114](https://github.com/davidrsch/rrlmgraph/issues/114)).
- `vignettes/benchmark-results.Rmd`: Corrected `graph_rag_agentic`
  description from “LLM drives MCP tool calls iteratively” to “REPL loop
  (Algorithm 1, arXiv:2512.24601)”
  ([\#113](https://github.com/davidrsch/rrlmgraph/issues/113)).

#### Code organisation

- All R source files capped at 400 lines. Oversized files split at
  natural function boundaries into focused modules:
  - `edge_builders.R` → `edge_builders.R` + `edge_builders_cochange.R` +
    `edge_builder_utils.R`
  - `graph_build.R` → `graph_build.R` + `graph_build_helpers.R`
  - `rrlm_graph.R` → `rrlm_graph.R` + `rrlm_graph_helpers.R`
  - `sqlite_export.R` → `sqlite_export.R` + `sqlite_export_helpers.R`
  - `task_trace.R` → `task_trace.R` + `task_trace_io.R`
  - `llm_interface.R` → `llm_interface.R` + `llm_backend.R`
  - `context_assemble.R` → `context_assemble.R` + `context_formatters.R`
    No public API changes.

#### Bug fixes

- `graph_traverse.R`
  [`query_context()`](https://davidrsch.github.io/rrlmgraph/dev/reference/query_context.md):
  BFS token-cost estimation inside the loop now always uses
  `"compressed"` mode for all supporting (non-seed) nodes. Previously
  the condition `if (nodes_added == 0L)` caused the first BFS-discovered
  node to be costed as `"full"` (~3× larger), leading to premature
  termination of the traversal
  ([\#103](https://github.com/davidrsch/rrlmgraph/issues/103)).
- `context_assemble.R`: fixed token-count heuristic from `/ 4` to
  `/ 3.5`, matching all other token-counting sites in the codebase
  ([\#95](https://github.com/davidrsch/rrlmgraph/issues/95)).
- `task_trace.R`: weight normalization floor changed from `0.1` to `0`
  so that zero-weight tasks are achievable as documented
  ([\#96](https://github.com/davidrsch/rrlmgraph/issues/96)).
- `relevance.R` `.count_tokens()`: added cross-repo sync note directing
  maintainers to also update `.bench_estimate_tokens()` in
  `rrlmgraph-bench` when changing the formula
  ([\#101](https://github.com/davidrsch/rrlmgraph/issues/101)).
- `generate_instructions.R`
  [`generate_instructions()`](https://davidrsch.github.io/rrlmgraph/dev/reference/generate_instructions.md):
  token-budget computation changed from `max_tokens * 4L` (4
  chars/token) to `ceiling(max_tokens * 3.5)` (3.5 chars/token),
  matching `.count_tokens()`, `context_assemble.R`, and the MCP
  `estimateTokens()` helper. `@param max_tokens` doc updated accordingly
  ([\#107](https://github.com/davidrsch/rrlmgraph/issues/107)).

#### CI / infrastructure

- `pkgdown.yaml`: added a `Gate on R-CMD-check` step that verifies the
  most recent R-CMD-check run has a `success` conclusion before allowing
  docs to be deployed on `release` and `workflow_dispatch` events,
  preventing documentation from being published from a broken commit
  ([\#104](https://github.com/davidrsch/rrlmgraph/issues/104)).
- `precompute-vignettes.yml`, `pr-commands.yaml`: added `concurrency:`
  blocks to prevent redundant concurrent runs
  ([\#105](https://github.com/davidrsch/rrlmgraph/issues/105)).
- Added top-level `permissions: {}` deny-all block to
  `format-suggest.yaml` so runtime permissions are granted per-job only
  ([\#97](https://github.com/davidrsch/rrlmgraph/issues/97)).
- Added `concurrency:` groups to `R-CMD-check.yaml`,
  `test-coverage.yaml`, and `format-suggest.yaml` to cancel stale runs
  on new pushes
  ([\#97](https://github.com/davidrsch/rrlmgraph/issues/97)).
- SHA-pinned all remaining mutable action tags:
  `posit-dev/setup-air@v1`, `reviewdog/action-suggester@v1`,
  `JamesIves/github-pages-deploy-action@v4.5.0`,
  `codecov/codecov-action@v5`, `r-lib/actions/pr-fetch@v2`, and
  `r-lib/actions/pr-push@v2`
  ([\#98](https://github.com/davidrsch/rrlmgraph/issues/98)).
- `pkgdown.yaml`: deployment now triggered by `workflow_run` on
  `R-CMD-check` completion instead of directly on `push`, so broken
  packages can no longer produce a published documentation site
  ([\#99](https://github.com/davidrsch/rrlmgraph/issues/99)).
- `pr-commands.yaml`: removed `continue-on-error: true` from the
  style-job commit step; the `|| echo "No changes to commit"` guard in
  the `run:` block already handles the empty-commit case without hiding
  genuine errors
  ([\#102](https://github.com/davidrsch/rrlmgraph/issues/102)).

#### Documentation

- `DESCRIPTION`: version now carries `.9000` development suffix on
  `main` branch to distinguish development snapshots from the CRAN
  release ([\#100](https://github.com/davidrsch/rrlmgraph/issues/100)).
- `NEWS.md`: merged duplicate `### Documentation` subsection heading in
  the development section into a single heading
  ([\#106](https://github.com/davidrsch/rrlmgraph/issues/106)).
- [`compute_relevance()`](https://davidrsch.github.io/rrlmgraph/dev/reference/compute_relevance.md):
  added a `@note` paragraph explaining that the MCP server’s TypeScript
  BFS uses a depth-penalty signal in place of the co-change signal
  because `CO_CHANGES` edge weights are not stored in the exported
  SQLite schema. Closes mcp#41.
- `vignettes/mcp-integration.Rmd`: replaced the **Available MCP tools**
  table with the complete, correctly-named 8-tool set; previous table
  listed 4 wrong names (`get_node`, `list_nodes`, `get_neighbours`,
  `graph_stats`) and omitted `find_callees`, `search_nodes`,
  `add_task_trace`, and `rebuild_graph`. Closes
  [\#91](https://github.com/davidrsch/rrlmgraph/issues/91).
- `vignettes/benchmark-results.Rmd`: fixed stale strategy-name literals
  `"rrlmgraph"` / `"rrlmgraph_mcp"` in both plot chunks; correct names
  are `"graph_rag_tfidf"` / `"graph_rag_mcp"`, so all data points were
  rendering grey. Closes
  [\#92](https://github.com/davidrsch/rrlmgraph/issues/92).
- `vignettes/benchmark-results.Rmd`: corrected column names in the
  `summary-table`, `score-plot`, and `pairwise` code chunks to match the
  actual output of `rrlmgraphbench::compute_benchmark_statistics()`:
  `ci_lower` → `ci_lo_95`, `ci_upper` → `ci_hi_95`, `mean_tokens` →
  `mean_total_tokens`, `n_obs` → `n`, `strategy_a` / `strategy_b` →
  `strategy_1` / `strategy_2`. Added inline derivation of a
  `significant` column (absent from the function’s return value). Fixed
  prose reference `detect_hallucination()` → `count_hallucinations()`.
  Closes [\#93](https://github.com/davidrsch/rrlmgraph/issues/93).
- `vignettes/mcp-integration.Rmd`: corrected CLI flag `--db` →
  `--db-path`, file extension `graph.db` → `graph.sqlite` throughout
  (architecture diagram, npx example, VS Code JSON config), and added
  the required `db_path` argument to both
  [`export_to_sqlite()`](https://davidrsch.github.io/rrlmgraph/dev/reference/export_to_sqlite.md)
  calls. Closes
  [\#94](https://github.com/davidrsch/rrlmgraph/issues/94).

#### CI / Security

- All GitHub Actions workflows are now pinned to full commit SHA digests
  instead of mutable `@v4`/`@v2` tags, eliminating the supply-chain risk
  described in OWASP A06. Closes
  [\#90](https://github.com/davidrsch/rrlmgraph/issues/90).

## rrlmgraph 0.1.3

#### Bug fixes

- [`export_to_sqlite()`](https://davidrsch.github.io/rrlmgraph/dev/reference/export_to_sqlite.md):
  `.upsert_nodes()` was reading the non-existent vertex attribute
  `task_weight` instead of the actual `task_trace_weight` attribute
  written by
  [`log_task_trace()`](https://davidrsch.github.io/rrlmgraph/dev/reference/log_task_trace.md).
  Every exported `task_weight` value was `NA`, silently breaking the
  feedback loop that re-ranks nodes after task completion
  ([\#89](https://github.com/davidrsch/rrlmgraph/issues/89)).
- `task_trace_weight` cold-start default changed from `0.5` to `0.0`
  ([\#78](https://github.com/davidrsch/rrlmgraph/issues/78)). Previously
  the very first BFS when `task_trace_weight` was uninitialized boosted
  all neighbours equally (0.5), causing spoke nodes to score above
  `min_relevance` even when unrelated to the query, inflating context
  size. The new default keeps spoke influence at zero until an explicit
  weight is provided.

## rrlmgraph 0.1.2

#### New vignettes

- **LLM Chat with Context** (`vignettes/llm-chat.Rmd`): shows real model
  responses loaded from a pre-captured RDS — no API key required to read
  the vignette.
- **Benchmark Results** (`vignettes/benchmark-results.Rmd`): compares
  the eight context-retrieval strategies from **rrlmgraph-bench** with
  summary tables and a score plot.
- **MCP Server Integration** (`vignettes/mcp-integration.Rmd`):
  narrative guide and VS Code configuration for the **rrlmgraph-mcp**
  companion.

#### Infrastructure

- Added `.github/workflows/precompute-vignettes.yml`
  (`workflow_dispatch`): calls GitHub Models once, serialises LLM
  responses and benchmark statistics to `vignettes/precomputed/`, then
  triggers a pkgdown site rebuild.

## rrlmgraph 0.1.1

#### Bug fixes

- [`extract_function_nodes()`](https://davidrsch.github.io/rrlmgraph/dev/reference/extract_function_nodes.md)
  / `.make_node()`: node IDs now include the parent directory name as a
  prefix (e.g. `"R/utils::fn"` instead of `"utils::fn"`), matching the
  seed-node format used by the benchmark task definitions. Previously,
  [`query_context()`](https://davidrsch.github.io/rrlmgraph/dev/reference/query_context.md)
  would abort with “seed_node not in graph” for any benchmark task,
  leaving `retrieved_n = 0` for the `rrlmgraph_tfidf` strategy on every
  trial ([\#78](https://github.com/davidrsch/rrlmgraph/issues/78)).

## rrlmgraph 0.1.0

First release.

#### Improvements

- [`plot.rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/dev/reference/plot.rrlm_graph.md):
  replaced the static
  [`igraph::plot.igraph()`](https://r.igraph.org/reference/plot.igraph.html)
  backend with an interactive **Graphviz** widget via
  [`DiagrammeR::grViz()`](https://rich-iannone.github.io/DiagrammeR/reference/grViz.html).
  Nodes are grouped into dashed sub-graph boxes by source file, coloured
  by node type, and sized by PageRank. The widget renders in the RStudio
  Viewer or any browser with full pan-and-zoom support (scroll to zoom
  centred on cursor, drag to pan, pinch on touch devices, double-click
  to reset) via injected vanilla JS – no CDN or plugin required. A new
  `layout` parameter selects the Graphviz engine (`"dot"` hierarchical,
  `"neato"`, `"fdp"`, `"sfdp"`, `"circo"`); a new `file` parameter saves
  to `.html` (via **htmlwidgets**) or to `.png`/`.pdf`/`.svg` (via
  **webshot2**).
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

- [`chat_with_context()`](https://davidrsch.github.io/rrlmgraph/dev/reference/chat_with_context.md):
  the `model` parameter is now `NULL` by default (was `"gpt-4o-mini"`).
  A per-provider sensible default is applied automatically. Code that
  passed `model` explicitly is unaffected.

#### Improvements

- [`chat_with_context()`](https://davidrsch.github.io/rrlmgraph/dev/reference/chat_with_context.md)
  gains a `provider` argument (`"openai"`, `"ollama"`, `"github"`,
  `"anthropic"`). When `ellmer` is installed the appropriate
  `ellmer::chat_*()` backend is used; the `httr2` fallback is retained
  for `provider = "openai"` when `ellmer` is absent
  ([\#41](https://github.com/davidrsch/rrlmgraph/issues/41)).
- [`ollama_available()`](https://davidrsch.github.io/rrlmgraph/dev/reference/ollama_available.md)
  no longer requires the `ollamar` package. It now pings the Ollama REST
  API directly via `httr2` (`GET /api/tags`). Set `OLLAMA_BASE_URL` to
  override the default `http://localhost:11434`
  ([\#40](https://github.com/davidrsch/rrlmgraph/issues/40)).
- Ollama embedding calls
  ([`embed_nodes()`](https://davidrsch.github.io/rrlmgraph/dev/reference/embed_nodes.md),
  [`embed_query()`](https://davidrsch.github.io/rrlmgraph/dev/reference/embed_query.md))
  now hit `POST /api/embed` directly via `httr2`, removing the `ollamar`
  dependency entirely
  ([\#40](https://github.com/davidrsch/rrlmgraph/issues/40)).

#### Bug fixes

- `R/parse_ast.R`: `as.character(expr[[1L]])` now guarded with an extra
  `[[1L]]` so that namespace-qualified calls (`pkg::fn`) no longer
  produce a length-3 character vector, which previously caused a
  “condition has length \> 1” crash when building the graph. Same guard
  added in `.collect_nse_symbols()` and `.find_qualified_calls()`.
  R6Class detection changed to use `%in%` rather than `==` for
  robustness against multi-element vectors.
- `R/graph_build.R`:
  [`build_test_edges()`](https://davidrsch.github.io/rrlmgraph/dev/reference/build_test_edges.md)
  now skips unresolvable bare names instead of throwing “subscript out
  of bounds” when a called symbol does not appear in the function-node
  index.
- `NAMESPACE`: restored `S3method()` registrations for
  `plot.rrlm_graph`, `print.rrlm_graph`, `summary.rrlm_graph`,
  `print.rrlm_context`, and `summary.rrlm_context`. Previously the file
  listed plain `export()` entries, which broke S3 dispatch so igraph
  methods were called instead. A new `R/imports.R` carrying all
  `@importFrom` roxygen tags ensures
  [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
  regenerates the full `importFrom()` block.
- [`print.rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/dev/reference/print.rrlm_graph.md),
  [`summary.rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/dev/reference/summary.rrlm_graph.md):
  replaced `cli::cli_*()` calls with
  [`cat()`](https://rdrr.io/r/base/cat.html) so output is captured by
  [`capture.output()`](https://rdrr.io/r/utils/capture.output.html) in
  tests and pipe operators work correctly.
- `inst/WORDLIST`: added `centres`, `cex`, `Kamada`, `Kawai` to silence
  [`spelling::spell_check_package()`](https://docs.ropensci.org/spelling//reference/spell_check_package.html)
  failures in CI.
- `tests/testthat/test-parse-ast.R`: fixed `write_r_file()` helper so
  the temporary file outlives the helper call (was deleted immediately
  by
  [`withr::local_tempfile()`](https://withr.r-lib.org/reference/with_tempfile.html);
  now uses [`tempfile()`](https://rdrr.io/r/base/tempfile.html) +
  `withr::defer(..., envir = parent.frame())`).
- `tests/testthat/test-s3-methods.R`: replaced
  `igraph::V(g)$attr <- NULL` with
  [`igraph::delete_vertex_attr()`](https://r.igraph.org/reference/delete_vertex_attr.html)
  /
  [`igraph::delete_edge_attr()`](https://r.igraph.org/reference/delete_edge_attr.html)
  to match the igraph 2.x API.

#### New features

**Graph construction**

- [`build_rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/dev/reference/build_rrlm_graph.md)
  – build a typed knowledge graph for any R project.
- [`update_graph_incremental()`](https://davidrsch.github.io/rrlmgraph/dev/reference/update_graph_incremental.md)
  – re-parse only changed files for fast single-file updates.
- [`extract_function_nodes()`](https://davidrsch.github.io/rrlmgraph/dev/reference/extract_function_nodes.md)
  – AST-based function node extraction supporting standard assignments,
  S4 generics/methods, and R5/R6 classes.
- [`find_calls_in_body()`](https://davidrsch.github.io/rrlmgraph/dev/reference/find_calls_in_body.md)
  – enumerate global symbol references in a function body, filtering NSE
  false-positives.
- [`build_call_edges()`](https://davidrsch.github.io/rrlmgraph/dev/reference/build_call_edges.md)
  – intra-project CALLS edges.
- [`build_import_edges()`](https://davidrsch.github.io/rrlmgraph/dev/reference/build_import_edges.md)
  – IMPORTS edges from source files to package dependencies (library
  calls, qualified calls, and DESCRIPTION fields).
- [`build_test_edges()`](https://davidrsch.github.io/rrlmgraph/dev/reference/build_test_edges.md)
  – TEST edges from test files to user-defined functions.

**Querying and context assembly**

- [`query_context()`](https://davidrsch.github.io/rrlmgraph/dev/reference/query_context.md)
  – relevance-guided BFS producing a token-budgeted context object for
  use with an LLM.
- [`compute_relevance()`](https://davidrsch.github.io/rrlmgraph/dev/reference/compute_relevance.md)
  – composite relevance score (semantic, PageRank, task-trace,
  co-change).
- [`build_node_context()`](https://davidrsch.github.io/rrlmgraph/dev/reference/build_node_context.md)
  – full or compressed text representation of a single graph node.
- [`assemble_context_string()`](https://davidrsch.github.io/rrlmgraph/dev/reference/assemble_context_string.md)
  – structured, LLM-ready prompt string from ranked node hits.

**Embedding**

- [`embed_nodes()`](https://davidrsch.github.io/rrlmgraph/dev/reference/embed_nodes.md)
  – TF-IDF (default), Ollama, or OpenAI dense embeddings.
- [`embed_query()`](https://davidrsch.github.io/rrlmgraph/dev/reference/embed_query.md)
  – project a free-text query into the model’s vector space.
- [`cosine_similarity()`](https://davidrsch.github.io/rrlmgraph/dev/reference/cosine_similarity.md)
  – cosine similarity between two numeric vectors.
- [`ollama_available()`](https://davidrsch.github.io/rrlmgraph/dev/reference/ollama_available.md)
  – runtime check for a reachable Ollama daemon.

**LLM interface**

- [`chat_with_context()`](https://davidrsch.github.io/rrlmgraph/dev/reference/chat_with_context.md)
  – chat with an LLM using graph-derived context; uses `ellmer` when
  installed, falls back to `httr2`.

**Task learning**

- [`log_task_trace()`](https://davidrsch.github.io/rrlmgraph/dev/reference/log_task_trace.md)
  – append a JSONL feedback entry to `.rrlmgraph/task_trace.jsonl`.
- [`update_task_weights()`](https://davidrsch.github.io/rrlmgraph/dev/reference/update_task_weights.md)
  – apply exponential decay and accumulate per-node weights from the
  trace file.
- [`update_task_polarity()`](https://davidrsch.github.io/rrlmgraph/dev/reference/update_task_polarity.md)
  – rewrite polarity values of recent trace entries.

**Cache and export**

- [`save_graph_cache()`](https://davidrsch.github.io/rrlmgraph/dev/reference/save_graph_cache.md)
  – serialise graph to `.rrlmgraph/graph.rds`.
- [`load_graph_cache()`](https://davidrsch.github.io/rrlmgraph/dev/reference/load_graph_cache.md)
  – restore graph from cache, refreshing task weights.
- [`is_cache_stale()`](https://davidrsch.github.io/rrlmgraph/dev/reference/is_cache_stale.md)
  – compare cache mtime against R source files.
- [`export_to_sqlite()`](https://davidrsch.github.io/rrlmgraph/dev/reference/export_to_sqlite.md)
  – write nodes, edges, and traces to a SQLite database for the
  TypeScript MCP server.

**Project detection**

- [`detect_rproject()`](https://davidrsch.github.io/rrlmgraph/dev/reference/detect_rproject.md)
  – detect project type and discover all R source files.

**Instructions**

- [`generate_instructions()`](https://davidrsch.github.io/rrlmgraph/dev/reference/generate_instructions.md)
  – generate a Copilot-style instruction Markdown file from the project
  graph.

**S3 methods**

- [`print.rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/dev/reference/print.rrlm_graph.md),
  [`summary.rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/dev/reference/summary.rrlm_graph.md),
  [`plot.rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/dev/reference/plot.rrlm_graph.md)
- [`print.rrlm_context()`](https://davidrsch.github.io/rrlmgraph/dev/reference/print.rrlm_context.md),
  [`summary.rrlm_context()`](https://davidrsch.github.io/rrlmgraph/dev/reference/summary.rrlm_context.md)
