# Adversarial Audit — rrlmgraph workspace

## Repositories in scope

| Repo                        | Language   | Role                                                   |
|-----------------------------|------------|--------------------------------------------------------|
| `davidrsch/rrlmgraph`       | R          | Core igraph-based code-graph package                   |
| `davidrsch/rrlmgraph-bench` | R          | Benchmark suite comparing context-retrieval strategies |
| `davidrsch/rrlmgraph-mcp`   | TypeScript | MCP stdio server exposing graph traversal to LLMs      |

## Pre-audit reading (do this first, in parallel)

Before raising any findings, read and internalise:

1.  The paper abstract at <https://arxiv.org/abs/2512.24601> (15-line
    summary in memory is fine).
2.  All three `README.md` files.
3.  All three `NEWS.md` / `CHANGELOG.md` files — understand what has
    already been fixed.
4.  Every source file in `rrlmgraph/R/`, `rrlmgraph-bench/R/`, and
    `rrlmgraph-mcp/src/**/*.ts`.
5.  All vignettes in `rrlmgraph/vignettes/` and
    `rrlmgraph-bench/vignettes/`.
6.  All `man/*.Rd` roxygen output files for exported symbols in both R
    packages.
7.  Every CI/CD workflow in `.github/workflows/` across all three repos.
8.  `package.json`, `tsconfig.json` for the MCP server.
9.  `DESCRIPTION` files for both R packages (Version, Depends, Imports,
    Suggests).

## Audit dimensions — examine ALL of these

For each dimension, read the relevant code before raising findings. Do
not raise findings based on partial reads.

### 1. Correctness

- Algorithm implementations match their documented behaviour (BFS
  traversal direction, edge types traversed, embedding encoding, cosine
  similarity computation).
- Data flows between the three packages are consistent (attribute names,
  column names, JSON field names, SQLite schema vs R-side expectations).
- Token-budget enforcement uses the same estimator across all retrieval
  strategies.
- Scoring weights in `score_response()` match what is documented in
  vignettes and README.
- Multi-turn token aggregation is arithmetically correct (no vector
  recycling).
- NDCG is computable for every strategy (not always `NA`).

### 2. Benchmark validity

- Default strategy list in `run_full_benchmark()` includes every
  strategy that `build_context()` supports.
- `N_STRATEGIES` in workflow environment variables matches the actual
  default strategy count.
- Agentic strategy records which nodes were actually visited (not empty)
  so NDCG is meaningful.
- Random baselines and agentic strategies are included in every
  comparison run.

### 3. Security (OWASP Top 10 + supply-chain)

- No [`eval()`](https://rdrr.io/r/base/eval.html) /
  [`parse()`](https://rdrr.io/r/base/parse.html) on LLM-generated text
  without a sandbox blocking `system`, `system2`, `unlink`,
  `file.remove`, `shell`, `Sys.setenv`, etc.
- No `curl | sh` or equivalent unverified installer. Any installer must
  be downloaded to a temp file, SHA256-verified, then executed.
- No injection risk in SQL queries (BFS CTE, FTS5, LIKE patterns). Check
  LIKE wildcards (`%`, `_`, `\`) are properly escaped.
- Workflow permissions are minimal (`contents: read` by default; write
  permissions only where strictly required).
- No `pull_request_target` workflows executing code from the PR branch.
- All third-party GitHub Actions pinned to full commit SHAs (not mutable
  tags like `@v4`).
- No secrets logged or echoed in workflow steps.

### 4. Documentation — MUST CHECK ALL OF THESE

- Every exported R function has a roxygen block with `@param` for every
  parameter (including optional ones added recently), `@return`, and
  `@examples` where appropriate.
- Parameter names in roxygen `@param` entries match actual function
  signatures exactly.
- Vignette prose (strategy tables, score formulas, strategy counts) is
  consistent with the code. Check all numeric coefficients, all strategy
  names, all strategy counts.
- `NEWS.md` (R packages) and `CHANGELOG.md` (MCP server) have entries
  for every fix and new feature in the current development version.
  Entries must reference issue numbers.
- `DESCRIPTION` `Version:` fields are consistent with `NEWS.md` section
  headers.
- README examples are runnable and reference current API (function
  names, argument names).
- Vignette strategy tables list all strategies that `build_context()` /
  `run_full_benchmark()` support.
- Score formula coefficients in vignettes match `score_response()`
  source exactly.

### 5. CI/CD completeness

- Every workflow has a `permissions:` block.
- Lint / type-check steps run on every PR (not just on main or tags).
- Integration tests run before any publish/deploy step.
- R package builds are tested on both current and old-release R, and on
  Windows + Linux.
- Benchmark workflow gate logic correctly reflects the actual strategy
  count.
- Workflow concurrency settings prevent duplicate runs eating quota.

### 6. Paradigm fidelity (RLM-Graph)

- The `graph_rag_agentic` strategy genuinely implements RLM-Graph (LLM
  drives traversal via tool calls; no pre-assembled context).
- Distinguish `graph_rag_agentic` (true agentic) from static graph-RAG
  strategies.
- The benchmark reports this distinction clearly in its vignette and
  result commentary.

### 7. Dead code and consistency

- Internal helpers used in only one strategy do not silently differ from
  equivalent helpers in other strategies.
- Any `@keywords internal` function that is never called from non-test
  code should be flagged.
- Strategy names used in `build_context()`, `run_full_benchmark()`,
  vignettes, README, and CI workflows are all identical and consistently
  spelled.

## Issue creation rules

- One GitHub issue per distinct finding. Do NOT aggregate multiple bugs
  into one issue.

- Title format: `SEVERITY [Dimension] Short description` where SEVERITY
  ∈ {CRITICAL, HIGH, MEDIUM, LOW}.

- Body must contain: (a) exact file path and line numbers, (b) the
  incorrect current behaviour, (c) the correct expected behaviour, (d) a
  minimal reproduction or proof.

- Label with the severity string.

- After creating all issues, add every issue to GitHub project \#13:

      gh project item-add 13 --owner davidrsch --url <issue-url>

## Fix implementation rules

After all issues are created and added to the project:

1.  Fix every finding. Implement the actual change; do not leave TODOs.
2.  **Documentation is a first-class fix target** — update every
    affected:
    - roxygen `@param` / `@return` entry
    - Vignette prose (score formulas, strategy tables, counts)
    - `NEWS.md` / `CHANGELOG.md` (add a `(development version)` or
      `[Unreleased]` section)
    - `README.md` examples if function signatures changed
3.  Do not fix issues in a different repo/file without also fixing
    cross-cutting consistency (e.g. if a strategy is added to bench, the
    rrlmgraph vignette strategy table and NEWS must also be updated).
4.  Run R syntax checks ([`parse()`](https://rdrr.io/r/base/parse.html))
    on every modified `.R` file before committing.
5.  Run TypeScript type checks (`npx tsc --noEmit`) before committing to
    the MCP repo.

## Commit and push rules

- Commit each repo separately with a message that references all fixed
  issue numbers.
- Push all three repos.
- Poll `gh run list` until every triggered workflow shows
  `completed / success`.
- If any workflow fails, read the failure log, fix the root cause, and
  re-push. Do not mark the task done until all CI is green.

## Issue closure

After CI is green, close every issue you created:

    gh issue close <number> --repo <owner/repo> --comment "Fixed in <commit-sha>. CI green."

## Definition of done

The audit is complete when ALL of the following are true:

Every source file, vignette, man page, NEWS/CHANGELOG, and workflow
read.

No finding overlooked (re-verify each dimension against the final
codebase).

One GitHub issue per finding, all in project \#13.

All findings fixed in code **and** documentation.

`NEWS.md` / `CHANGELOG.md` updated with every fix.

All three repos pushed.

All CI workflows green.

All created issues closed with fix references.
