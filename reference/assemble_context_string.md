# Assemble a structured, LLM-ready context string from ranked hits

Takes an ordered list of node names (seed first) and returns a formatted
context string suitable for inclusion in a language-model prompt. The
structure is:

## Usage

``` r
assemble_context_string(hits, graph, query = "")
```

## Arguments

- hits:

  Character vector. Node names ordered by relevance, seed (most
  relevant) first.

- graph:

  An `rrlm_graph` / `igraph` object.

- query:

  Character(1). The original user query string. Included in the header
  for context.

## Value

Character(1) valid UTF-8 string.
[`nchar()`](https://rdrr.io/r/base/nchar.html) is stable across calls
with identical inputs.

## Details

1.  **Header** – project name, R version, approximate token count.

2.  **CORE FUNCTIONS** – seed node rendered in full-source mode.

3.  **SUPPORTING FUNCTIONS** – remaining user-function nodes in
    compressed mode.

4.  **FRAMEWORK / PACKAGE CONTEXT** – package-type nodes in compressed
    mode.

5.  **RECENT TASK HISTORY** – up to 3 entries from the `task_history`
    graph attribute; omitted when empty.

6.  **CONSTRAINTS** – boilerplate footer reminding the LLM to use only
    listed functions.

## See also

[`build_node_context()`](https://davidrsch.github.io/rrlmgraph/reference/build_node_context.md),
[`compute_relevance()`](https://davidrsch.github.io/rrlmgraph/reference/compute_relevance.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g    <- build_rrlm_graph("mypkg")
hits <- c("utils::load_data", "utils::clean_data", "dplyr")
cat(assemble_context_string(hits, g, "load and clean training data"))
} # }
```
