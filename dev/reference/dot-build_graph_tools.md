# Build ellmer tool definitions that expose graph navigation to the LLM.

The four tools give the LLM the same graph-traversal primitives
available in the MCP server, allowing it to iteratively explore call
relationships and read function bodies before producing its answer. This
is the RLM-Graph execution mechanism: the LLM drives traversal, the R
environment executes.

## Usage

``` r
.build_graph_tools(graph, budget_tokens, min_relevance)
```
