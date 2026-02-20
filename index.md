# rrlmgraph <img src="man/figures/logo.svg" align="right" height="139" alt="rrlmgraph logo" />

> Typed knowledge graphs for R projects — give your LLM the right context,
> not the whole codebase.

## Overview

`rrlmgraph` builds a typed knowledge graph of an R project and uses it to
supply large language models with precise, token-budgeted context. It reduces
hallucinations and token consumption by grounding every LLM response in
verified, version-specific project knowledge.

## Installation

```r
# install.packages("pak")
pak::pak("davidrsch/rrlmgraph")
```

## Quick start

```r
library(rrlmgraph)

# Build the graph for any R project
g <- build_rrlm_graph("path/to/my_project")

# Query for context relevant to a task
ctx <- query_context(g, "How does the model evaluation work?")
cat(ctx$context_string)

# Chat with an LLM using graph-derived context
answer <- chat_with_context(g, "Add cross-validation to fit_model()")
```

## Learn more

See the [Getting Started](articles/getting-started.html) vignette for a
complete walkthrough, and the [Reference](reference/index.html) for full
function documentation.
