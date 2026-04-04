# LLM Chat with Context

[`chat_with_context()`](https://davidrsch.github.io/rrlmgraph/dev/reference/chat_with_context.md)
assembles a token-budgeted context window from the project graph and
sends it to an LLM. This vignette shows real model responses captured
from a live run — no API key is required to read the vignette.

## How it works

``` r
library(rrlmgraph)

graph <- build_rrlm_graph(system.file("extdata", "demo", package = "rrlmgraph"))

# GitHub Models Marketplace — free, uses GITHUB_PAT in Actions
answer <- chat_with_context(
    graph,
    "How does the data preparation and validation pipeline work?",
    provider = "github",
    model    = "gpt-4o-mini"
)
cat(answer)
```

Under the hood
[`chat_with_context()`](https://davidrsch.github.io/rrlmgraph/dev/reference/chat_with_context.md):

1.  Calls
    [`query_context()`](https://davidrsch.github.io/rrlmgraph/dev/reference/query_context.md)
    to run relevance-guided BFS capped at `budget_tokens` (default 2
    000).
2.  Assembles a system prompt from the selected node summaries and
    source snippets.
3.  Sends the prompt + user query to the chosen provider via the
    **ellmer** package.
4.  Appends an entry to `.rrlmgraph/task_trace.jsonl` for future weight
    updates.

## Captured outputs

The responses below were generated once by the `precompute-vignettes`
GitHub Actions workflow and committed to the repository as
`vignettes/precomputed/llm_chat_outputs.rds`. They will not be
re-generated on every `R CMD check` or CRAN build.

``` r
# When a vignette is built, cwd = vignettes/ so this relative path works
# both during R CMD check and pkgdown site builds.
precomputed_path <- file.path("precomputed", "llm_chat_outputs.rds")
if (file.exists(precomputed_path)) {
    outputs <- readRDS(precomputed_path)
} else {
    outputs <- NULL
}
```

``` r
if (is.null(outputs)) {
    cat(
        "> **Note:** Precomputed outputs not yet available.",
        "Run the `precompute-vignettes` workflow to generate them.\n"
    )
} else {
    for (item in outputs) {
        cat("### Query\n\n")
        cat(item$query, "\n\n")
        cat("**Context nodes used:**", paste(item$context_nodes, collapse = ", "), "\n\n")
        cat("**Provider / model:**", item$provider, "/", item$model, "\n\n")
        cat("**Response:**\n\n")
        cat(item$response, "\n\n")
        cat("---\n\n")
    }
}
```

### Query

How does the data preparation and validation pipeline work?

**Context nodes used:** demo/predict::run_pipeline,
demo/data_prep::prepare_data, demo/data_prep::clean_data,
demo/model::fit_model, demo/model::tune_hyperparams,
demo/predict::predict_results, demo/predict::evaluate_predictions,
demo/data_prep::validate_inputs, demo/model::select_features

**Provider / model:** github / gpt-4o-mini

**Response:**

ERROR: HTTP 403 Forbidden. ℹ {“error”:{“code”:“no_access”,“message”:“No
access to model: /gpt-4o-mini”,“details”:“No access to model:
/gpt-4o-mini”}}

------------------------------------------------------------------------

### Query

Which function is called by fit_model() and what does it do?

**Context nodes used:** demo/predict::run_pipeline,
demo/predict::predict_results, demo/data_prep::clean_data,
demo/model::fit_model, demo/data_prep::prepare_data,
demo/data_prep::validate_inputs, demo/model::select_features,
demo/model::tune_hyperparams, demo/predict::evaluate_predictions

**Provider / model:** github / gpt-4o-mini

**Response:**

ERROR: HTTP 403 Forbidden. ℹ {“error”:{“code”:“no_access”,“message”:“No
access to model: /gpt-4o-mini”,“details”:“No access to model:
/gpt-4o-mini”}}

------------------------------------------------------------------------

### Query

What are the main entry points of the package?

**Context nodes used:** demo/predict::run_pipeline,
demo/data_prep::prepare_data, demo/data_prep::clean_data,
demo/predict::predict_results, demo/data_prep::validate_inputs,
demo/predict::evaluate_predictions, demo/model::fit_model,
demo/model::tune_hyperparams, demo/model::select_features

**Provider / model:** github / gpt-4o-mini

**Response:**

ERROR: HTTP 403 Forbidden. ℹ {“error”:{“code”:“no_access”,“message”:“No
access to model: /gpt-4o-mini”,“details”:“No access to model:
/gpt-4o-mini”}}

------------------------------------------------------------------------

## Regenerating outputs

Trigger the **precompute-vignettes** workflow from [GitHub
Actions](https://github.com/davidrsch/rrlmgraph/actions/workflows/precompute-vignettes.yml)
to refresh `llm_chat_outputs.rds` and redeploy the pkgdown site.
