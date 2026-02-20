# Chat with an LLM using graph-derived context

Retrieves a token-budgeted context window from the graph via
[`query_context()`](https://davidrsch.github.io/rrlmgraph/reference/query_context.md),
builds a grounded system prompt, and sends a message to an LLM. Uses
ellmer when installed (supporting multiple providers); falls back to a
direct httr2 call for the `"openai"` provider when ellmer is absent.

## Usage

``` r
chat_with_context(
  graph,
  message,
  provider = c("openai", "ollama", "github", "anthropic"),
  model = NULL,
  budget_tokens = 6000L,
  seed_node = NULL,
  min_relevance = 0.1,
  ...
)
```

## Arguments

- graph:

  An `rrlm_graph` / `igraph` object.

- message:

  Character(1). User message / question.

- provider:

  Character(1). LLM provider. One of `"openai"` (default), `"ollama"`,
  `"github"`, `"anthropic"`.

- model:

  Character(1) or `NULL`. Model identifier. When `NULL` (default) a
  sensible per-provider default is used: `"gpt-4o-mini"`
  (openai/github), `"llama3.2"` (ollama), `"claude-3-5-haiku-latest"`
  (anthropic).

- budget_tokens:

  Integer(1). Context token budget passed to
  [`query_context()`](https://davidrsch.github.io/rrlmgraph/reference/query_context.md).
  Default `6000L`.

- seed_node:

  Character(1) or `NULL`. Forwarded to
  [`query_context()`](https://davidrsch.github.io/rrlmgraph/reference/query_context.md).

- min_relevance:

  Numeric(1). Forwarded to
  [`query_context()`](https://davidrsch.github.io/rrlmgraph/reference/query_context.md).

- ...:

  Additional arguments forwarded to the ellmer `chat_*()` constructor
  (e.g. `base_url` for ollama).

## Value

Character(1) containing the LLM response text. Returns a descriptive
error string (prefixed `"[rrlmgraph error]"`) rather than throwing when
the LLM call fails.

## System prompt structure

The system prompt combines:

1.  The assembled context string (code snippets relevant to the query).

2.  A grounding constraint block that instructs the model to answer only
    from the provided context and to cite node names.

## Authentication

- `"openai"`:

  Set `OPENAI_API_KEY` environment variable.

- `"ollama"`:

  No key required (local daemon). Set `OLLAMA_BASE_URL` to override the
  default `http://localhost:11434`.

- `"github"`:

  Set `GITHUB_PAT` environment variable. Requires access to the GitHub
  Models Marketplace (<https://github.com/marketplace/models>).

- `"anthropic"`:

  Set `ANTHROPIC_API_KEY` environment variable.

## See also

[`query_context()`](https://davidrsch.github.io/rrlmgraph/reference/query_context.md),
[`update_task_weights()`](https://davidrsch.github.io/rrlmgraph/reference/update_task_weights.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- build_rrlm_graph("mypkg")
# OpenAI (default) -- requires OPENAI_API_KEY
chat_with_context(g, "How does data_prep() work?")
# GitHub Models Marketplace -- requires GITHUB_PAT
chat_with_context(g, "How does data_prep() work?", provider = "github")
# Local Ollama
chat_with_context(g, "How does data_prep() work?",
  provider = "ollama", model = "llama3.2")
} # }
```
