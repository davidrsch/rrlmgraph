# Security Policy

## Supported Versions

Only the current development version of **rrlmgraph** receives security
fixes. Once the package reaches a stable CRAN release that version will
also be supported.

| Version          | Supported |
|------------------|-----------|
| 0.1.4.9000 (dev) | ✓         |
| \< 0.1.4         | ✗         |

## Threat Model — sandbox / REPL environment

`rrlmgraph` builds a knowledge graph of an R project and exposes a
[`chat_with_context()`](https://davidrsch.github.io/rrlmgraph/dev/reference/chat_with_context.md)
interface that passes retrieved code snippets as context to an LLM. The
relevant threat-model notes are:

- **R code is never evaluated from LLM output.** The package only
  *reads* and *indexes* source files; it does not
  [`eval()`](https://rdrr.io/r/base/eval.html) or
  [`source()`](https://rdrr.io/r/base/source.html) any LLM-generated
  text.
- **Bounded stdout.** When
  [`log_task_trace()`](https://davidrsch.github.io/rrlmgraph/dev/reference/log_task_trace.md)
  is used, output is written to a local SQLite file. No network egress
  of trace data occurs within the package itself.
- **External API calls.** Embedding and chat calls are sent to provider
  APIs (OpenAI, Ollama, GitHub Models, Anthropic) using credentials
  supplied by the user. Credentials are never stored by the package.

## PATH Safety

`rrlmgraph` spawns no external processes, but calling code
(e.g. `Rscript`) must come from a trusted location. Users should ensure
their R installation directory appears in `PATH` before any
world-writable directories to prevent binary-hijacking attacks.

## Reporting a Vulnerability

Please use **GitHub private vulnerability reporting** to disclose
security issues:

1.  Navigate to the [Security
    tab](https://github.com/davidrsch/rrlmgraph/security) of this
    repository.
2.  Click **“Report a vulnerability”**.
3.  Provide a description, reproduction steps, and potential impact.

You can expect an initial response within **7 days**. Please do not open
a public issue for security vulnerabilities.
