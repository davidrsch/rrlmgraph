# R/llm_interface.R
# LLM chat integration using rrlm graph context.
# Covers rrlmgraph issue #13 (Sprint 2).

# ---- chat_with_context -----------------------------------------------

#' Chat with an LLM using graph-derived context
#'
#' Retrieves a token-budgeted context window from the graph via
#' \code{query_context()}, builds a grounded system prompt, and sends a
#' message to an LLM.  Uses \pkg{ellmer} when installed (supporting
#' multiple providers); falls back to a direct \pkg{httr2} call for
#' the \code{"openai"} provider when \pkg{ellmer} is absent.
#'
#' @section System prompt structure:
#' The system prompt combines:
#' \enumerate{
#'   \item The assembled context string (code snippets relevant to the query).
#'   \item A grounding constraint block that instructs the model to answer
#'         only from the provided context and to cite node names.
#' }
#'
#' @section Authentication:
#' \describe{
#'   \item{\code{"openai"}}{Set \code{OPENAI_API_KEY} environment variable.}
#'   \item{\code{"ollama"}}{No key required (local daemon).  Set
#'     \code{OLLAMA_BASE_URL} to override the default
#'     \code{http://localhost:11434}.}
#'   \item{\code{"github"}}{Set \code{GITHUB_PAT} environment variable.
#'     Requires access to the GitHub Models Marketplace
#'     (\url{https://github.com/marketplace/models}).}
#'   \item{\code{"anthropic"}}{Set \code{ANTHROPIC_API_KEY} environment variable.}
#' }
#'
#' @param graph An \code{rrlm_graph} / \code{igraph} object.
#' @param message Character(1).  User message / question.
#' @param provider Character(1).  LLM provider.  One of \code{"openai"}
#'   (default), \code{"ollama"}, \code{"github"}, \code{"anthropic"}.
#' @param model Character(1) or \code{NULL}.  Model identifier.  When
#'   \code{NULL} (default) a sensible per-provider default is used:
#'   \code{"gpt-4o-mini"} (openai/github), \code{"llama3.2"} (ollama),
#'   \code{"claude-3-5-haiku-latest"} (anthropic).
#' @param budget_tokens Integer(1).  Context token budget passed to
#'   \code{query_context()}.  Default \code{6000L}.
#' @param seed_node Character(1) or \code{NULL}.  Forwarded to
#'   \code{query_context()}.
#' @param min_relevance Numeric(1).  Forwarded to
#'   \code{query_context()}.
#' @param ... Additional arguments forwarded to the \pkg{ellmer}
#'   \code{chat_*()} constructor (e.g. \code{base_url} for ollama).
#'
#' @return Character(1) containing the LLM response text.  Returns a
#'   descriptive error string (prefixed \code{"[rrlmgraph error]"}) rather
#'   than throwing when the LLM call fails.
#'
#' @seealso [query_context()], [update_task_weights()]
#' @export
#' @examples
#' \dontrun{
#' g <- build_rrlm_graph("mypkg")
#' # OpenAI (default) -- requires OPENAI_API_KEY
#' chat_with_context(g, "How does data_prep() work?")
#' # GitHub Models Marketplace -- requires GITHUB_PAT
#' chat_with_context(g, "How does data_prep() work?", provider = "github")
#' # Local Ollama
#' chat_with_context(g, "How does data_prep() work?",
#'   provider = "ollama", model = "llama3.2")
#' }
chat_with_context <- function(
  graph,
  message,
  provider = c("openai", "ollama", "github", "anthropic"),
  model = NULL,
  budget_tokens = 6000L,
  seed_node = NULL,
  min_relevance = 0.1,
  ...
) {
  provider <- match.arg(provider)
  if (!inherits(graph, "igraph")) {
    cli::cli_abort("{.arg graph} must be an igraph / rrlm_graph object.")
  }
  if (!is.character(message) || length(message) != 1L) {
    cli::cli_abort("{.arg message} must be a single character string.")
  }

  # ---- 1. Build context -----------------------------------------------
  ctx <- query_context(
    graph = graph,
    query = message,
    seed_node = seed_node,
    budget_tokens = as.integer(budget_tokens),
    min_relevance = min_relevance
  )

  # ---- 2. Build system prompt -----------------------------------------
  system_prompt <- .build_system_prompt(ctx$context_string)

  # ---- 3. Validate provider availability ------------------------------
  if (!requireNamespace("ellmer", quietly = TRUE) && provider != "openai") {
    cli::cli_abort(
      c(
        "{.pkg ellmer} is required for provider {.val {provider}}.",
        "i" = "Install it with {.code install.packages('ellmer')}."
      )
    )
  }

  # ---- 4. Send to LLM -------------------------------------------------
  response_text <- tryCatch(
    {
      if (requireNamespace("ellmer", quietly = TRUE)) {
        .llm_via_ellmer(system_prompt, message, provider, model, ...)
      } else {
        .llm_via_httr2(
          system_prompt,
          message,
          if (is.null(model)) "gpt-4o-mini" else model
        )
      }
    },
    error = function(e) {
      cli::cli_warn("LLM call failed: {conditionMessage(e)}")
      paste0("[rrlmgraph error] ", conditionMessage(e))
    }
  )

  # ---- 5. Log task completion -----------------------------------------
  .log_task_completion(
    graph = graph,
    query = message,
    nodes = ctx$nodes,
    response = response_text
  )

  response_text
}

# ---- Internal helpers ------------------------------------------------

#' @keywords internal
.build_system_prompt <- function(context_string) {
  grounding <- paste(
    "You are a code-assistant that answers questions about an R project.",
    "RULES:",
    "1. Base your answer ONLY on the code context provided below.",
    "2. If the context does not contain enough information, say so explicitly.",
    "3. When referencing a function or file from the context, cite its name.",
    "4. Do not invent function signatures or behaviour not shown in the context.",
    sep = "\n"
  )

  if (nchar(trimws(context_string)) == 0L) {
    return(grounding)
  }

  paste0(
    grounding,
    "\n\n--- BEGIN CODE CONTEXT ---\n",
    context_string,
    "\n--- END CODE CONTEXT ---"
  )
}

#' @keywords internal
.llm_via_ellmer <- function(system_prompt, message, provider, model, ...) {
  default_models <- c(
    openai = "gpt-4o-mini",
    ollama = "llama3.2",
    github = "gpt-4o-mini",
    anthropic = "claude-3-5-haiku-latest"
  )
  resolved_model <- if (!is.null(model)) model else default_models[[provider]]

  chat_fn_name <- switch(
    provider,
    openai = "chat_openai",
    ollama = "chat_ollama",
    github = "chat_github",
    anthropic = "chat_anthropic"
  )
  chat_fn <- getExportedValue("ellmer", chat_fn_name)
  chat <- chat_fn(system_prompt = system_prompt, model = resolved_model, ...)
  chat$chat(message)
}

#' @keywords internal
.llm_via_httr2 <- function(system_prompt, message, model) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    cli::cli_abort(
      "Neither {.pkg ellmer} nor {.pkg httr2} is installed. ",
      "Install one to use {.fn chat_with_context}."
    )
  }

  api_key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  if (nchar(api_key) == 0L) {
    cli::cli_abort(
      "OPENAI_API_KEY is not set. ",
      "Call {.code Sys.setenv(OPENAI_API_KEY = 'sk-...')}."
    )
  }

  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = message)
    )
  )

  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)
  resp_obj <- httr2::resp_body_json(resp)

  if (!is.null(resp_obj$error)) {
    cli::cli_abort("OpenAI API error: {resp_obj$error$message}")
  }

  resp_obj$choices[[1L]]$message$content
}

#' @keywords internal
.log_task_completion <- function(graph, query, nodes, response) {
  project_root <- tryCatch(
    igraph::graph_attr(graph, "project_root"),
    error = function(e) NULL
  )
  if (
    is.null(project_root) || is.na(project_root) || !nchar(trimws(project_root))
  ) {
    return(invisible(NULL))
  }

  trace_dir <- file.path(project_root, ".rrlmgraph")
  trace_file <- file.path(trace_dir, "task_trace.jsonl")

  dir.create(trace_dir, showWarnings = FALSE, recursive = TRUE)

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(invisible(NULL))
  }

  entry <- jsonlite::toJSON(
    list(
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      query = query,
      nodes = as.list(nodes),
      response = substr(response, 1L, 500L),
      session_id = .session_id()
    ),
    auto_unbox = TRUE
  )

  cat(entry, "\n", file = trace_file, append = TRUE, sep = "")
  invisible(NULL)
}

#' @keywords internal
.session_id <- function() {
  id_env <- Sys.getenv("RRLMGRAPH_SESSION_ID", unset = "")
  if (nchar(id_env) > 0L) {
    return(id_env)
  }
  format(proc.time()[["elapsed"]], digits = 10L)
}
