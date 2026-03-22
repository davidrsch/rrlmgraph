#' @keywords internal
.llm_via_httr2 <- function(system_prompt, message, model) {
  api_key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  if (!nchar(api_key)) {
    cli::cli_abort(
      "OPENAI_API_KEY environment variable is not set or is empty."
    )
  }
  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model,
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = message)
      )
    ))
  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp)
  if (!is.null(body$error)) {
    cli::cli_abort("OpenAI API error: {body$error$message}")
  }
  body$choices[[1L]]$message$content
}

#' @keywords internal
.llm_via_ellmer <- function(
  system_prompt,
  message,
  provider,
  model,
  tools,
  ...
) {
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
  # Register each graph navigation tool.  ellmer executes tool calls, appends
  # results as tool messages, and re-sends to the model until it returns a
  # plain text response.  This loop IS the RLM-Graph execution mechanism.
  for (tool_def in tools) {
    chat$register_tool(tool_def)
  }
  chat$chat(message)
}

# Alias kept for back-compat; new name is .llm_via_ellmer.
.llm_via_ellmer_rlm <- .llm_via_ellmer

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
