# R/llm_interface.R
# LLM chat integration using rrlm graph context.
# Covers rrlmgraph issue #13 (Sprint 2).

# ---- chat_with_context -----------------------------------------------

#' Chat with an LLM using graph-derived context
#'
#' Retrieves a token-budgeted context window from the graph via
#' \code{query_context()}, builds a grounded system prompt, and sends a
#' message to an LLM.  Requires \pkg{ellmer}, which handles provider
#' connections and the tool-call loop that implements RLM-Graph traversal.
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
#' @return Character(1) containing the LLM response text.  Throws on error.
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

  # ---- RLM-Graph tool-calling (ellmer) / httr2 fallback (OpenAI only) ------
  # When ellmer is available the LLM drives graph traversal via tool calls
  # (search_nodes, get_node_info, find_callers, find_callees).  When ellmer is
  # absent, an OpenAI-compatible httr2 path is used with a context-window
  # prompt; other providers require ellmer and will error.
  if (requireNamespace("ellmer", quietly = TRUE)) {
    graph_tools <- .build_graph_tools(graph, budget_tokens, min_relevance)
    system_prompt <- .build_rlm_system_prompt()
    response_text <- .llm_via_ellmer(
      system_prompt,
      message,
      provider,
      model,
      graph_tools,
      ...
    )
  } else if (provider == "openai") {
    ctx_obj <- tryCatch(
      query_context(
        graph,
        message,
        budget_tokens = budget_tokens,
        seed_node = seed_node,
        min_relevance = min_relevance
      ),
      error = function(e) NULL
    )
    ctx_str <- if (!is.null(ctx_obj)) {
      tryCatch(assemble_context_string(ctx_obj), error = function(e) "")
    } else {
      ""
    }
    system_prompt <- .build_system_prompt(ctx_str)
    resolved_model <- if (!is.null(model)) model else "gpt-4o-mini"
    response_text <- .llm_via_httr2(system_prompt, message, resolved_model)
  } else {
    cli::cli_abort(c(
      "{.pkg ellmer} is required for {.fn chat_with_context} with provider {.val {provider}}.",
      "i" = "Install it with: {.code install.packages(\"ellmer\")}"
    ))
  }

  .log_task_completion(graph, message, character(0L), response_text)
  response_text
}

# ---- Internal helpers ------------------------------------------------

#' @keywords internal
.build_system_prompt <- function(context) {
  ctx_trimmed <- trimws(if (is.null(context)) "" else context)
  grounding <- paste(
    "RULES:",
    "1. Answer based ONLY on the code context provided.",
    "2. Do not fabricate function signatures or behaviour not in the context.",
    "3. Cite exact node names where relevant.",
    sep = "\n"
  )
  if (nchar(ctx_trimmed) == 0L) {
    return(grounding)
  }
  paste(
    grounding,
    "",
    "---- BEGIN CODE CONTEXT ----",
    ctx_trimmed,
    "---- END CODE CONTEXT ----",
    sep = "\n"
  )
}

#' @keywords internal
.build_rlm_system_prompt <- function() {
  paste(
    "You are an expert R code assistant with access to a structured knowledge",
    "graph of an R project's codebase. Navigate this graph using the provided",
    "tools to gather relevant code before answering.",
    "",
    "Available tools:",
    "  search_nodes(query, limit)         -- find functions by keyword",
    "  get_node_info(node_name)           -- read a function's code and docs",
    "  find_callers(function_name, limit) -- find what calls a function",
    "  find_callees(function_name, limit) -- find what a function calls",
    "",
    "RULES:",
    "1. Use the tools to explore the graph until you have enough context.",
    "2. Base your final answer ONLY on code you retrieved from the graph.",
    "3. Cite exact node names where relevant.",
    "4. Do not invent function signatures or behaviour not shown in the graph.",
    sep = "\n"
  )
}

#' Build ellmer tool definitions that expose graph navigation to the LLM.
#'
#' The four tools give the LLM the same graph-traversal primitives available
#' in the MCP server, allowing it to iteratively explore call relationships and
#' read function bodies before producing its answer.  This is the RLM-Graph
#' execution mechanism: the LLM drives traversal, the R environment executes.
#' @keywords internal
.build_graph_tools <- function(graph, budget_tokens, min_relevance) {
  # ellmer 0.4.0: pass name= explicitly so tool objects are addressable by
  # the LLM's function-calling JSON.  Anonymous functions yield empty names.
  # ---- search_nodes -------------------------------------------------------
  search_tool <- ellmer::tool(
    fun = function(query, limit = 10L) {
      v_names <- igraph::V(graph)$name
      v_types <- igraph::vertex_attr(graph, "node_type")
      v_sigs <- igraph::vertex_attr(graph, "signature")
      v_bodies <- igraph::vertex_attr(graph, "body_text")
      limit <- min(as.integer(limit), 50L)
      terms <- grep(
        ".",
        strsplit(tolower(query), "[^a-z0-9]+", perl = TRUE)[[1L]],
        perl = TRUE,
        value = TRUE
      )
      if (!length(terms)) {
        return("No search terms provided.")
      }
      needle <- paste(terms, collapse = "|")
      scores <- vapply(
        seq_along(v_names),
        function(i) {
          haystack <- paste(
            tolower(v_names[[i]] %||% ""),
            tolower(v_sigs[[i]] %||% ""),
            tolower(v_bodies[[i]] %||% ""),
            sep = " "
          )
          sum(gregexpr(needle, haystack, perl = TRUE)[[1L]] > 0L)
        },
        integer(1L)
      )
      ord <- order(scores, decreasing = TRUE)
      top <- head(ord[scores[ord] > 0L], limit)
      if (!length(top)) {
        return("No matching nodes found.")
      }
      lines <- vapply(
        seq_along(top),
        function(i) {
          tp <- if (!is.na(v_types[top[[i]]])) v_types[top[[i]]] else "node"
          sprintf("%d. `%s` (%s)", i, v_names[top[[i]]], tp)
        },
        character(1L)
      )
      paste(lines, collapse = "\n")
    },
    description = "Search the code graph for relevant nodes by keyword. Returns a numbered list of matching functions and their types.",
    name = "search_nodes",
    query = ellmer::type_string(
      "Keywords to search across function names and code bodies"
    ),
    limit = ellmer::type_integer(
      "Maximum results to return (default 10, max 50)",
      required = FALSE
    )
  )

  # ---- get_node_info ------------------------------------------------------
  info_tool <- ellmer::tool(
    fun = function(node_name) {
      v_names <- igraph::V(graph)$name
      if (!node_name %in% v_names) {
        return(sprintf(
          "Node '%s' not found. Use search_nodes to find the correct name.",
          node_name
        ))
      }
      build_node_context(node_name, graph, mode = "full")
    },
    description = "Get the full source code, signature, and documentation for a specific named function node in the graph.",
    name = "get_node_info",
    node_name = ellmer::type_string(
      "Exact node name as returned by search_nodes"
    )
  )

  # ---- find_callers -------------------------------------------------------
  callers_tool <- ellmer::tool(
    fun = function(function_name, limit = 10L) {
      v_names <- igraph::V(graph)$name
      if (!function_name %in% v_names) {
        return(sprintf("Node '%s' not found.", function_name))
      }
      in_nbrs <- unique(igraph::V(graph)$name[
        igraph::neighbors(graph, function_name, mode = "in")
      ])
      in_nbrs <- in_nbrs[!is.na(in_nbrs)]
      if (!length(in_nbrs)) {
        return(sprintf("No callers found for '%s'.", function_name))
      }
      top <- head(in_nbrs, as.integer(limit))
      paste(sprintf("%d. `%s`", seq_along(top), top), collapse = "\n")
    },
    description = "Find functions that call the specified function (callers / upstream dependencies).",
    name = "find_callers",
    function_name = ellmer::type_string(
      "Exact node name of the function to find callers for"
    ),
    limit = ellmer::type_integer(
      "Maximum callers to return (default 10)",
      required = FALSE
    )
  )

  # ---- find_callees -------------------------------------------------------
  callees_tool <- ellmer::tool(
    fun = function(function_name, limit = 10L) {
      v_names <- igraph::V(graph)$name
      if (!function_name %in% v_names) {
        return(sprintf("Node '%s' not found.", function_name))
      }
      out_nbrs <- unique(igraph::V(graph)$name[
        igraph::neighbors(graph, function_name, mode = "out")
      ])
      out_nbrs <- out_nbrs[!is.na(out_nbrs)]
      if (!length(out_nbrs)) {
        return(sprintf("No callees found for '%s'.", function_name))
      }
      top <- head(out_nbrs, as.integer(limit))
      paste(sprintf("%d. `%s`", seq_along(top), top), collapse = "\n")
    },
    description = "Find functions called by the specified function (callees / downstream dependencies).",
    name = "find_callees",
    function_name = ellmer::type_string(
      "Exact node name of the function to find callees of"
    ),
    limit = ellmer::type_integer(
      "Maximum callees to return (default 10)",
      required = FALSE
    )
  )

  list(search_tool, info_tool, callers_tool, callees_tool)
}

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
