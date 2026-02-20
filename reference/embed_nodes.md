# Embed function nodes with TF-IDF, Ollama, or OpenAI vectors

Builds embeddings from the concatenated text of all nodes (`signature`,
`body_text`, `roxygen_text`). Three backends are supported:

## Usage

``` r
embed_nodes(
  func_nodes,
  method = c("tfidf", "ollama", "openai"),
  min_term_count = 1L,
  min_doc_count = 1L,
  existing_corpus = NULL,
  verbose = FALSE,
  cache_dir = NULL
)
```

## Arguments

- func_nodes:

  A list of node records as returned by
  [`extract_function_nodes()`](https://davidrsch.github.io/rrlmgraph/reference/extract_function_nodes.md).
  Each record must have `node_id`, `signature`, `body_text`, and
  `roxygen_text` fields.

- method:

  Character(1). One of `"tfidf"`, `"ollama"`, or `"openai"`. Default
  `"tfidf"`.

- min_term_count:

  Integer(1). TF-IDF vocabulary pruning: drop terms appearing fewer than
  this many times. Default `1L`.

- min_doc_count:

  Integer(1). TF-IDF vocabulary pruning: drop terms in fewer than this
  many documents. Default `1L`.

- existing_corpus:

  Optional character vector of additional document texts to include when
  fitting the TF-IDF vocabulary. This keeps incremental updates in the
  same vector space. Ignored for `"ollama"` and `"openai"` backends.

- verbose:

  Logical(1). Emit progress messages. For the `"openai"` backend also
  prints an estimated cost. Default `FALSE`.

- cache_dir:

  Character(1) or `NULL`. Directory used to store embedding caches for
  the `ollama` and `openai` backends. `NULL` (default) uses the
  `.rrlmgraph/` folder under the working directory.

## Value

A named list:

- `embeddings`:

  Named list of `numeric` vectors, one per node.

- `model`:

  Embedding model for use with
  [`embed_query()`](https://davidrsch.github.io/rrlmgraph/reference/embed_query.md).

- `matrix`:

  Optional numeric matrix (rows = nodes) for backends that return dense
  vectors. `NULL` for TF-IDF.

## Details

- `tfidf`:

  Sparse TF-IDF vectors (default, no API required).

- `ollama`:

  768-dim dense vectors via the local Ollama daemon
  (`nomic-embed-text`). Falls back to TF-IDF when Ollama is unavailable.

- `openai`:

  1536-dim dense vectors via the OpenAI `text-embedding-3-small` model.
  Requires the `OPENAI_API_KEY` environment variable.

## See also

[`embed_query()`](https://davidrsch.github.io/rrlmgraph/reference/embed_query.md),
[`cosine_similarity()`](https://davidrsch.github.io/rrlmgraph/reference/cosine_similarity.md),
[`ollama_available()`](https://davidrsch.github.io/rrlmgraph/reference/ollama_available.md)

## Examples

``` r
if (FALSE) { # \dontrun{
proj   <- detect_rproject("/path/to/mypkg")
nodes  <- extract_function_nodes(proj$r_files)
result <- embed_nodes(nodes)                          # TF-IDF
result <- embed_nodes(nodes, method = "ollama")      # Ollama
result <- embed_nodes(nodes, method = "openai")      # OpenAI
} # }
```
