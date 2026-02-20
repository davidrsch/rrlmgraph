# Check whether Ollama is available

Returns `TRUE` if the local Ollama daemon is reachable (i.e. responds to
a `GET /api/tags` request within a short timeout). This is used
internally to decide whether to fall back to TF-IDF when the caller
requests `method = "ollama"`.

## Usage

``` r
ollama_available()
```

## Value

Logical(1).

## Configuration

Set the `OLLAMA_BASE_URL` environment variable to override the default
endpoint (`http://localhost:11434`).

## See also

[`embed_nodes()`](https://davidrsch.github.io/rrlmgraph/reference/embed_nodes.md),
[`embed_query()`](https://davidrsch.github.io/rrlmgraph/reference/embed_query.md)

## Examples

``` r
if (FALSE) { # \dontrun{
if (ollama_available()) {
  result <- embed_nodes(nodes, method = "ollama")
}
} # }
```
