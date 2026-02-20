# Embed a free-text query into the model's vector space

Projects `query` into the same vector space produced by
[`embed_nodes()`](https://davidrsch.github.io/rrlmgraph/reference/embed_nodes.md),
using the vocabulary and TF-IDF transformer stored in `model`. For the
`"ollama"` and `"openai"` backends the same API call is made for the
query as for the document nodes.

## Usage

``` r
embed_query(query, model, method = c("tfidf", "ollama", "openai"))
```

## Arguments

- query:

  Character(1). The free-text query string.

- model:

  The `model` element returned by
  [`embed_nodes()`](https://davidrsch.github.io/rrlmgraph/reference/embed_nodes.md).

- method:

  Character(1). Must match the method used when building the model. One
  of `"tfidf"`, `"ollama"`, or `"openai"`.

## Value

A `numeric` vector in the same vector space as the stored node
embeddings. Returns a zero-length numeric if `model` is `NULL`.

## See also

[`embed_nodes()`](https://davidrsch.github.io/rrlmgraph/reference/embed_nodes.md),
[`cosine_similarity()`](https://davidrsch.github.io/rrlmgraph/reference/cosine_similarity.md)

## Examples

``` r
if (FALSE) { # \dontrun{
result <- embed_nodes(nodes)
qvec   <- embed_query("how are models evaluated?", result$model)
} # }
```
