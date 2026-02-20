# Compute cosine similarity between two numeric vectors

Compute cosine similarity between two numeric vectors

## Usage

``` r
cosine_similarity(a, b)
```

## Arguments

- a:

  Numeric vector.

- b:

  Numeric vector of the same length as `a`.

## Value

Numeric(1) in `[-1, 1]`. Returns `0` if either vector has zero norm (to
avoid `NaN`).

## See also

[`embed_nodes()`](https://davidrsch.github.io/rrlmgraph/reference/embed_nodes.md),
[`embed_query()`](https://davidrsch.github.io/rrlmgraph/reference/embed_query.md)

## Examples

``` r
cosine_similarity(c(1, 0, 0), c(1, 0, 0))  # 1
#> [1] 1
cosine_similarity(c(1, 0, 0), c(0, 1, 0))  # 0
#> [1] 0
```
