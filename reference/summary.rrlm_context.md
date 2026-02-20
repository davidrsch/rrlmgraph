# Summarise an rrlm_context object

Summarise an rrlm_context object

## Usage

``` r
# S3 method for class 'rrlm_context'
summary(object, ...)
```

## Arguments

- object:

  An `rrlm_context` object.

- ...:

  Ignored.

## Value

`object`, invisibly.

## See also

[`query_context()`](https://davidrsch.github.io/rrlmgraph/reference/query_context.md),
[`print.rrlm_context()`](https://davidrsch.github.io/rrlmgraph/reference/print.rrlm_context.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g   <- build_rrlm_graph("mypkg")
ctx <- query_context(g, "load data")
summary(ctx)
} # }
```
