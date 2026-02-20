# Print an rrlm_context object

Print an rrlm_context object

## Usage

``` r
# S3 method for class 'rrlm_context'
print(x, ...)
```

## Arguments

- x:

  An `rrlm_context` object.

- ...:

  Ignored.

## Value

`x`, invisibly.

## See also

[`query_context()`](https://davidrsch.github.io/rrlmgraph/reference/query_context.md),
[`summary.rrlm_context()`](https://davidrsch.github.io/rrlmgraph/reference/summary.rrlm_context.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g   <- build_rrlm_graph("mypkg")
ctx <- query_context(g, "load data")
print(ctx)
} # }
```
