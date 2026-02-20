# Find function calls within an R function body

Uses
[`codetools::findGlobals()`](https://rdrr.io/pkg/codetools/man/findGlobals.html)
to enumerate all global symbol references in `fn_body`, then removes:

- Language primitives and operators.

- NSE false-positives: data-masked column names in tidyverse verbs,
  `.data$` pronouns, `{{ }}` injections, and
  [`rlang::enquo()`](https://rlang.r-lib.org/reference/enquo.html)/`sym()`
  args.

## Usage

``` r
find_calls_in_body(fn_body)
```

## Arguments

- fn_body:

  An R language object (the body expression of a function, i.e.\\
  `body(f)` or the third element of a parsed `function(...)` call).

## Value

A character vector of symbol names that represent real function calls,
sorted and de-duplicated. Returns `character(0)` on error.

## Details

For `pkg::fn` and `pkg:::fn` patterns the symbol is verified to exist as
a function in the package namespace before being included.

## See also

[`extract_function_nodes()`](https://davidrsch.github.io/rrlmgraph/reference/extract_function_nodes.md)

## Examples

``` r
fn <- function(x) sqrt(x) + log(x)
find_calls_in_body(body(fn))
#> character(0)
# [1] "log"  "sqrt"
```
