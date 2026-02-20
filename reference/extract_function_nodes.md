# Extract function nodes from R source files

Parses every file in `r_files`, walks its AST, and returns a list of
node records – one per named function definition discovered. Handles
standard `<-`/`=` assignments, S4 generics/methods/classes, and R6 class
methods. Files that cannot be parsed are skipped with a warning; the
function never throws.

## Usage

``` r
extract_function_nodes(r_files)
```

## Arguments

- r_files:

  Character vector of absolute paths to `.R` source files (typically the
  `r_files` element returned by
  [`detect_rproject()`](https://davidrsch.github.io/rrlmgraph/reference/detect_rproject.md)).

## Value

A list of node records. Each record is a named list with elements:

- `node_id`:

  `"<file_stem>::<fn_name>"` character(1).

- `name`:

  Function name, character(1).

- `file`:

  Absolute path of the source file, character(1).

- `line_start`:

  Integer: first line of the definition.

- `line_end`:

  Integer: last line of the definition.

- `signature`:

  Deparsed signature: `"name(arg1, arg2)"`, character(1).

- `body_text`:

  Full source text of the definition, character(1).

- `roxygen_text`:

  Adjacent Roxygen block (may be `""`), character(1).

- `complexity`:

  Integer: approximate cyclomatic complexity.

- `calls_list`:

  Character vector of called symbol names.

## See also

[`find_calls_in_body()`](https://davidrsch.github.io/rrlmgraph/reference/find_calls_in_body.md)

## Examples

``` r
if (FALSE) { # \dontrun{
proj  <- detect_rproject("/path/to/mypkg")
nodes <- extract_function_nodes(proj$r_files)
length(nodes)  # number of functions found
} # }
```
