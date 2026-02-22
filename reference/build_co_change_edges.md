# Build CO_CHANGES edges from git commit history

Scans the git log and identifies R source files that were modified
together in the same commit. Function nodes in those files receive
bidirectional `CO_CHANGES` edges with weight proportional to how often
the files change together (capped at `1.0`).

## Usage

``` r
build_co_change_edges(func_nodes, project_root = ".", min_cochanges = 2L)
```

## Arguments

- func_nodes:

  A list of node records from
  [`extract_function_nodes()`](https://davidrsch.github.io/rrlmgraph/reference/extract_function_nodes.md).

- project_root:

  Character(1). Root of the git repository. Defaults to `"."`.

- min_cochanges:

  Integer(1). Minimum number of co-change commits required to create an
  edge. Default `2L`.

## Value

A `data.frame` with columns `from`, `to`, `weight`. Zero rows when no
qualifying pairs are found.

## Details

Requires that `project_root` is inside a git repository. Returns zero
rows silently when git is unavailable or the project has no commit
history with co-changed R files.

## See also

[`build_call_edges()`](https://davidrsch.github.io/rrlmgraph/reference/build_call_edges.md),
[`build_rrlm_graph()`](https://davidrsch.github.io/rrlmgraph/reference/build_rrlm_graph.md)

## Examples

``` r
if (FALSE) { # \dontrun{
proj  <- detect_rproject("/path/to/mypkg")
nodes <- extract_function_nodes(proj$r_files)
edges <- build_co_change_edges(nodes, proj$root)
} # }
```
