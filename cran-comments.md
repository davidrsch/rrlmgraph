# cran-comments.md

## R CMD check results

0 errors | 0 warnings | 0 notes

(Or describe any NOTEs here, e.g.:)

> Possibly misspelled words in DESCRIPTION: rrlm (3 occurrences)
> -- "rrlm" is the intentional package acronym.

## Test environments

- local: Windows 10, R 4.5.2
- GitHub Actions: ubuntu-latest (R 4.4, R 4.5), windows-latest (R 4.5),
  macos-latest (R 4.5)
- win-builder: R-devel

## Reverse dependencies

This is a new package with no reverse dependencies.

## Comments

- The `httr2`, `ollamar`, and `ellmer` packages are optional back-ends for
  embedding and LLM chat; the package functions correctly with only
  base R and the declared `Imports` when those packages are absent.
- No internet access is required during tests; external API calls are
  guarded by `requireNamespace()` and `skip_if_not_installed()`.
