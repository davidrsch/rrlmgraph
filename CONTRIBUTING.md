# Contributing to rrlmgraph

## Version bump discipline (closes rrlmgraph#144)

Version bumps must be made in a dedicated commit whose message includes
`Version bump to X.Y.Z`. Do **not** bundle a version bump into a refactoring
or feature commit — this makes the bump invisible in commit history.

The accompanying `NEWS.md` entry must:

1. Identify the specific fix or feature that caused the bump.
2. Reference the issue number(s) responsible.

Example good commit sequence:

```
fix: correct BFS direction in query_context() (#37)
chore: Version bump to 0.1.4 for BFS direction fix (#37)
```

Example bad pattern (do not do this):

```
refactor: Refactor graph traversal internals  <- version bump buried here
```

## Pull-request checklist

- Run `devtools::check()` locally before pushing.
- Update `NEWS.md` under the development version heading.
- Verify that `vignettes/precomputed/` files are up to date if you changed
  `R/` or `inst/extdata/` (the `precompute-vignettes` workflow runs
  automatically on push to `main` for those paths).
