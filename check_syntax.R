base <- "f:/APPs/Projects/rrlmgraph/rrlmgraph/repos/rrlmgraph"
files <- file.path(
  base,
  c("R/cache.R", "R/graph_build.R", "R/graph_traverse.R")
)
for (f in files) {
  result <- tryCatch(
    {
      parse(f, keep.source = FALSE)
      paste(basename(f), ": OK")
    },
    error = function(e) {
      paste(basename(f), ": ERROR -", conditionMessage(e))
    }
  )
  cat(result, "\n")
}
