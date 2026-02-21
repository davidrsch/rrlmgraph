# R/imports.R
# Centralised @importFrom declarations so that devtools::document()
# keeps them in NAMESPACE.  All runtime calls in the package use the
# pkg::fun() qualified form, but these declarations are still needed so
# that R CMD check does not complain about "package used but not imported".

#' @importFrom cli cli_abort cli_bullets cli_h1 cli_h2 cli_inform cli_text cli_warn
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbWriteTable
#' @importFrom RSQLite SQLite
#' @importFrom igraph E V add_edges add_vertices delete_vertices as_data_frame
#' @importFrom igraph ecount ends graph_attr "graph_attr<-" graph_from_data_frame
#' @importFrom DiagrammeR grViz
#' @importFrom igraph incident induced_subgraph make_empty_graph neighborhood neighbors page_rank
#' @importFrom igraph vcount vertex_attr
#' @importFrom tools file_ext
#' @importFrom codetools findGlobals
#' @importFrom fs dir_create dir_exists dir_ls file_exists path path_abs path_ext_remove path_file
#' @importFrom stats complete.cases runif setNames
#' @importFrom text2vec TfIdf create_dtm create_vocabulary
NULL
