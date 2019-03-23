#' @export
mat_from_gml <- function(gml) {

  mat_tmp <- as.matrix(data.frame(x=gml$coord[[1]], y=gml$coord[[2]]))
  row.names(mat_tmp) <- as.character(gml$coord[["L1"]])
  mat_tmp

}

#' @export
mat_update <- function(x) {

  tmp_lay <- as.data.frame(igraph::get.graph.attribute(x, "layout"))
  tmp_nam <- igraph::get.vertex.attribute(x, "name")
  x       <- igraph::delete_graph_attr(x, "layout")
  x       <- igraph::set_graph_attr(x, "layout", as.matrix(tmp_lay[tmp_nam,]))
  x

}
