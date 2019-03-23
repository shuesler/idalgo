#' @export
is_edge_U <- function(x) {

  tmp1 <- get.edge.attribute(x, "description")

  dplyr::case_when(tmp1 == "U" ~ TRUE,
                   tmp1 == ""  ~ FALSE,
                   TRUE        ~ FALSE)
}
