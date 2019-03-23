#' @export
mark_U <- function(x) {

  x <- set_edge_attr(x, "curved", E(x), 0)
  x <- set_edge_attr(x, "curved", E(x)[is_edge_U(x)], .2)

  x <- set_edge_attr(x, "color", E(x), "black")
  x <- set_edge_attr(x, "color", E(x)[is_edge_U(x)], "red")

  x

}
