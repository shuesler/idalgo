#' @export
or_filter <- function(x,y) {

  paste0(x, " == ", glue::single_quote(y))

}
