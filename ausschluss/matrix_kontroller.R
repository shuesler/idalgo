#' @export
matrix_kontroller <- function(x) {
  if (is.null(x)) {
    NULL
  } else {
    if (!is.matrix(x)) {
      matrix(as.numeric(x), ncol = 2)
    } else {
      x
    }
  }
}
