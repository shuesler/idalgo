#' @export
norma_layout <- function(x) {

  set.graph.attribute(x, "layout", norma(get.graph.attribute(x, "layout")))

}

#' @export
norma <- function(x) {

  alles <- c(x[,1], x[,2])
  MEAN  <- mean(alles)
  SD    <- sd(alles)

  x[,1] <- (x[,1]-MEAN)/SD
  x[,2] <- (x[,2]-MEAN)/SD

  x
}
