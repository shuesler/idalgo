#' @export
geparste_liste <- function(x) {

  #x                         <- rlang::enquo(x)
  igraph_addr               <- list_parser(x, "IGRAPH")
  platzhalter_namen_igraphs <- paste0("graph", seq_along(igraph_addr))
  expr_erw                  <- purrr::map2(igraph_addr, platzhalter_namen_igraphs, ~ rlang::expr(`<-`(!!.x, !!.y)))
  igraph_addr_eval          <- igraph_addr %>% purrr::map(rlang::eval_bare)
  NODES                     <- names(V(igraph_addr_eval[[1]]))
  list(igraph_addr = igraph_addr, platzhalter_namen_igraphs = platzhalter_namen_igraphs, expr_erw = expr_erw, igraph_addr_eval = igraph_addr_eval, NODES = NODES)

}
