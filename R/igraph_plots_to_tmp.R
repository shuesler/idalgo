#' importFrom("grDevices", "dev.off", "jpeg")
#' importFrom("graphics", "plot")

#' @export
igraph_plots_to_tmp <- function(igraphs, lay = NULL, open = FALSE) {
  platzhalter_namen_igraphs <<- paste0("graph", seq_along(igraphs))
  plotliste <- list()


  for (i in seq_along(igraphs)) {
    tmp            <- tempfile(fileext = ".jpeg")
    jpeg(tmp)
    set.seed(1)
    graph          <- igraphs[[i]]
    tmp99          <- matrix_kontroller(lay[names(V(graph)),])
    plot(graph, layout = tmp99)
    dev.off()

    plotliste[[i]] <- tmp
  }

  if (open) {
    fold <- plotliste %>% str_split("file") %>% `[[`(1) %>% `[[`(1)
    shell.exec(fold)
  }


  plotliste %>% purrr::as_vector()
}
