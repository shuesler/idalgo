#' importFrom("grDevices", "dev.off", "jpeg")
#' importFrom("graphics", "plot")

#' @export
igraph_plots_to_tmp <- function(igraphs, open = FALSE) {

  plotliste <- list()

  for (i in seq_along(igraphs)) {
    tmp            <- tempfile(fileext = ".jpeg")
    jpeg(tmp)
    set.seed(1)
    plot( igraphs[[i]])
    dev.off()

    plotliste[[i]] <- tmp
  }

  if (open) {
    fold <- plotliste %>% stringr::str_split("file") %>% `[[`(1) %>% `[[`(1)
    shell.exec(fold)
  }


  plotliste %>% purrr::as_vector()
}
