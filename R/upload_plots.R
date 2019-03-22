#' @export
upload_plots <- function(igra_addr_eval, platzh_namen_igraphs) {

  plotl                     <- igraph_plots_to_tmp(igra_addr_eval)

  tib                       <- make_arg_tibble(plotl)

  drive_plots               <-
    purrr::pmap(tib, uploader) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(url           = paste0("https://docs.google.com/uc?id=", id),
                  namen_igraphs = platzh_namen_igraphs)

  drive_plots

}
