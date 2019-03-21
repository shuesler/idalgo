make_arg_tibble <- function(plotvec, path = "~/graphen/") {

  tibble::tibble(
    media = plotvec,
    path  = rep(path, length(plotvec)),
    name  = plotl %>%
      purrr::map(~ stringr::str_split(.x, "\\\\") %>%
            purrr::as_vector()) %>% purrr::map_chr(~ tail(.x, 1))
  )

}
