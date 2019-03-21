list_parser <- function(x, y) {

  x      <- rlang::enquo(x)
  x_char <- rlang::quo_name(x)

  tmp    <- tempfile(fileext = ".txt")

  sink(tmp)
  print(rlang::eval_tidy(x))
  sink()

  txt    <- readr::read_lines(tmp)
  tmp2   <- stringr::str_detect(txt, y)
  tmp3   <- txt[seq_along(tmp2)[tmp2]-1]


  tmp4   <- paste0(x_char, tmp3)

  tmp5   <- rlang::parse_exprs(tmp4)

  fs::file_delete(tmp)

  tmp5

}
