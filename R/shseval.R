shseval <- function(x, code = FALSE) {

  tmp  <- tempfile(fileext = ".R")

  sink(tmp)
  for (i in seq_along(x)) {
    print(x[[i]])
  }
  sink()

  source(tmp, local = globalenv())

  if(code) {

    shell.exec(tmp)
  }


}
