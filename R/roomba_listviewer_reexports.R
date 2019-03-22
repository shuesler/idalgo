#' @export
replace_null <- function (x, replacement = NA)
{
  empty_idx <- dfs_idx(x, ~length(.x) == 0 || is.na(.x))
  for (i in empty_idx) {
    x[[i]] <- replacement
  }
  x
}

#' @export
dfs_idx <- function (.x, .f)
{
  .f <- purrr::as_mapper(.f)
  res <- list()
  num <- 0L
  walk <- function(x, idx) {
    for (i in seq_along(x)) {
      if (isTRUE(tryCatch(.f(x[[i]]), error = function(e) FALSE))) {
        res[[num <<- num + 1L]] <<- append(idx, i)
      }
      if (is.list(x[[i]])) {
        walk(x[[i]], append(idx, i))
      }
    }
  }
  walk(.x, integer())
  res
}

#' @export
number_unnamed <- function (l)
{
  recurse(l, add_number_names)
}

#' @export
recurse <- function (l, func, ...)
{
  l <- func(l, ...)
  if (is.list(l) && length(l) > 0) {
    lapply(l, function(ll) {
      recurse(ll, func, ...)
    })
  }
  else {
    l
  }
}

#' @export
add_number_names <- function (l)
{
  if (length(l) > 1 && is.null(names(l))) {
    names(l) <- seq_len(length(l))
  }
  l
}
