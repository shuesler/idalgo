#' @export
patterns_lazy <- function(var, value, folge) {

  rlang::expr(.data[[var]] == !!value ~ !!folge)
}
