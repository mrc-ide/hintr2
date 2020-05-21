#' @importFrom traduire t_
NULL

.onLoad <- function(...) {
  hintr:::hintr_init_traduire() # nocov
}
