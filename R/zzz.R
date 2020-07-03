#' @importFrom traduire t_
NULL

cfg <- new.env(parent = emptyenv())
.onLoad <- function(...) {
  cfg$version_info <- hintr:::get_version_info() # nocov
  hintr:::hintr_init_traduire() # nocov
}

tr_ <- function(...) {
  t_(..., package = "hintr")
}
