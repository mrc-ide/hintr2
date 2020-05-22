hintr2_file <- function(...) {
  system.file(..., package = "hintr2", mustWork = TRUE)
}

schema_root <- function() {
  hintr2_file("schema")
}

scalar <- function(val) {
  jsonlite::unbox(val)
}

json_null <- function() {
  null <- "null"
  class(null) <- "logical"
  scalar(null)
}

is_pkgapi_error <- function(e) {
  inherits(e, "pkgapi_error")
}
