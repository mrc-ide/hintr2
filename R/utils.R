hintr2_file <- function(...) {
  system.file(..., package = "hintr2", mustWork = TRUE)
}

schema_root <- function() {
  hintr2_file("schema")
}

scalar <- function(val) {
  if ("scalar" %in% class(val)) {
    val
  } else {
    jsonlite::unbox(val)
  }
}

scalar_null <- function() {
  null <- "null"
  class(null) <- "logical"
  scalar(null)
}

json_null <- function() {
  null <- "null"
  class(null) <- "json"
  scalar(null)
}

is_pkgapi_error <- function(e) {
  inherits(e, "pkgapi_error")
}
