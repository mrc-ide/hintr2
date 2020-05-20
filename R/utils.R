hintr2_file <- function(...) {
  system.file(..., package = "hintr2", mustWork = TRUE)
}

schema_root <- function() {
  hintr2_file("schema")
}

scalar <- function(val) {
  jsonlite::unbox(val)
}
