hintr_error <- function(message, error, status_code = 400L, ...) {
  key <- scalar(ids::proquint(n_words = 3))
  pkgapi::pkgapi_stop(message, error, errors = NULL, status_code = status_code,
                      key = key, ...)
}
