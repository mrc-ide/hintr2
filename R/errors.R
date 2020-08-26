hintr_error <- function(message, error, status_code = 400L, ...) {
  key <- scalar(ids::proquint(n_words = 3))
  pkgapi::pkgapi_stop(message, error, errors = NULL, status_code = status_code,
                      key = key, ...)
}

hintr2_404_handler <- function(req, res) {
  ## Manually construct the response here
  res$status <- 404L
  message <- tr_("ERROR_404",
                 list(verb = req$REQUEST_METHOD, path = req$PATH_INFO))
  list(
    status = scalar("failure"),
    errors = list(
      list(
        error = scalar("NOT_FOUND"),
        detail = scalar(message),
        key = scalar(ids::proquint(n_words = 3))
      )
    ),
    data = NULL
  )
}
