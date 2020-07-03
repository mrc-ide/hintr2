test_redis_available <- function() {
  available <- redux::redis_available()
  if (!available) {
    testthat::skip("Skipping test as redis is not available")
  }
  invisible(available)
}

MockQueue <- R6::R6Class(
  "MockQueue",
  inherit = hintr:::Queue,
  cloneable = FALSE,
  public = list(
    submit = function(data, options) {
      self$queue$enqueue_(quote(stop("test error")))
    }
  )
)

test_queue <- function() {
  queue <- hintr:::Queue$new()
  withr::defer_parent({
    message("cleaning up workers")
    queue$cleanup()
  })
  queue
}
