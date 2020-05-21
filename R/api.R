api_build <- function(queue) {
  api <- pkgapi::pkgapi$new()
  api$handle(endpoint_root())
  api$handle(endpoint_baseline_individual())
  api$handle(endpoint_model_submit(queue))
  api
}

#' Build and start the API
#'
#' @param port Port for API
#' @param queue_id ID of an existing queue to connect to, creates a new one
#' if NULL
#' @param workers Number of workers to spawn
#' @param results_dir The dir for results to be saved to
#' @param prerun_dir The directory to store prerun results
#'
#' @return Running API
#' @export
api <- function(port = 8888, queue_id = NULL, workers = 2,
                results_dir = tempdir(), prerun_dir = NULL) {
  # nocov start
  queue <- hintr:::Queue$new(queue_id, workers, results_dir = results_dir,
                             prerun_dir = prerun_dir)
  api <- api_build(queue)
  api$run(port = port)
  # nocov end
}

endpoint_root <- function() {
  pkgapi::pkgapi_endpoint$new("GET",
                              "/",
                              root_endpoint,
                              returning = pkgapi::pkgapi_returning_json())
}

endpoint_baseline_individual <- function() {
  ## TODO: Shouldn't have to paste root here but it isn't picking up the
  ## schema directory automatically
  input <- pkgapi::pkgapi_input_body_json("input",
                                          "ValidateInputRequest.schema",
                                          schema_root())
  response <- pkgapi::pkgapi_returning_json("ValidateInputResponse.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("POST",
                              "/validate/baseline-individual",
                              validate_baseline,
                              input,
                              returning = response,
                              validate = TRUE)
}

endpoint_model_submit <- function(queue) {
  input <- pkgapi::pkgapi_input_body_json("input",
                                          "ModelSubmitRequest.schema",
                                          schema_root())
  response <- pkgapi::pkgapi_returning_json("ModelSubmitResponse.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("POST",
                              "/model/submit",
                              submit_model(queue),
                              input,
                              returning = response,
                              validate = TRUE)
}
