api_build <- function() {
  api <- pkgapi::pkgapi$new()
  api$handle(endpoint_baseline_individual())
  api
}

api <- function(port = 8888, queue_id = NULL, workers = 2,
                results_dir = tempdir(), prerun_dir = NULL) {
  # nocov start
  api <- api_build()
  api$run(port, swagger = FALSE)
  # nocov end
}

endpoint_baseline_individual <- function() {
  ## TODO: Shouldn't have to paste root here but it isn't picking up the
  ## schema directory automatically
  root <- system.file("schema", package = "hintr2")
  response <- pkgapi::pkgapi_returning_json("ValidateInputResponse.schema",
                                            root)
  input <- pkgapi::pkgapi_input_body_json("validate_input",
                                          "ValidateInputRequest.schema", root)
  pkgapi::pkgapi_endpoint$new("POST",
                              "validate/baseline-individual",
                              validate_baseline,
                              returning = response,
                              input,
                              validate = TRUE)
}
