api_build <- function() {
  api <- pkgapi::pkgapi$new()
  api$handle(endpoint_root())
  api$handle(endpoint_baseline_individual())
  api
}

api <- function(port = 8888, queue_id = NULL, workers = 2,
                results_dir = tempdir(), prerun_dir = NULL) {
  # nocov start
  api <- api_build()
  api$run(port = port, swagger = FALSE)
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
                              "validate/baseline-individual",
                              validate_baseline,
                              input,
                              returning = response,
                              validate = TRUE)
}
