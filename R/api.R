api_build <- function(queue) {
  api <- pkgapi::pkgapi$new()
  api$handle(endpoint_root())
  api$handle(endpoint_baseline_individual())
  api$handle(endpoint_baseline_combined())
  api$handle(endpoint_validate_survey_programme())
  api$handle(endpoint_model_options())
  api$handle(endpoint_model_options_validate())
  api$handle(endpoint_model_submit(queue))
  api$handle(endpoint_model_status(queue))
  api$handle(endpoint_model_result(queue))
  api$handle(endpoint_model_cancel(queue))
  api$handle(endpoint_model_debug(queue))
  api$handle(endpoint_plotting_metadata())
  api$handle(endpoint_download_spectrum(queue))
  api$handle(endpoint_download_spectrum_head(queue))
  api$handle(endpoint_download_summary(queue))
  api$handle(endpoint_download_summary_head(queue))
  api$handle(endpoint_hintr_version())
  api$handle(endpoint_hintr_worker_status(queue))
  api$handle(endpoint_hintr_stop(queue))
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

endpoint_baseline_combined <- function() {
  input <- pkgapi::pkgapi_input_body_json("input",
                                          "ValidateBaselineRequest.schema",
                                          schema_root())
  response <- pkgapi::pkgapi_returning_json("ValidateBaselineResponse.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("POST",
                              "/validate/baseline-combined",
                              validate_baseline_combined,
                              input,
                              returning = response,
                              validate = TRUE)
}

endpoint_validate_survey_programme <- function() {
  input <- pkgapi::pkgapi_input_body_json(
    "input", "ValidateSurveyAndProgrammeRequest.schema", schema_root())
  response <- pkgapi::pkgapi_returning_json("ValidateInputResponse.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("POST",
                              "/validate/survey-and-programme",
                              validate_survey_programme,
                              input,
                              returning = response,
                              validate = TRUE)
}

returning_json_version <- function(schema = NULL, root = NULL,
                                   status_code = 200L) {
  ## This is the same as pkgapi::pkgapi_returning_json except we
  ## override the process function to also add version info along side the
  ## data
  returning  <- pkgapi::pkgapi_returning_json(schema, root, status_code)
  response_success <- function(data) {
    list(
      status = jsonlite::unbox("success"),
      errors = json_null(),
      data = data,
      version = cfg$version_info
    )
  }
  returning$process <- function(data) {
    as.character(hintr:::to_json(response_success(data)))
  }
  returning
}

endpoint_model_options <- function() {
  input <- pkgapi::pkgapi_input_body_json("input",
                                          "ModelRunOptionsRequest.schema",
                                          schema_root())
  response <- returning_json_version("ModelRunOptions.schema", schema_root())
  pkgapi::pkgapi_endpoint$new("POST",
                              "/model/options",
                              model_options,
                              input,
                              returning = response,
                              validate = TRUE)
}

endpoint_model_options_validate <- function() {
  input <- pkgapi::pkgapi_input_body_json("input",
                                          "ModelOptionsValidateRequest.schema",
                                          schema_root())
  response <- pkgapi::pkgapi_returning_json("ModelOptionsValidate.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("POST",
                              "/validate/options",
                              model_options_validate,
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

endpoint_model_status <- function(queue) {
  response <- pkgapi::pkgapi_returning_json("ModelStatusResponse.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("GET",
                              "/model/status/<id>",
                              model_status(queue),
                              returning = response,
                              validate = TRUE)
}

endpoint_model_result <- function(queue) {
  response <- pkgapi::pkgapi_returning_json("ModelResultResponse.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("GET",
                              "/model/result/<id>",
                              model_result(queue),
                              returning = response,
                              validate = TRUE)
}

endpoint_model_cancel <- function(queue) {
  response <- pkgapi::pkgapi_returning_json("ModelCancelResponse.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("GET",
                              "/model/cancel/<id>",
                              model_cancel(queue),
                              returning = response,
                              validate = TRUE)
}

endpoint_model_debug <- function(queue) {
  pkgapi::pkgapi_endpoint$new("GET",
                              "/model/debug/<id>",
                              download_debug(queue),
                              returning = pkgapi::pkgapi_returning_binary())
}

endpoint_plotting_metadata <- function() {
  response <- pkgapi::pkgapi_returning_json("PlottingMetadataResponse.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("GET",
                              "/meta/plotting/<iso3>",
                              plotting_metadata,
                              returning = response,
                              validate = TRUE)
}

## Return same headers as binary returning but ensure no body is returned.
returning_binary_head <- function(status_code = 200L) {
  pkgapi::pkgapi_returning("application/octet-stream",
                           process = function(data) NULL,
                           validate = function(body) TRUE)
}

endpoint_download_spectrum <- function(queue) {
  pkgapi::pkgapi_endpoint$new("GET",
                              "/download/spectrum/<id>",
                              download_spectrum(queue),
                              returning = pkgapi::pkgapi_returning_binary())
}

endpoint_download_spectrum_head <- function(queue) {
  pkgapi::pkgapi_endpoint$new("HEAD",
                              "/download/spectrum/<id>",
                              download_spectrum(queue),
                              returning = returning_binary_head(),
                              validate = FALSE)
}

endpoint_download_summary <- function(queue) {
  pkgapi::pkgapi_endpoint$new("GET",
                              "/download/summary/<id>",
                              download_summary(queue),
                              returning = pkgapi::pkgapi_returning_binary())
}

endpoint_download_summary_head <- function(queue) {
  pkgapi::pkgapi_endpoint$new("HEAD",
                              "/download/summary/<id>",
                              download_summary(queue),
                              returning = returning_binary_head(),
                              validate = FALSE)
}

endpoint_hintr_version <- function() {
  response <- pkgapi::pkgapi_returning_json("HintrVersionResponse.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("GET",
                              "/hintr/version",
                              function() cfg$version_info,
                              returning = response,
                              validate = TRUE)
}

endpoint_hintr_worker_status <- function(queue) {
  response <- pkgapi::pkgapi_returning_json("HintrWorkerStatus.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("GET",
                              "/hintr/worker/status",
                              worker_status(queue),
                              returning = response,
                              validate = TRUE)
}

pkgapi_returning_null <- function() {
  pkgapi::pkgapi_returning(content_type = "text/plain",
                           process = function(data) NULL,
                           validate = function(body) TRUE)
}

endpoint_hintr_stop <- function(queue) {
  pkgapi::pkgapi_endpoint$new("POST",
                              "/hintr/stop",
                              hintr_stop(queue),
                              returning = pkgapi_returning_null(),
                              validate = FALSE)
}
