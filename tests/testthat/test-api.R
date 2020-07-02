context("api")

test_that("endpoint_baseline_individual", {
  endpoint <- endpoint_baseline_individual()
  response <- endpoint$run(readLines("payload/validate_pjnz_payload.json"))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_equal(response$data$hash, scalar("12345"))
  expect_equal(response$data$data$country, scalar("Malawi"))
  expect_equal(response$data$data$iso3, scalar("MWI"))
  expect_equal(response$data$filename, scalar("Malawi2019.PJNZ"))
  expect_equal(response$data$filters, json_null())
})

test_that("endpoint_baseline_individual works", {
  api <- api_build()
  res <- api$request("POST", "/validate/baseline-individual",
                     body = readLines("payload/validate_pjnz_payload.json"))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$data$hash, "12345")
  expect_equal(body$data$data$country, "Malawi")
  expect_equal(body$data$data$iso3, "MWI")
  expect_equal(body$data$filename, "Malawi2019.PJNZ")
  expect_equal(body$data$filters, NULL)
})

test_that("endpoint_baseline_combined", {
  endpoint <- endpoint_baseline_combined()
  response <- endpoint$run(readLines("payload/validate_baseline_payload.json"))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_equal(response$data$consistent, scalar(TRUE))
})

test_that("endpoint_baseline_combined works", {
  api <- api_build()
  res <- api$request("POST", "/validate/baseline-combined",
                     body = readLines("payload/validate_baseline_payload.json"))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_true(body$data$consistent)
})

test_that("endpoint_validate_survey_programme programme", {
  endpoint <- endpoint_validate_survey_programme()
  response <- endpoint$run(readLines("payload/validate_programme_payload.json"))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_equal(response$data$filename, scalar("original.csv"))
  expect_equal(response$data$hash, scalar("12345"))
  ## Sanity check that data has been returned
  expect_true(nrow(response$data$data) >= 200)
  expect_equal(typeof(response$data$data[, "current_art"]), "double")
})

test_that("endpoint_validate_survey_programme works with programme data", {
  api <- api_build()
  res <- api$request("POST", "/validate/survey-and-programme",
                     body =
                       readLines("payload/validate_programme_payload.json"))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(body$data$filename, "original.csv")
  expect_equal(body$data$hash, "12345")
  ## Sanity check that data has been returned
  expect_true(nrow(body$data$data) >= 200)
  expect_equal(typeof(body$data$data[, "current_art"]), "double")
})

test_that("endpoint_validate_survey_programme anc", {
  endpoint <- endpoint_validate_survey_programme()
  response <- endpoint$run(readLines("payload/validate_anc_payload.json"))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_equal(response$data$filename, scalar("original.csv"))
  expect_equal(response$data$hash, scalar("12345"))
  ## Sanity check that data has been returned
  expect_true(nrow(response$data$data) >= 200)
  expect_equal(typeof(response$data$data[, "prevalence"]), "double")
  expect_equal(typeof(response$data$data[, "art_coverage"]), "double")
})

test_that("endpoint_validate_survey_programme works with anc data", {
  api <- api_build()
  res <- api$request("POST", "/validate/survey-and-programme",
                     body = readLines("payload/validate_anc_payload.json"))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(body$data$filename, "original.csv")
  expect_equal(body$data$hash, "12345")
  ## Sanity check that data has been returned
  expect_true(nrow(body$data$data) >= 200)
  expect_equal(typeof(body$data$data[, "prevalence"]), "double")
  expect_equal(typeof(body$data$data[, "art_coverage"]), "double")
})

test_that("endpoint_validate_survey_programme survey", {
  endpoint <- endpoint_validate_survey_programme()
  response <- endpoint$run(readLines("payload/validate_survey_payload.json"))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_equal(response$data$filename, scalar("original.csv"))
  expect_equal(response$data$hash, scalar("12345"))
  ## Sanity check that data has been returned
  expect_true(nrow(response$data$data) >= 20000)
  expect_equal(typeof(response$data$data[, "est"]), "double")
})

test_that("endpoint_validate_survey_programme works with survey data", {
  api <- api_build()
  res <- api$request("POST", "/validate/survey-and-programme",
                     body = readLines("payload/validate_survey_payload.json"))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(body$data$filename, "original.csv")
  expect_equal(body$data$hash, "12345")
  ## Sanity check that data has been returned
  expect_true(nrow(body$data$data) >= 20000)
  expect_equal(typeof(body$data$data[, "est"]), "double")
})

test_that("endpoint_model_submit can be run", {
  test_redis_available()
  queue <- hintr:::Queue$new()
  endpoint <- endpoint_model_submit(queue)
  path <- setup_submit_payload()
  response <- endpoint$run(readLines(path))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_true(!is.null(response$data$id))
})

test_that("api can call endpoint_model_submit", {
  test_redis_available()
  queue <- hintr:::Queue$new()
  api <- api_build(queue)
  path <- setup_submit_payload()
  res <- api$request("POST", "/model/submit",
                     body = readLines(path))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_true(!is.null(body$data$id))
})

test_that("endpoint_model_status can be run", {
  test_redis_available()
  test_mock_model_available()
  queue <- hintr:::Queue$new()
  model_run <- endpoint_model_submit(queue)
  path <- setup_submit_payload()
  run_response <- model_run$run(readLines(path))
  expect_equal(run_response$status_code, 200)
  expect_true(!is.null(run_response$data$id))

  endpoint <- endpoint_model_status(queue)
  out <- queue$queue$task_wait(run_response$data$id)
  response <- endpoint$run(run_response$data$id)
  expect_equal(response$status_code, 200)
  expect_equal(response$data$id, run_response$data$id)
  expect_equal(response$data$done, scalar(TRUE))
  expect_equal(response$data$status, scalar("COMPLETE"))
  expect_equal(response$data$queue, scalar(0))
  expect_equal(response$data$success, scalar(TRUE))
  expect_length(response$data$progress, 2)
  expect_equal(response$data$progress[[1]]$name, scalar("Started mock model"))
  expect_true(response$data$progress[[1]]$complete)
  expect_equal(response$data$progress[[2]]$name, scalar("Finished mock model"))
  expect_false(response$data$progress[[2]]$complete)
})

test_that("api can call endpoint_model_status", {
  test_redis_available()
  test_mock_model_available()
  queue <- hintr:::Queue$new()
  api <- api_build(queue)
  path <- setup_submit_payload()
  res <- api$request("POST", "/model/submit",
                     body = readLines(path))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_true(!is.null(body$data$id))

  out <- queue$queue$task_wait(body$data$id)
  res <- api$request("GET", sprintf("/model/status/%s", body$data$id))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(body$data$id, body$data$id)
  expect_equal(body$data$done, TRUE)
  expect_equal(body$data$status, "COMPLETE")
  expect_equal(body$data$queue, 0)
  expect_equal(body$data$success, TRUE)
  expect_equal(nrow(body$data$progress), 2)
  expect_equal(body$data$progress[1, "name"], "Started mock model")
  expect_true(body$data$progress[1, "complete"])
  expect_equal(body$data$progress[2, "name"], "Finished mock model")
  expect_false(body$data$progress[2, "complete"])
})

test_that("endpoint_plotting_metadata can be run", {
  endpoint <- endpoint_plotting_metadata()
  response <- endpoint$run("MWI")

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_true(all(names(response$data) %in%
                    c("survey", "anc", "output", "programme")))
})

test_that("api can call endpoint_plotting_metadata", {
  api <- api_build()
  res <- api$request("GET", "/meta/plotting/MWI")
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_true(all(names(body$data) %in%
                    c("survey", "anc", "output", "programme")))
})

test_that("endpoint_download_spectrum can be run", {
  test_redis_available()
  test_mock_model_available()

  queue <- hintr:::Queue$new()
  run_endpoint <- endpoint_model_submit(queue)
  path <- setup_submit_payload()
  run_response <- run_endpoint$run(readLines(path))
  expect_equal(run_response$status_code, 200)
  out <- queue$queue$task_wait(run_response$data$id)

  endpoint <- endpoint_download_spectrum(queue)
  response <- endpoint$run(run_response$data$id)

  expect_equal(response$status_code, 200)
  expect_match(response$headers$`Content-Disposition`,
               'attachment; filename="MWI_\\d+-\\d+_naomi_spectrum_digest.zip"')
  ## Size of bytes is close to expected
  size <- length(response$data)
  expect_true(size - size/10 <
                file.size(system.file("output", "malawi_spectrum_download.zip",
                                      package = "hintr")))
  expect_true(size + size/10 >
                file.size(system.file("output", "malawi_spectrum_download.zip",
                                      package = "hintr")))
})

test_that("api can call endpoint_download_spectrum", {
  test_redis_available()
  test_mock_model_available()

  queue <- hintr:::Queue$new()
  api <- api_build(queue)

  ## Run the model
  path <- setup_submit_payload()
  res <- api$request("POST", "/model/submit",
                     body = readLines(path))
  expect_equal(res$status, 200)
  response <- jsonlite::fromJSON(res$body)
  out <- queue$queue$task_wait(response$data$id)

  ## Get result
  res <- api$request("GET", paste0("/download/spectrum/", response$data$id))

  expect_equal(res$status, 200)
  expect_equal(res$headers$`Content-Type`, "application/octet-stream")
  expect_match(res$headers$`Content-Disposition`,
               'attachment; filename="MWI_\\d+-\\d+_naomi_spectrum_digest.zip"')
  ## Size of bytes is close to expected
  size <- length(res$body)
  expect_true(size - size/10 <
                file.size(system.file("output", "malawi_spectrum_download.zip",
                                      package = "hintr")))
  expect_true(size + size/10 >
                file.size(system.file("output", "malawi_spectrum_download.zip",
                                      package = "hintr")))
})

test_that("endpoint_download_summary can be run", {
  test_redis_available()
  test_mock_model_available()

  queue <- hintr:::Queue$new()
  run_endpoint <- endpoint_model_submit(queue)
  path <- setup_submit_payload()
  run_response <- run_endpoint$run(readLines(path))
  expect_equal(run_response$status_code, 200)
  out <- queue$queue$task_wait(run_response$data$id)

  endpoint <- endpoint_download_summary(queue)
  response <- endpoint$run(run_response$data$id)

  expect_equal(response$status_code, 200)
  expect_match(
    response$headers$`Content-Disposition`,
    'attachment; filename="MWI_\\d+-\\d+_naomi_coarse_age_groups.zip"')
  ## Size of bytes is close to expected
  size <- length(response$data)
  expect_true(size - size/10 <
                file.size(system.file("output", "malawi_summary_download.zip",
                                      package = "hintr")))
  expect_true(size + size/10 >
                file.size(system.file("output", "malawi_summary_download.zip",
                                      package = "hintr")))
})

test_that("api can call endpoint_download_summary", {
  test_redis_available()
  test_mock_model_available()

  queue <- hintr:::Queue$new()
  api <- api_build(queue)

  ## Run the model
  path <- setup_submit_payload()
  res <- api$request("POST", "/model/submit",
                     body = readLines(path))
  expect_equal(res$status, 200)
  response <- jsonlite::fromJSON(res$body)
  out <- queue$queue$task_wait(response$data$id)

  ## Get result
  res <- api$request("GET", paste0("/download/summary/", response$data$id))

  expect_equal(res$status, 200)
  expect_equal(res$headers$`Content-Type`, "application/octet-stream")
  expect_match(
    res$headers$`Content-Disposition`,
    'attachment; filename="MWI_\\d+-\\d+_naomi_coarse_age_groups.zip"')
  ## Size of bytes is close to expected
  size <- length(res$body)
  expect_true(size - size/10 <
                file.size(system.file("output", "malawi_summary_download.zip",
                                      package = "hintr")))
  expect_true(size + size/10 >
                file.size(system.file("output", "malawi_summary_download.zip",
                                      package = "hintr")))
})

test_that("returning_binary_head ensures no body in response", {
  returning <- returning_binary_head()
  data <- charToRaw("test")
  expect_null(returning$process(data))
  expect_true(returning$validate(NULL))
})

test_that("endpoint_download_spectrum_head returns headers only", {
  test_redis_available()
  test_mock_model_available()

  queue <- hintr:::Queue$new()
  run_endpoint <- endpoint_model_submit(queue)
  path <- setup_submit_payload()
  run_response <- run_endpoint$run(readLines(path))
  expect_equal(run_response$status_code, 200)
  out <- queue$queue$task_wait(run_response$data$id)

  endpoint <- endpoint_download_spectrum_head(queue)
  response <- endpoint$run(run_response$data$id)

  expect_equal(response$status_code, 200)
  expect_equal(response$content_type, "application/octet-stream")
  expect_match(response$headers$`Content-Disposition`,
               'attachment; filename="MWI_\\d+-\\d+_naomi_spectrum_digest.zip"')
  expect_null(response$body, NULL)
})

test_that("api endpoint_download_spectrum_head returns headers only", {
  test_redis_available()
  test_mock_model_available()

  queue <- hintr:::Queue$new()
  api <- api_build(queue)

  ## Run the model
  path <- setup_submit_payload()
  res <- api$request("POST", "/model/submit",
                     body = readLines(path))
  expect_equal(res$status, 200)
  response <- jsonlite::fromJSON(res$body)
  out <- queue$queue$task_wait(response$data$id)

  ## Get result
  res <- api$request("HEAD", paste0("/download/spectrum/", response$data$id))

  expect_equal(res$status, 200)
  expect_equal(res$headers$`Content-Type`, "application/octet-stream")
  expect_match(res$headers$`Content-Disposition`,
               'attachment; filename="MWI_\\d+-\\d+_naomi_spectrum_digest.zip"')
  ## Plumber uses an empty string to represent an empty body
  expect_equal(res$body, "")
})

test_that("endpoint_download_summary_head returns headers only", {
  test_redis_available()
  test_mock_model_available()

  queue <- hintr:::Queue$new()
  run_endpoint <- endpoint_model_submit(queue)
  path <- setup_submit_payload()
  run_response <- run_endpoint$run(readLines(path))
  expect_equal(run_response$status_code, 200)
  out <- queue$queue$task_wait(run_response$data$id)

  endpoint <- endpoint_download_summary_head(queue)
  response <- endpoint$run(run_response$data$id)

  expect_equal(response$status_code, 200)
  expect_equal(response$content_type, "application/octet-stream")
  expect_match(
    response$headers$`Content-Disposition`,
     'attachment; filename="MWI_\\d+-\\d+_naomi_coarse_age_groups.zip"')
  expect_null(response$body, NULL)
})

test_that("api endpoint_download_summary_head returns headers only", {
  test_redis_available()
  test_mock_model_available()

  queue <- hintr:::Queue$new()
  api <- api_build(queue)

  ## Run the model
  path <- setup_submit_payload()
  res <- api$request("POST", "/model/submit",
                     body = readLines(path))
  expect_equal(res$status, 200)
  response <- jsonlite::fromJSON(res$body)
  out <- queue$queue$task_wait(response$data$id)

  ## Get result
  res <- api$request("HEAD", paste0("/download/summary/", response$data$id))

  expect_equal(res$status, 200)
  expect_equal(res$headers$`Content-Type`, "application/octet-stream")
  expect_match(
    res$headers$`Content-Disposition`,
    'attachment; filename="MWI_\\d+-\\d+_naomi_coarse_age_groups.zip"')
  ## Plumber uses an empty string to represent an empty body
  expect_equal(res$body, "")
})
