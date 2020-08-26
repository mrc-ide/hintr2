context("integration")

test_that("validate pjnz is equivalent", {
  hintr_server <- hintr_server()
  hintr2_server <- hintr2_server()
  payload <- file.path("payload", "validate_pjnz_payload.json")

  hintr_r <- httr::POST(paste0(hintr_server$url, "/validate/baseline-individual"),
                  body = httr::upload_file(payload),
                  encode = "json")
  expect_equal(httr::status_code(hintr_r), 200)

  hintr2_r <- httr::POST(paste0(hintr2_server$url, "/validate/baseline-individual"),
                        body = httr::upload_file(payload),
                        encode = "json")
  expect_equal(httr::status_code(hintr2_r), 200)

  ## Identical up to format of errors
  hintr <- response_from_json(hintr_r)
  hintr2 <- response_from_json(hintr2_r)
  ## Format when no errors is different
  expect_equal(hintr$errors, list())
  hintr$errors <- NULL
  expect_equal(hintr2$errors, NULL)
  hintr2$errors <- NULL
  expect_identical(hintr, hintr2)
  gc()
})

test_that("validate shape is equivalent", {
  hintr_server <- hintr_server()
  hintr2_server <- hintr2_server()
  payload <- file.path("payload", "validate_shape_payload.json")

  hintr_r <- httr::POST(paste0(hintr_server$url, "/validate/baseline-individual"),
                        body = httr::upload_file(payload),
                        encode = "json")
  expect_equal(httr::status_code(hintr_r), 200)

  hintr2_r <- httr::POST(paste0(hintr2_server$url, "/validate/baseline-individual"),
                         body = httr::upload_file(payload),
                         encode = "json")
  expect_equal(httr::status_code(hintr2_r), 200)

  ## Identical up to format of errors
  hintr <- response_from_json(hintr_r)
  hintr2 <- response_from_json(hintr2_r)
  ## Format when no errors is different
  expect_equal(hintr$errors, list())
  hintr$errors <- NULL
  expect_equal(hintr2$errors, NULL)
  hintr2$errors <- NULL
  expect_identical(hintr, hintr2)
  gc()
})

test_that("validate population is equivalent", {
  hintr_server <- hintr_server()
  hintr2_server <- hintr2_server()
  payload <- file.path("payload", "validate_population_payload.json")

  hintr_r <- httr::POST(paste0(hintr_server$url, "/validate/baseline-individual"),
                        body = httr::upload_file(payload),
                        encode = "json")
  expect_equal(httr::status_code(hintr_r), 200)

  hintr2_r <- httr::POST(paste0(hintr2_server$url, "/validate/baseline-individual"),
                         body = httr::upload_file(payload),
                         encode = "json")
  expect_equal(httr::status_code(hintr2_r), 200)

  ## Identical up to format of errors
  hintr <- response_from_json(hintr_r)
  hintr2 <- response_from_json(hintr2_r)
  ## Format when no errors is different
  expect_equal(hintr$errors, list())
  hintr$errors <- NULL
  expect_equal(hintr2$errors, NULL)
  hintr2$errors <- NULL
  expect_identical(hintr, hintr2)
  gc()
})

test_that("validate population is equivalent", {
  hintr_server <- hintr_server()
  hintr2_server <- hintr2_server()
  payload <- file.path("payload", "validate_programme_payload.json")

  hintr_r <- httr::POST(paste0(hintr_server$url, "/validate/survey-and-programme"),
                        body = httr::upload_file(payload),
                        encode = "json")
  expect_equal(httr::status_code(hintr_r), 200)

  hintr2_r <- httr::POST(paste0(hintr2_server$url, "/validate/survey-and-programme"),
                         body = httr::upload_file(payload),
                         encode = "json")
  expect_equal(httr::status_code(hintr2_r), 200)

  ## Identical up to format of errors
  hintr <- response_from_json(hintr_r)
  hintr2 <- response_from_json(hintr2_r)
  ## Format when no errors is different
  expect_equal(hintr$errors, list())
  hintr$errors <- NULL
  expect_equal(hintr2$errors, NULL)
  hintr2$errors <- NULL
  expect_identical(hintr, hintr2)
  gc()
})

test_that("validate ANC is equivalent", {
  hintr_server <- hintr_server()
  hintr2_server <- hintr2_server()
  payload <- file.path("payload", "validate_anc_payload.json")

  hintr_r <- httr::POST(paste0(hintr_server$url, "/validate/survey-and-programme"),
                        body = httr::upload_file(payload),
                        encode = "json")
  expect_equal(httr::status_code(hintr_r), 200)

  hintr2_r <- httr::POST(paste0(hintr2_server$url, "/validate/survey-and-programme"),
                         body = httr::upload_file(payload),
                         encode = "json")
  expect_equal(httr::status_code(hintr2_r), 200)

  ## Identical up to format of errors
  hintr <- response_from_json(hintr_r)
  hintr2 <- response_from_json(hintr2_r)
  ## Format when no errors is different
  expect_equal(hintr$errors, list())
  hintr$errors <- NULL
  expect_equal(hintr2$errors, NULL)
  hintr2$errors <- NULL
  expect_identical(hintr, hintr2)
  gc()
})

test_that("validate survey is equivalent", {
  hintr_server <- hintr_server()
  hintr2_server <- hintr2_server()
  payload <- file.path("payload", "validate_survey_payload.json")

  hintr_r <- httr::POST(paste0(hintr_server$url, "/validate/survey-and-programme"),
                        body = httr::upload_file(payload),
                        encode = "json")
  expect_equal(httr::status_code(hintr_r), 200)

  hintr2_r <- httr::POST(paste0(hintr2_server$url, "/validate/survey-and-programme"),
                         body = httr::upload_file(payload),
                         encode = "json")
  expect_equal(httr::status_code(hintr2_r), 200)

  ## Identical up to format of errors
  hintr <- response_from_json(hintr_r)
  hintr2 <- response_from_json(hintr2_r)
  ## Format when no errors is different
  expect_equal(hintr$errors, list())
  hintr$errors <- NULL
  expect_equal(hintr2$errors, NULL)
  hintr2$errors <- NULL
  expect_identical(hintr, hintr2)
  gc()
})

test_that("validate baseline is equivalent", {
  hintr_server <- hintr_server()
  hintr2_server <- hintr2_server()
  payload <- file.path("payload", "validate_baseline_payload.json")

  hintr_r <- httr::POST(paste0(hintr_server$url, "/validate/baseline-combined"),
                        body = httr::upload_file(payload),
                        encode = "json")
  expect_equal(httr::status_code(hintr_r), 200)

  hintr2_r <- httr::POST(paste0(hintr2_server$url, "/validate/baseline-combined"),
                         body = httr::upload_file(payload),
                         encode = "json")
  expect_equal(httr::status_code(hintr2_r), 200)

  ## Identical up to format of errors
  hintr <- response_from_json(hintr_r)
  hintr2 <- response_from_json(hintr2_r)
  ## Format when no errors is different
  expect_equal(hintr$errors, list())
  hintr$errors <- NULL
  expect_equal(hintr2$errors, NULL)
  hintr2$errors <- NULL
  expect_identical(hintr, hintr2)
  gc()
})

test_that("model run is equivalent", {
  hintr_results <- tempfile("hintr_results")
  dir.create(hintr_results)
  hintr2_results <- tempfile("hintr2_results")
  dir.create(hintr2_results)
  withr::with_envvar(c("USE_MOCK_MODEL" = "false"), {
    hintr_server <- hintr_server(results_dir = hintr_results)
    hintr2_server <- hintr2_server(results_dir = hintr2_results)
    payload <- setup_submit_payload()

    ## Submit
    hintr_r <- httr::POST(paste0(hintr_server$url, "/model/submit"),
                          body = httr::upload_file(payload,
                                                   type = "application/json"),
                          encode = "json")
    expect_equal(httr::status_code(hintr_r), 200)
    hintr_response <- response_from_json(hintr_r)

    hintr2_r <- httr::POST(paste0(hintr2_server$url, "/model/submit"),
                           body = httr::upload_file(payload,
                                                    type = "application/json"),
                           encode = "json")
    expect_equal(httr::status_code(hintr2_r), 200)
    hintr2_response <- response_from_json(hintr2_r)
  })

  ## Content won't be identical as ID of model run will differ
  expect_identical(hintr_response$status, hintr2_response$status)


  ## Status
  testthat::try_again(5, {
    Sys.sleep(60)
    hintr_r <- httr::GET(paste0(hintr_server$url, "/model/status/",
                                hintr_response$data$id))
    expect_equal(httr::status_code(hintr_r), 200)
    hintr_response <- response_from_json(hintr_r)
    expect_equal(hintr_response$data$status, "COMPLETE")

    hintr2_r <- httr::GET(paste0(hintr2_server$url, "/model/status/",
                                hintr2_response$data$id))
    expect_equal(httr::status_code(hintr2_r), 200)
    hintr2_response <- response_from_json(hintr2_r)
    expect_equal(hintr2_response$data$status, "COMPLETE")
  })

  ## Identical up to data id
  hintr_resp <- hintr_response
  hintr_resp$data$id <- NULL
  hintr2_resp <- hintr2_response
  hintr2_resp$data$id <- NULL
  ## Format of errors section has also changed update this for check
  expect_equal(hintr_resp$errors, list())
  expect_equal(hintr2_resp$errors, NULL)
  hintr_resp$errors <- NULL
  hintr2_resp$errors <- NULL
  expect_identical(hintr_resp, hintr2_resp)

  ## Debug
  hintr_r <- httr::GET(paste0(hintr_server$url, "/model/debug/",
                        hintr_response$data$id))
  expect_equal(httr::status_code(hintr_r), 200)
  hintr2_r <- httr::GET(paste0(hintr2_server$url, "/model/debug/",
                              hintr2_response$data$id))
  expect_equal(httr::status_code(hintr2_r), 200)

  bin <- httr::content(hintr_r, "raw")
  zip <- tempfile(fileext = ".zip")
  writeBin(bin, zip)
  tmp <- tempfile()
  dir.create(tmp)
  zip::unzip(zip, exdir = tmp)
  expect_equal(dir(tmp), hintr_response$data$id)
  expect_setequal(dir(file.path(tmp, hintr_response$data$id)),
                  c("data.rds", "files"))
  dat <- readRDS(file.path(tmp, hintr_response$data$id, "data.rds"))

  bin <- httr::content(hintr2_r, "raw")
  zip <- tempfile(fileext = ".zip")
  writeBin(bin, zip)
  tmp <- tempfile()
  dir.create(tmp)
  zip::unzip(zip, exdir = tmp)
  expect_equal(dir(tmp), hintr2_response$data$id)
  expect_setequal(dir(file.path(tmp, hintr2_response$data$id)),
                  c("data.rds", "files"))
  dat2 <- readRDS(file.path(tmp, hintr2_response$data$id, "data.rds"))

  ## Bits of the debug endpoint will differ - check some equivalence
  expect_identical(dat$objects$data, dat2$objects$data)
  expect_identical(dat$objects$options, dat2$objects$options)

  ## Result
  hintr_r <- httr::GET(paste0(hintr_server$url, "/model/result/",
                              hintr_response$data$id))
  expect_equal(httr::status_code(hintr_r), 200)
  hintr_response <- response_from_json(hintr_r)
  hintr2_r <- httr::GET(paste0(hintr2_server$url, "/model/result/",
                              hintr2_response$data$id))
  expect_equal(httr::status_code(hintr2_r), 200)
  hintr2_response <- response_from_json(hintr2_r)

  ## Form of errors when none is different between versions
  expect_equal(hintr_response$errors, list())
  hintr_response$errors <- NULL
  expect_equal(hintr2_response$errors, NULL)
  hintr2_response$errors <- NULL
  expect_identical(hintr_response, hintr2_response)

  gc()
})

test_that("plotting metadata is equivalent", {
  hintr_server <- hintr_server()
  hintr2_server <- hintr2_server()

  hintr_r <- httr::GET(paste0(hintr_server$url, "/meta/plotting/MWI"))
  expect_equal(httr::status_code(hintr_r), 200)

  hintr2_r <- httr::GET(paste0(hintr2_server$url, "/meta/plotting/MWI"))
  expect_equal(httr::status_code(hintr2_r), 200)

  ## Identical up to format of errors
  hintr <- response_from_json(hintr_r)
  hintr2 <- response_from_json(hintr2_r)
  ## Format when no errors is different
  expect_equal(hintr$errors, list())
  hintr$errors <- NULL
  expect_equal(hintr2$errors, NULL)
  hintr2$errors <- NULL
  expect_identical(hintr, hintr2)
  gc()
})

test_that("model run options are equivalent", {
  hintr_server <- hintr_server()
  hintr2_server <- hintr2_server()
  payload <- file.path("payload", "model_run_options_payload.json")

  hintr_r <- httr::POST(paste0(hintr_server$url, "/model/options"),
                        body = httr::upload_file(payload),
                        encode = "json")
  expect_equal(httr::status_code(hintr_r), 200)

  hintr2_r <- httr::POST(paste0(hintr2_server$url, "/model/options"),
                         body = httr::upload_file(payload),
                         encode = "json")
  expect_equal(httr::status_code(hintr2_r), 200)

  ## Identical up to format of errors
  hintr <- response_from_json(hintr_r)
  hintr2 <- response_from_json(hintr2_r)
  ## Format when no errors is different
  expect_equal(hintr$errors, list())
  hintr$errors <- NULL
  expect_equal(hintr2$errors, NULL)
  hintr2$errors <- NULL
  expect_identical(hintr, hintr2)
  gc()
})

test_that("version info is equivalent", {
  hintr_server <- hintr_server()
  hintr2_server <- hintr2_server()

  hintr_r <- httr::GET(paste0(hintr_server$url, "/hintr/version"))
  expect_equal(httr::status_code(hintr_r), 200)

  hintr2_r <- httr::GET(paste0(hintr2_server$url, "/hintr/version"))
  expect_equal(httr::status_code(hintr2_r), 200)

  ## Identical up to format of errors
  hintr <- response_from_json(hintr_r)
  hintr2 <- response_from_json(hintr2_r)
  ## Format when no errors is different
  expect_equal(hintr$errors, list())
  hintr$errors <- NULL
  expect_equal(hintr2$errors, NULL)
  hintr2$errors <- NULL
  expect_identical(hintr, hintr2)
  gc()
})

test_that("error formatting is equivalent", {
  hintr_server <- hintr_server()
  hintr2_server <- hintr2_server()

  hintr_r <- httr::GET(paste0(hintr_server$url, "/model/debug/abc"))
  expect_equal(httr::status_code(hintr_r), 400)

  hintr2_r <- httr::GET(paste0(hintr2_server$url, "/model/debug/abc"))
  expect_equal(httr::status_code(hintr2_r), 400)

  ## Identical up to format of empty data section and error key
  hintr <- response_from_json(hintr_r)
  hintr2 <- response_from_json(hintr2_r)
  expect_equal(hintr$data, setNames(list(), list()))
  expect_equal(hintr2$data, NULL)
  expect_identical(hintr$status, hintr2$status)
  expect_equal(names(hintr$errors[[1]]), names(hintr2$errors[[1]]))
  expect_equal(hintr$errors[[1]]$error, hintr2$errors[[1]]$error)
  expect_equal(hintr$errors[[1]]$detail, hintr2$errors[[1]]$detail)
  expect_true(!(hintr$errors[[1]]$key == hintr2$errors[[1]]$key))
  gc()
})

test_that("worker info is equivalent", {
  hintr_server <- hintr_server()
  hintr2_server <- hintr2_server()

  hintr_r <- httr::GET(paste0(hintr_server$url, "/hintr/worker/status"))
  expect_equal(httr::status_code(hintr_r), 200)

  hintr2_r <- httr::GET(paste0(hintr2_server$url, "/hintr/worker/status"))
  expect_equal(httr::status_code(hintr2_r), 200)

  ## Format of errors different, names of workers different
  hintr <- response_from_json(hintr_r)
  hintr2 <- response_from_json(hintr2_r)
  ## Format when no errors is different
  expect_equal(hintr$errors, list())
  expect_equal(hintr2$errors, NULL)
  expect_equal(length(hintr$data), length(hintr2$data))
  gc()
})

test_that("spectrum file download is equivalent", {
  test_mock_model_available()
  hintr_server <- hintr_server()
  hintr2_server <- hintr2_server()

  ## Run a model
  payload <- setup_submit_payload()

  hintr_r <- httr::POST(paste0(hintr_server$url, "/model/submit"),
                        body = httr::upload_file(payload),
                        encode = "json")
  expect_equal(httr::status_code(hintr_r), 200)
  hintr_response <- response_from_json(hintr_r)

  hintr2_r <- httr::POST(paste0(hintr2_server$url, "/model/submit"),
                         body = httr::upload_file(payload, type = "application/json"),
                         encode = "json")
  expect_equal(httr::status_code(hintr2_r), 200)
  hintr2_response <- response_from_json(hintr2_r)

  testthat::try_again(4, {
    Sys.sleep(2)
    hintr_r <- httr::GET(paste0(hintr_server$url, "/download/spectrum/", hintr_response$data$id))
    expect_equal(httr::status_code(hintr_r), 200)
    hintr2_r <- httr::GET(paste0(hintr2_server$url, "/download/spectrum/", hintr2_response$data$id))
    expect_equal(httr::status_code(hintr2_r), 200)
  })

  ## Identical content
  expect_identical(hintr_r$content, hintr2_r$content)

  ## Download summary
  testthat::try_again(4, {
    Sys.sleep(2)
    hintr_r <- httr::GET(paste0(hintr_server$url, "/download/summary/", hintr_response$data$id))
    expect_equal(httr::status_code(hintr_r), 200)
    hintr2_r <- httr::GET(paste0(hintr2_server$url, "/download/summary/", hintr2_response$data$id))
    expect_equal(httr::status_code(hintr2_r), 200)
  })

  ## Identical content
  expect_identical(hintr_r$content, hintr2_r$content)
  gc()
})

test_that("404 schema is equivalent", {
  hintr_server <- hintr_server()
  hintr2_server <- hintr2_server()

  hintr_r <- httr::GET(paste0(hintr_server$url, "/meaning-of-life"))
  expect_equal(httr::status_code(hintr_r), 404)

  hintr2_r <- httr::GET(paste0(hintr2_server$url, "/meaning-of-life"))
  expect_equal(httr::status_code(hintr2_r), 404)

  ## Content identical up to error key
  hintr <- response_from_json(hintr_r)
  hintr2 <- response_from_json(hintr2_r)
  expect_equal(hintr$errors[[1]]$error, hintr2$errors[[1]]$error)
  expect_equal(hintr$errors[[1]]$detail, hintr2$errors[[1]]$detail)
  gc()
})

test_that("model run can be cancelled", {
  test_mock_model_available()
  hintr_server <- hintr_server()
  hintr2_server <- hintr2_server()

  ## Run a model and cancel it
  payload <- setup_submit_payload()

  hintr_r <- httr::POST(paste0(hintr_server$url, "/model/submit"),
                        body = httr::upload_file(payload),
                        encode = "json")
  expect_equal(httr::status_code(hintr_r), 200)
  hintr_id <- response_from_json(hintr_r)$data$id

  hintr_r <- httr::POST(paste0(hintr_server$url, "/model/cancel/", hintr_id))
  expect_equal(httr::status_code(hintr_r), 200)
  hintr <- response_from_json(hintr_r)

  hintr2_r <- httr::POST(paste0(hintr2_server$url, "/model/submit"),
                         body = httr::upload_file(payload, type = "application/json"),
                         encode = "json")
  expect_equal(httr::status_code(hintr2_r), 200)
  hintr2_id <- response_from_json(hintr2_r)$data$id

  ## Note that hintr refactor switches this to a GET endpoint over
  hintr2_r <- httr::GET(paste0(hintr2_server$url, "/model/cancel/", hintr2_id))
  expect_equal(httr::status_code(hintr2_r), 200)
  hintr2 <- response_from_json(hintr2_r)

  ## Equivalent up to format of errors section
  expect_equal(hintr$status, hintr2$status)
  expect_equal(hintr$data, hintr2$data)
  expect_equal(hintr$errors, list())
  expect_equal(hintr2$errors, NULL)

  hintr_r <- httr::GET(paste0(hintr_server$url, "/model/status/", hintr_id))
  expect_equal(httr::status_code(hintr_r), 200)
  hintr <- response_from_json(hintr_r)

  hintr2_r <- httr::GET(paste0(hintr2_server$url, "/model/status/", hintr2_id))
  expect_equal(httr::status_code(hintr2_r), 200)
  hintr2 <- response_from_json(hintr2_r)

  ## ID and Format when no errors are only differences different
  expect_equal(hintr$errors, list())
  hintr$errors <- NULL
  expect_equal(hintr2$errors, NULL)
  hintr2$errors <- NULL
  hintr$data$id <- NULL
  hintr2$data$id <- NULL
  expect_identical(hintr, hintr2)

  hintr_r <- httr::GET(paste0(hintr_server$url, "/model/result/", hintr_id))
  expect_equal(httr::status_code(hintr_r), 400)
  hintr <- response_from_json(hintr_r)

  hintr2_r <- httr::GET(paste0(hintr2_server$url, "/model/result/", hintr2_id))
  expect_equal(httr::status_code(hintr2_r), 400)
  hintr2 <- response_from_json(hintr2_r)

  ## Error key and format of empty data are only differences
  expect_equal(hintr$data, setNames(list(), list()))
  hintr$data <- NULL
  expect_equal(hintr2$data, NULL)
  hintr$data <- NULL
  hintr$errors[[1]]$key <- NULL
  hintr2$errors[[1]]$key <- NULL
  expect_identical(hintr, hintr)
})
