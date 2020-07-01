context("survey-and-programme")

test_that("endpoint_validate_survey_programme supports programme file", {
  input <- validate_programme_survey_input(
    file.path("testdata", "programme.csv"),
    "programme",
    file.path("testdata", "malawi.geojson"))
  response <- validate_survey_programme(input)

  expect_equal(response$filename, scalar("original"))
  expect_equal(response$hash, scalar("12345"))
  ## Sanity check that data has been returned
  expect_true(nrow(response$data) >= 200)
  expect_equal(typeof(response$data[, "current_art"]), "double")
})

test_that("endpoint_validate_survey_programme returns error on invalid programme data", {
  input <- validate_programme_survey_input(
    file.path("testdata", "malformed_programme.csv"),
    "programme",
    file.path("testdata", "malawi.geojson"))
  error <- expect_error(validate_survey_programme(input))

  expect_equal(error$data[[1]]$error, scalar("INVALID_FILE"))
  expect_equal(
    error$data[[1]]$detail,
    scalar("Data missing column year."))
  expect_equal(error$status_code, 400)
})

test_that("endpoint_validate_survey_programme supports ANC file", {
  input <- validate_programme_survey_input(
    file.path("testdata", "anc.csv"),
    "anc",
    file.path("testdata", "malawi.geojson"))
  response <- validate_survey_programme(input)

  expect_equal(response$filename, scalar("original"))
  expect_equal(response$hash, scalar("12345"))
  ## Sanity check that data has been returned
  expect_true(nrow(response$data) >= 200)
  expect_equal(typeof(response$data[, "prevalence"]), "double")
  expect_equal(typeof(response$data[, "art_coverage"]), "double")
})

test_that("endpoint_validate_survey_programme returns error on invalid ANC data", {
  input <- validate_programme_survey_input(
    file.path("testdata", "malformed_anc.csv"),
    "anc",
    file.path("testdata", "malawi.geojson"))
  error <- expect_error(validate_survey_programme(input))

  expect_equal(error$data[[1]]$error, scalar("INVALID_FILE"))
  expect_equal(
    error$data[[1]]$detail,
    scalar(paste0("Anc file contains regions for more than one country. ",
                  "Got countries MWI, AGO.")))
  expect_equal(error$status_code, 400)
})

test_that("endpoint_validate_survey_programme supports survey file", {
  survey <- file.path("testdata", "survey.csv")
  input <- validate_programme_survey_input(
    file.path("testdata", "survey.csv"),
    "survey",
    file.path("testdata", "malawi.geojson"))
  response <- validate_survey_programme(input)

  expect_equal(response$filename, scalar("original"))
  expect_equal(response$hash, scalar("12345"))
  ## Sanity check that data has been returned
  expect_true(nrow(response$data) >= 20000)
  expect_equal(typeof(response$data[, "est"]), "double")
})

test_that("endpoint_validate_survey_programme returns error on invalid survey data", {

  input <- validate_programme_survey_input(
    file.path("testdata", "malformed_survey.csv"),
    "survey",
    file.path("testdata", "malawi.geojson"))
  error <- expect_error(validate_survey_programme(input))

  expect_equal(error$data[[1]]$error, scalar("INVALID_FILE"))
  expect_equal(
    error$data[[1]]$detail,
    scalar("Data missing column survey_id."))
  expect_equal(error$status_code, 400)
})
