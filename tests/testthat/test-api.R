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

test_that("endpoint_model_submit can be run", {
  queue <- hintr:::Queue$new()
  endpoint <- endpoint_model_submit(queue)
  path <- setup_submit_payload()
  response <- endpoint$run(readLines(path))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_true(!is.null(response$data$id))
})

test_that("api can call endpoint_model_submit", {
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
  res <- api$request("POST", "/meta/plotting/MWI")
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_true(all(names(body$data) %in%
                    c("survey", "anc", "output", "programme")))
})
