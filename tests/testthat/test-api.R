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
  null <- "null"
  class(null) <- "logical"
  expect_equal(response$data$filters, scalar(null))
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
