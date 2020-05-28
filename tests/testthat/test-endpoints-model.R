context("endpoints-model")

test_that("endpoint model run queues a model run", {
  test_redis_available()
  test_mock_model_available()

  ## Setup payload
  path <- setup_submit_payload()

  ## Call the endpoint
  queue <- hintr:::Queue$new()
  model_submit <- submit_model(queue)
  response <- model_submit(readLines(path))
  expect_true("id" %in% names(response))

  ## Wait for complete and query for status
  ## Query for status
  testthat::try_again(5, {
    result <- queue$queue$task_wait(response$id)
    status_endpoint <- model_status(queue)
    status <- status_endpoint(response$id)
    expect_equal(status$id, response$id)
    expect_equal(status$done, scalar(TRUE))
    expect_equal(status$status, scalar("COMPLETE"))
    expect_equal(status$queue, scalar(0))
    expect_equal(status$success, scalar(TRUE))
    expect_length(status$progress, 2)
    expect_equal(status$progress[[1]]$name, scalar("Started mock model"))
    expect_true(status$progress[[1]]$complete)
    expect_equal(status$progress[[2]]$name, scalar("Finished mock model"))
    expect_false(status$progress[[2]]$complete)
    Sys.sleep(2)
  })

  # ## Get the result
  # res <- MockPlumberResponse$new()
  # model_result <- endpoint_model_result(queue)
  # result <- model_result(NULL, res, status$data$id)
  # result <- jsonlite::parse_json(result)
  # expect_equal(res$status, 200)
  # expect_equal(names(result$data), c("data", "plottingMetadata"))
  # expect_equal(names(result$data$data[[1]]),
  #              c("area_id", "sex", "age_group", "calendar_quarter",
  #                "indicator_id", "mode", "mean", "lower", "upper"))
  # expect_true(length(result$data$data) > 84042)
  # expect_equal(names(result$data$plottingMetadata), c("barchart", "choropleth"))
  #
  #
  # ## Barchart
  # barchart <- result$data$plottingMetadata$barchart
  # expect_equal(names(barchart), c("indicators", "filters", "defaults"))
  # expect_length(barchart$filters, 4)
  # expect_equal(names(barchart$filters[[1]]),
  #              c("id", "column_id", "label", "options", "use_shape_regions"))
  # expect_equal(names(barchart$filters[[2]]),
  #              c("id", "column_id", "label", "options"))
  # ## Choropleth has the correct filters in correct order
  # filters <- lapply(barchart$filters, function(filter) {
  #   filter$column_id
  # })
  # expect_equal(filters[[1]], "area_id")
  # expect_equal(filters[[2]], "calendar_quarter")
  # expect_equal(filters[[3]], "sex")
  # expect_equal(filters[[4]], "age_group")
  # expect_length(barchart$filters[[2]]$options, 3)
  # expect_equal(barchart$filters[[2]]$options[[2]]$id, "CY2018Q3")
  # expect_equal(barchart$filters[[2]]$options[[2]]$label, "September 2018")
  # expect_true(length(barchart$filters[[4]]$options) >= 29)
  # expect_length(barchart$indicators, 10)
  #
  # ## Quarters are in descending order
  # calendar_quarters <-
  #   lapply(barchart$filters[[2]]$options, function(option) {
  #     option$id
  #   })
  # expect_equal(unlist(calendar_quarters),
  #              sort(unlist(calendar_quarters), decreasing = TRUE))
  #
  #
  # ## Barchart indicators are in numeric id order
  # indicators <- lapply(barchart$indicators, function(indicator) {
  #   indicator$indicator
  # })
  # expect_equal(unlist(indicators),
  #              c("population", "prevalence", "plhiv", "art_coverage",
  #                "current_art", "receiving_art", "incidence", "new_infections",
  #                "anc_prevalence", "anc_art_coverage"))
  #
  # ## Choropleth
  # choropleth <- result$data$plottingMetadata$choropleth
  # expect_equal(names(choropleth), c("indicators", "filters"))
  # expect_length(choropleth$filters, 4)
  # expect_equal(names(choropleth$filters[[1]]),
  #              c("id", "column_id", "label", "options", "use_shape_regions"))
  # expect_equal(names(choropleth$filters[[2]]),
  #              c("id", "column_id", "label", "options"))
  # ## Choropleth has the correct filters in correct order
  # filters <- lapply(choropleth$filters, function(filter) {
  #   filter$column_id
  # })
  # expect_equal(filters[[1]], "area_id")
  # expect_equal(filters[[2]], "calendar_quarter")
  # expect_equal(filters[[3]], "sex")
  # expect_equal(filters[[4]], "age_group")
  # expect_length(choropleth$filters[[2]]$options, 3)
  # expect_equal(choropleth$filters[[2]]$options[[2]]$id, "CY2018Q3")
  # expect_equal(choropleth$filters[[2]]$options[[2]]$label, "September 2018")
  # expect_true(length(choropleth$filters[[4]]$options) >= 29)
  # expect_length(choropleth$indicators, 10)
  #
  # ## Quarters are in descending order
  # calendar_quarters <-
  #   lapply(choropleth$filters[[2]]$options, function(option) {
  #     option$id
  #   })
  # expect_equal(unlist(calendar_quarters),
  #              sort(unlist(calendar_quarters), decreasing = TRUE))
  #
  # ## Choropleth indicators are in numeric id order
  # indicators <- lapply(choropleth$indicators, function(indicator) {
  #   indicator$indicator
  # })
  # expect_equal(unlist(indicators),
  #              c("population", "prevalence", "plhiv", "art_coverage",
  #                "current_art", "receiving_art", "incidence", "new_infections",
  #                "anc_prevalence", "anc_art_coverage"))
})

test_that("endpoint_run_model returns error if queueing fails", {
  test_redis_available()
  ## Create request data
  path <- setup_submit_payload()

  ## Create mocks
  queue <- hintr:::Queue$new()
  mock_submit <- function(data, options) { stop("Failed to queue") }

  ## Call the endpoint
  model_submit <- submit_model(queue)
  mockery::stub(model_submit, "queue$submit", mock_submit)
  error <- expect_error(model_submit(readLines(path)))

  expect_equal(error$data[[1]]$error, scalar("FAILED_TO_QUEUE"))
  expect_equal(error$data[[1]]$detail, scalar("Failed to queue"))
  expect_equal(error$status, 400)
})

test_that("running model with old version throws an error", {
  test_redis_available()

  ## Setup payload
  path <- setup_submit_payload('{
                               "hintr": "0.0.12",
                               "naomi": "0.0.15",
                               "rrq": "0.2.1"
                               }')

  ## Call the endpoint
  queue <- hintr:::Queue$new()
  model_submit <- submit_model(queue)
  error <- expect_error(model_submit(readLines(path)))

  expect_equal(error$data[[1]]$error, scalar("VERSION_OUT_OF_DATE"))
  expect_equal(error$data[[1]]$detail, scalar("MODEL_SUBMIT_OLD"))
  expect_equal(error$status, 400)
})

test_that("querying for status of missing job returns useful message", {
  test_redis_available()

  queue <- hintr:::Queue$new()
  status_endpoint <- model_status(queue)
  status <- status_endpoint("ID")
  expect_equal(status$done, json_null())
  expect_equal(status$status, scalar("MISSING"))
  expect_equal(status$success, json_null())
  expect_equal(status$id, scalar("ID"))
})

test_that("endpoint_run_status returns error if query for status fails", {
  test_redis_available()

  ## Create mocks
  queue <- hintr:::Queue$new()
  mock_status <- function(data, parameters) { stop("Failed to get status") }

  ## Call the endpoint
  status_endpoint <- model_status(queue)
  mockery::stub(status_endpoint, "queue$status", mock_status)
  error <- expect_error(status_endpoint("ID"))
  expect_equal(error$data[[1]]$error, scalar("FAILED_TO_RETRIEVE_STATUS"))
  expect_equal(error$data[[1]]$detail, scalar("Failed to get status"))
  expect_equal(error$status_code, 400)
})

test_that("erroring model run returns useful messages", {
  test_redis_available()

  ## Call the endpoint
  queue <- MockQueue$new()
  path <- setup_submit_payload()
  model_submit <- submit_model(queue)
  response <- model_submit(readLines(path))
  expect_true("id" %in% names(response))

  ## Get the status
  endpoint_status <- model_status(queue)
  status <- endpoint_status(response$id)
  expect_equal(status$done, scalar(TRUE))
  expect_equal(status$status, scalar("ERROR"))
  expect_equal(status$success, scalar(FALSE))
  expect_equal(status$id, response$id)

  ## Get the result
  model_result <- endpoint_model_result(queue)
  result <- model_result(req, res, response$data$id)
  result_parsed <- jsonlite::parse_json(result)
  expect_equal(res$status, 400)

  expect_equal(result_parsed$status, "failure")
  expect_length(result_parsed$data, 0)
  expect_length(result_parsed$errors, 1)
  expect_equal(result_parsed$errors[[1]]$error, "MODEL_RUN_FAILED")
  expect_equal(result_parsed$errors[[1]]$detail, "test error")

  trace <- vcapply(result_parsed$errors[[1]]$trace, identity)
  expect_true("rrq:::rrq_worker_main()" %in% trace)
  expect_true("stop(\"test error\")" %in% trace)
  expect_match(trace[[1]], "^# [[:xdigit:]]+$")

  ## Check logging:
  res$headers[["Content-Type"]] <- "application/json"
  res$body <- result
  res$status <- 400
  msg <- capture_messages(
    api_log_end(NULL, NULL, res, NULL))
  expect_match(msg[[1]], "error-key: [a-z]{5}-[a-z]{5}-[a-z]{5}")
  expect_match(msg[[2]], "error-detail: test error")
  expect_match(msg[[3]], "error-trace: rrq:::rrq_worker_main")
})
