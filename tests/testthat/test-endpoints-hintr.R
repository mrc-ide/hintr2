test_that("endpoint worker status works", {
  test_redis_available()
  queue <- test_queue()
  status <- worker_status(queue)

  response <- status()
  expect_equal(unlist(response, FALSE, FALSE), rep("IDLE", 2))

  queue$queue$worker_stop(timeout = 5)
  Sys.sleep(5)
  response <- status()
  expect_equal(unlist(response, FALSE, FALSE), rep("EXITED", 2))

  queue$queue$worker_delete_exited()
  response <- status()
  expect_equal(response, setNames(list(), character()))
})
