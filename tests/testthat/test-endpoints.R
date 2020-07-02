context("endpoints")

gc()

test_that("endpoint_plotting_metadata gets metadata", {
  response <- plotting_metadata("MWI")

  expect_true(all(names(response) %in%
                    c("survey", "anc", "output", "programme")))
  expect_equal(names(response$survey), "choropleth")
  expect_equal(names(response$anc), "choropleth")
  expect_equal(names(response$output), c("barchart", "choropleth"))
  expect_equal(names(response$programme), "choropleth")
  expect_length(response$anc$choropleth$indicators, 2)
  expect_equal(response$anc$choropleth$indicators[[1]]$indicator,
               scalar("prevalence"))
  expect_equal(response$anc$choropleth$indicators[[2]]$indicator,
               scalar("art_coverage"))
  expect_equal(response$anc$choropleth$indicators[[1]]$name,
               scalar("HIV prevalence"))
  expect_equal(response$anc$choropleth$indicators[[2]]$name,
               scalar("ART coverage"))
})

test_that("endpoint_plotting_metadata returns default data for missing country", {
  metadata <- testthat::evaluate_promise(plotting_metadata("Missing Country"))
  expect_equal(metadata$messages, paste0("Country with iso3 code Missing ",
   "Country not in metadata - returning default colour scales.\n"))
  response <- metadata$result

  expect_true(all(names(response) %in%
                    c("survey", "anc", "output", "programme")))
  expect_equal(names(response$survey), "choropleth")
  expect_equal(names(response$anc), "choropleth")
  expect_equal(names(response$output), c("barchart", "choropleth"))
  expect_equal(names(response$programme), "choropleth")
  expect_length(response$anc$choropleth$indicators, 2)
  expect_equal(response$anc$choropleth$indicators[[1]]$indicator,
               scalar("prevalence"))
  expect_equal(response$anc$choropleth$indicators[[2]]$indicator,
               scalar("art_coverage"))
  expect_equal(response$anc$choropleth$indicators[[1]]$name,
               scalar("HIV prevalence"))
  expect_equal(response$anc$choropleth$indicators[[2]]$name,
               scalar("ART coverage"))
})
