library(chemspiderapi)

context("check_data_sources")

test_that("check_data_sources() fails if more than 20 data sources are provided.", {
  expect_error(
    .check_data_sources(data_sources = letters)
    )
})

test_that("check_data_sources() is silent if a single data source is provided.", {
  expect_silent(
    .check_data_sources(data_sources = "PubChem")
  )
})

test_that("check_data_sources() stays silent if two data sources are provided.", {
  expect_silent(
    .check_data_sources(data_sources = c("PubChem", "ChEBI"))
  )
})
