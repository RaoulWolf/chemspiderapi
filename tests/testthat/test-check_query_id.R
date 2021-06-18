library(chemspiderapi)

context("check_query_id")

test_that("check_query_id() fails if no query_id is provided.", {
  expect_error(
    .check_query_id()
  )
})

test_that("check_query_id() fails if a NULL query_id is provided.", {
  expect_error(
    .check_query_id(query_id = NULL)
  )
})

test_that("check_query_id() fails if a query_id is not a character vector.", {
  expect_error(
    .check_query_id(query_id = 123)
  )
})

test_that("check_query_id() fails if multiple query_ids are provided.", {
  expect_error(
    .check_query_id(query_id = c("0c98889f-5e8b-4974-aabb-31ab28c54261", "0c98889f-5e8b-4974-aabb-31ab28c54261"))
  )
})

test_that("check_query_id() fails if a query_id is too long.", {
  expect_error(
    .check_query_id(query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261N")
  )
})

test_that("check_query_id() fails if a query_id is not hyphen divided into five parts.", {
  expect_error(
    .check_query_id(query_id = "0c98889f-5e8b-4974-aabb31ab28c54261N")
  )
})

test_that("check_query_id() fails if the first part a query_id is of the wrong length.", {
  expect_error(
    .check_query_id(query_id = "0c98889fN-5e8-4974-aabb-31ab28c54261")
  )
})

test_that("check_query_id() fails if the second part a query_id is of the wrong length.", {
  expect_error(
    .check_query_id(query_id = "0c98889f-5e8bN-4974-aab-31ab28c54261")
  )
})

test_that("check_query_id() fails if the third part a query_id is of the wrong length.", {
  expect_error(
    .check_query_id(query_id = "0c98889f-5e8b-4974N-aab-31ab28c54261")
  )
})

test_that("check_query_id() fails if the fourth part a query_id is of the wrong length.", {
  expect_error(
    .check_query_id(query_id = "0c98889f-5e8b-4974-aabbN-31ab28c5426")
  )
})

test_that("check_query_id() remains silent when the correct query_id is provided.", {
  expect_silent(
    .check_query_id(query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261")
  )
})
