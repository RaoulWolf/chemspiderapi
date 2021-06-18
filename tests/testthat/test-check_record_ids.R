library(chemspiderapi)

context("check_record_ids")

test_that("check_record_ids() fails if no record_ids are provided.", {
  expect_error(
    .check_record_ids()
  )
})

test_that("check_record_ids() fails if NULL record_ids are provided.", {
  expect_error(
    .check_record_ids(record_ids = NULL)
  )
})

test_that("check_record_ids() fails if record_ids are not a numeric vector.", {
  expect_error(
    .check_record_ids(record_ids = c("recordId1", "recordId2"))
  )
})

test_that("check_record_ids() fails if only a single recordId is provided.", {
  expect_error(
    .check_record_ids(record_ids = 2424L)
  )
})

test_that("check_record_ids() fails if more than 100 record_ids are provided.", {
  expect_error(
    .check_record_ids(record_ids = 1:101)
  )
})


test_that("check_record_ids() issues a warning when a non-integer record_ids is provided.", {
  expect_warning(
    .check_record_ids(record_ids = c(2424, 2345))
  )
})

test_that("check_record_ids() remains silent when the correct record_ids is provided.", {
  expect_silent(
    .check_record_ids(record_ids = c(2424L, 2345L))
  )
})
