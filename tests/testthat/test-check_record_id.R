library(chemspiderapi)

context("check_record_id")

test_that("check_record_id() fails if no record_id is provided.", {
  expect_error(
    .check_record_id()
  )
})

test_that("check_record_id() fails if a NULL record_id is provided.", {
  expect_error(
    .check_record_id(record_id = NULL)
  )
})

test_that("check_record_id() fails if a record_id is not a numeric vector.", {
  expect_error(
    .check_record_id(record_id = "record_id")
  )
})

test_that("check_record_id() fails if multiple record_ids are provided.", {
  expect_error(
    .check_record_id(record_id = c("123", "456"))
  )
})

test_that("check_record_id() issues a warning when a non-integer record_id is provided.", {
  expect_warning(
    .check_record_id(record_id = 2424)
  )
})

test_that("check_record_id() remains silent when the correct record_id is provided.", {
  expect_silent(
    .check_record_id(record_id = 2424L)
  )
})
