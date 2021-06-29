library(chemspiderapi)

context("check_order")

test_that("check_order() fails if more than one order_by is provided.", {
  expect_error(
    .check_order(order_by = c("recordid", "massdefect"), order_direction = NULL)
  )
})

test_that("check_order() fails if a false order_by is provided.", {
  expect_error(
    .check_order(order_by = "thewrongthing", order_direction = NULL)
  )
})

test_that("check_order() fails if a non-character order_by is provided.", {
  expect_error(
    .check_order(order_by = 123, order_direction = NULL)
  )
})

test_that("check_order() fails if more than one order_direction is provided.", {
  expect_error(
    .check_order(order_by = NULL, order_direction = c("ascending", "descending"))
  )
})

test_that("check_order() fails if a non-character order_direction is provided.", {
  expect_error(
    .check_order(order_by = NULL, order_direction = 123)
  )
})

test_that("check_order() fails if a false order_direction is provided.", {
  expect_error(
    .check_order(order_by = NULL, order_direction = "thewrongthing")
  )
})

test_that("check_order() remains silent when the correct order is provided.", {
  expect_silent(
    .check_order(order_by = "recordId", order_direction = "descending")
  )
})
