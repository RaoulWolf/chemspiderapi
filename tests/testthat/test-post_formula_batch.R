library(chemspiderapi)

context("post_formula_batch")

test_that("post_formula_batch() fails if no formulas are provided.", {
  expect_error(
    post_formula_batch()
  )
})

test_that("post_formula_batch() fails if NULL is provided as formulas.", {
  expect_error(
    post_formula_batch(formulas = NULL)
  )
})

test_that("post_formula_batch() fails if only a single formula is provided.", {
  expect_error(
    post_formula_batch(formulas = "C8H10N4O2")
  )
})

test_that("post_formula_batch() fails if more than 100 formulas are provided.", {
  expect_error(
    post_formula_batch(formulas = rep("C8H10N4O2", times = 101))
  )
})

test_that("post_formula_batch() fails if a non-character formulas are provided.", {
  expect_error(
    post_formula_batch(formulas = c(123, 456))
  )
})

test_that("post_formula_batch() fails if more than 20 data sources are provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"),
                       data_sources = letters)
  )
})

test_that("post_formula_batch() fails if more than one order_by is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), data_sources = NULL,
                order_by = c("recordid", "massdefect"), order_direction = NULL)
  )
})

test_that("post_formula_batch() fails if a false order_by is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), data_sources = NULL,
                order_by = "thewrongthing", order_direction = NULL)
  )
})

test_that("post_formula_batch() fails if a non-character order_by is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), data_sources = NULL,
                order_by = 123, order_direction = NULL)
  )
})

test_that("post_formula_batch() fails if more than one order_direction is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), data_sources = NULL,
                order_by = NULL, order_direction = c("ascending", "descending"))
  )
})

test_that("post_formula_batch() fails if a non-character order_direction is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), data_sources = NULL,
                order_by = NULL, order_direction = 123)
  )
})

test_that("post_formula_batch() fails if a false order_direction is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), data_sources = NULL,
                order_by = NULL, order_direction = "thewrongthing")
  )
})

test_that("post_formula_batch() fails if no API key is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), data_sources = NULL,
                 order_by = "recordId", order_direction = "ascending")
  )
})

test_that("post_formula_batch() fails if NULL is provided as API key.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), data_sources = NULL,
                 order_by = "recordId", order_direction = "ascending",
                 apikey = NULL)
  )
})

test_that("post_formula_batch() fails if more than one API key is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), data_sources = NULL,
                 order_by = "recordId", order_direction = "ascending",
                 apikey = c("API key one", "API key two"))
  )
})

test_that("post_formula_batch() fails if a numeric API key is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), data_sources = NULL,
                 order_by = "recordId", order_direction = "ascending",
                 apikey = 1234567890)
  )
})

test_that("post_formula_batch() fails if a logical API key is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), data_sources = NULL,
                 order_by = "recordId", order_direction = "ascending",
                 apikey = TRUE)
  )
})

test_that("post_formula_batch() fails if a non 32-character length API key is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), data_sources = NULL,
                 order_by = "recordId", order_direction = "ascending",
                 apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$post("/", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"queryId\":\"fe7fe60b-0b67-4b24-9d9b-1cf01b75f844\"}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("POST_FORMULA_BATCH_URL" = web$url())

test_that("post_formula_batch() returns a proper response.", {
  expect_type(
    post_formula_batch(formulas = c("C9H8O4", "C17H21NO4"),
                       apikey = "abcdefghijklmnopqrstuvqxyz123456"),
    "list"
  )
})

test_that("post_formula_batch() returns a proper response.", {
  expect_type(
    post_formula_batch(formulas = c("C9H8O4", "C17H21NO4"),
                       data_sources = "Royal Society of Chemistry",
                       apikey = "abcdefghijklmnopqrstuvqxyz123456",
                       coerce = TRUE),
    "list"
  )
})

Sys.unsetenv("POST_FORMULA_BATCH_URL")

web$stop()
