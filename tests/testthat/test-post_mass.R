library(chemspiderapi)

context("post_mass")

test_that("post_mass() fails if non-numeric mass is provided.", {
  expect_error(
    post_mass(mass = "hundredfifty", range = 0.002)
  )
})

test_that("post_mass() fails if non-numeric range is provided.", {
  expect_error(
    post_mass(mass = 150, range = "naughtpointzerozerotwo")
  )
})

test_that("post_mass() fails if mass is too small.", {
  expect_error(
    post_mass(mass = 0.1, range = 0.002)
  )
})

test_that("post_mass() fails if mass is too large.", {
  expect_error(
    post_mass(mass = 11100, range = 0.002)
  )
})

test_that("post_mass() fails if range is too small.", {
  expect_error(
    post_mass(mass = 150, range = 0.00001)
  )
})

test_that("post_mass() fails if range is too large.", {
  expect_error(
    post_mass(mass = 150, range = 1000)
  )
})

test_that("post_mass() fails if mass and range do not have the same length.", {
  expect_error(
    post_mass(mass = c(150, 140, 120), range = c(0.002, 0.001))
  )
})

test_that("post_mass() fails if more than 20 data sources are provided.", {
  expect_error(
    post_mass(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002),
              data_sources = letters)
  )
})

test_that("post_mass() fails if more than one order_by is provided.", {
  expect_error(
    post_mass(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), data_sources = NULL,
                order_by = c("recordid", "massdefect"), order_direction = NULL)
  )
})

test_that("post_mass() fails if a false order_by is provided.", {
  expect_error(
    post_mass(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), data_sources = NULL,
                order_by = "thewrongthing", order_direction = NULL)
  )
})

test_that("post_mass() fails if a non-character order_by is provided.", {
  expect_error(
    post_mass(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), data_sources = NULL,
                order_by = 123, order_direction = NULL)
  )
})

test_that("post_mass() fails if more than one order_direction is provided.", {
  expect_error(
    post_mass(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), data_sources = NULL,
                order_by = NULL, order_direction = c("ascending", "descending"))
  )
})

test_that("post_mass() fails if a non-character order_direction is provided.", {
  expect_error(
    post_mass(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), data_sources = NULL,
                order_by = NULL, order_direction = 123)
  )
})

test_that("post_mass() fails if a false order_direction is provided.", {
  expect_error(
    post_mass(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), data_sources = NULL,
                order_by = NULL, order_direction = "thewrongthing")
  )
})

test_that("post_mass() fails if no API key is provided.", {
  expect_error(
    post_mass(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), data_sources = NULL,
                 order_by = "recordId", order_direction = "ascending")
  )
})

test_that("post_mass() fails if NULL is provided as API key.", {
  expect_error(
    post_mass(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), data_sources = NULL,
                 order_by = "recordId", order_direction = "ascending",
                 apikey = NULL)
  )
})

test_that("post_mass() fails if more than one API key is provided.", {
  expect_error(
    post_mass(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), data_sources = NULL,
                 order_by = "recordId", order_direction = "ascending",
                 apikey = c("API key one", "API key two"))
  )
})

test_that("post_mass() fails if a numeric API key is provided.", {
  expect_error(
    post_mass(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), data_sources = NULL,
                 order_by = "recordId", order_direction = "ascending",
                 apikey = 1234567890)
  )
})

test_that("post_mass() fails if a logical API key is provided.", {
  expect_error(
    post_mass(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), data_sources = NULL,
                 order_by = "recordId", order_direction = "ascending",
                 apikey = TRUE)
  )
})

test_that("post_mass() fails if a non 32-character length API key is provided.", {
  expect_error(
    post_mass(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), data_sources = NULL,
                 order_by = "recordId", order_direction = "ascending",
                 apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})

test_that("post_mass() fails if a wrong coerce is provided.", {
  expect_error(
    post_mass(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), data_sources = NULL,
              order_by = "recordId", order_direction = "ascending",
              apikey = "abcdefghijklmnopqrstuvqxyz123456", coerce = "wrong")
  )
})

test_that("post_mass() fails if a wrong simplify is provided.", {
  expect_error(
    post_mass(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), data_sources = NULL,
              order_by = "recordId", order_direction = "ascending",
              apikey = "abcdefghijklmnopqrstuvqxyz123456", simplify = "wrong")
  )
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$post("/", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"queryId\":\"e95e0aea-ee69-4590-8e94-5addc43a3876\"}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("POST_MASS_URL" = web$url())

test_that("post_mass() returns a proper response.", {
  expect_type(
    post_mass(mass = 194,
              range = 0.002,
              apikey = "abcdefghijklmnopqrstuvqxyz123456",
              coerce = TRUE, simplify = TRUE),
    "character"
  )
})

test_that("post_mass() returns a proper response.", {
  expect_type(
    post_mass(mass = 194,
              range = 0.002,
              data_sources = "PubMed",
              apikey = "abcdefghijklmnopqrstuvqxyz123456",
              coerce = TRUE, simplify = TRUE),
    "character"
  )
})

Sys.unsetenv("POST_MASS_URL")

web$stop()
