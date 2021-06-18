library(chemspiderapi)

context("get_mass_batch_query_id_status")

test_that("get_mass_batch_query_id_status() fails if no query_id is provided.", {
  expect_error(
    get_mass_batch_query_id_status()
  )
})

test_that("get_mass_batch_query_id_status() fails if a NULL query_id is provided.", {
  expect_error(
    get_mass_batch_query_id_status(query_id = NULL)
  )
})

test_that("get_mass_batch_query_id_status() fails if a query_id is not a character vector.", {
  expect_error(
    get_mass_batch_query_id_status(query_id = 123)
  )
})

test_that("get_mass_batch_query_id_status() fails if multiple query_ids are provided.", {
  expect_error(
    get_mass_batch_query_id_status(query_id = c("0c98889f-5e8b-4974-aabb-31ab28c54261", "0c98889f-5e8b-4974-aabb-31ab28c54261"))
  )
})

test_that("get_mass_batch_query_id_status() fails if a query_id is too long.", {
  expect_error(
    get_mass_batch_query_id_status(query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261N")
  )
})

test_that("get_mass_batch_query_id_status() fails if a query_id is not hyphen divided into five parts.", {
  expect_error(
    get_mass_batch_query_id_status(query_id = "0c98889f-5e8b-4974-aabb31ab28c54261N")
  )
})

test_that("get_mass_batch_query_id_status() fails if the first part a query_id is of the wrong length.", {
  expect_error(
    get_mass_batch_query_id_status(query_id = "0c98889fN-5e8-4974-aabb-31ab28c54261")
  )
})

test_that("get_mass_batch_query_id_status() fails if the second part a query_id is of the wrong length.", {
  expect_error(
    get_mass_batch_query_id_status(query_id = "0c98889f-5e8bN-4974-aab-31ab28c54261")
  )
})

test_that("get_mass_batch_query_id_status() fails if the third part a query_id is of the wrong length.", {
  expect_error(
    get_mass_batch_query_id_status(query_id = "0c98889f-5e8b-4974N-aab-31ab28c54261")
  )
})

test_that("get_mass_batch_query_id_status() fails if the fourth part a query_id is of the wrong length.", {
  expect_error(
    get_mass_batch_query_id_status(query_id = "0c98889f-5e8b-4974-aabbN-31ab28c5426")
  )
})

test_that("get_mass_batch_query_id_status() fails if no API key is provided.", {
  expect_error(
    get_mass_batch_query_id_status(query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261")
  )
})

test_that("get_mass_batch_query_id_status() fails if NULL is provided as API key.", {
  expect_error(
    get_mass_batch_query_id_status(query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261", apikey = NULL)
  )
})

test_that("get_mass_batch_query_id_status() fails if more than one API key is provided.", {
  expect_error(
    get_mass_batch_query_id_status(query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261", apikey = c("API key one", "API key two"))
  )
})

test_that("get_mass_batch_query_id_status() fails if a numeric API key is provided.", {
  expect_error(
    get_mass_batch_query_id_status(query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261", apikey = 1234567890)
  )
})

test_that("get_mass_batch_query_id_status() fails if a logical API key is provided.", {
  expect_error(
    get_mass_batch_query_id_status(query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261", apikey = TRUE)
  )
})

test_that("get_mass_batch_query_id_status() fails if a non 32-character length API key is provided.", {
  expect_error(
    get_mass_batch_query_id_status(query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261", apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$get("/fe7fe60b-0b67-4b24-9d9b-1cf01b75f844/status", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"count\":2,\"message\":\"\",\"status\":\"Complete\"}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("GET_MASS_BATCH_QUERY_ID_URL" = web$url())

test_that("get_mass_batch_query_id_status() returns a proper response.", {
  expect_type(
    get_mass_batch_query_id_status(
      query_id = "fe7fe60b-0b67-4b24-9d9b-1cf01b75f844",
      apikey = "abcdefghijklmnopqrstuvqxyz123456",
      coerce = TRUE
      ),
    "list"
  )
})

Sys.unsetenv("GET_MASS_BATCH_QUERY_ID_URL")

web$stop()
