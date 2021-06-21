library(chemspiderapi)

context("get_mass_batch_query_id_results")

test_that("fails if no query_id is provided.", {
  
  expect_error(
    get_mass_batch_query_id_results()
  )
  
})

test_that("fails if a NULL query_id is provided.", {
  
  expect_error(
    get_mass_batch_query_id_results(query_id = NULL)
  )
  
})

test_that("fails if a query_id is not a character vector.", {
  
  expect_error(
    get_mass_batch_query_id_results(query_id = 123)
  )
  
})

test_that("fails if multiple query_ids are provided.", {
  
  expect_error(
    get_mass_batch_query_id_results(
      query_id = c("0c98889f-5e8b-4974-aabb-31ab28c54261", 
                   "0c98889f-5e8b-4974-aabb-31ab28c54261"))
  )
  
})

test_that("fails if a query_id is too long.", {
  
  expect_error(
    get_mass_batch_query_id_results(
      query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261N")
  )
  
})

test_that("fails if a query_id is not hyphen divided into five parts.", {
  
  expect_error(
    get_mass_batch_query_id_results(
      query_id = "0c98889f-5e8b-4974-aabb31ab28c54261N")
  )
  
})

test_that("fails if the first part a query_id is of the wrong length.", {
  
  expect_error(
    get_mass_batch_query_id_results(
      query_id = "0c98889fN-5e8-4974-aabb-31ab28c54261")
  )
  
})

test_that("fails if the second part a query_id is of the wrong length.", {
  
  expect_error(
    get_mass_batch_query_id_results(
      query_id = "0c98889f-5e8bN-4974-aab-31ab28c54261")
  )
  
})

test_that("fails if the third part a query_id is of the wrong length.", {
  
  expect_error(
    get_mass_batch_query_id_results(
      query_id = "0c98889f-5e8b-4974N-aab-31ab28c54261")
  )
  
})

test_that("fails if the fourth part a query_id is of the wrong length.", {
  
  expect_error(
    get_mass_batch_query_id_results(
      query_id = "0c98889f-5e8b-4974-aabbN-31ab28c5426")
  )
  
})

test_that("fails if no status is provided.", {
  
  expect_error(
    get_mass_batch_query_id_results(
      query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261")
  )
  
})

test_that("fails if a NULL status is provided.", {
  
  expect_error(
    get_mass_batch_query_id_results(
      query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = NULL)
  )
  
})

test_that("fails if a multiple status are provided.", {
  
  expect_error(
    get_mass_batch_query_id_results(
      query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261", 
      status = c("Complete", "Complete"))
  )
  
})

test_that("fails if a non-character status is provided.", {
  
  expect_error(
    get_mass_batch_query_id_results(
      query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = 123)
  )
  
})

test_that("fails if the status is not complete.", {
  
  expect_error(
    get_mass_batch_query_id_results(
      query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Incomplete")
  )
  
})

test_that("fails if no API key is provided.", {
  
  expect_error(
    get_mass_batch_query_id_results(
      query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete")
  )
  
})

test_that("fails if NULL is provided as API key.", {
  
  expect_error(
    get_mass_batch_query_id_results(
      query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", 
      apikey = NULL)
  )
  
})

test_that("fails if more than one API key is provided.", {
  
  expect_error(
    get_mass_batch_query_id_results(
      query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", 
      apikey = c("API key one", "API key two"))
  )
  
})

test_that("fails if a numeric API key is provided.", {
  
  expect_error(
    get_mass_batch_query_id_results(
      query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", 
      apikey = 1234567890)
  )
  
})

test_that("fails if a logical API key is provided.", {
  
  expect_error(
    get_mass_batch_query_id_results(
      query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", 
      apikey = TRUE)
  )
  
})

test_that("fails if a non 32-character length API key is provided.", {
  
  expect_error(
    get_mass_batch_query_id_results(
      query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", 
      apikey = "abcdefghijklmnopqrstuvqxyz")
  )
  
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$get("/fe7fe60b-0b67-4b24-9d9b-1cf01b75f844/results", function(req, res) {
  res$
    set_status(200)$
    send(charToRaw(paste0("{\"batchResults\":[{\"formula\":\"C9H8O4\",\"resul",
                          "ts\":[954,99021436]},{\"formula\":\"C17H21NO4\",\"",
                          "results\":[2724,24503676,98641190]}]}")))
})

web <- webfakes::new_app_process(app)

Sys.setenv("GET_MASS_BATCH_QUERY_ID_URL" = web$url())

test_that("returns a proper response.", {
  expect_type(
    get_mass_batch_query_id_results(
      query_id = "fe7fe60b-0b67-4b24-9d9b-1cf01b75f844", status = "Complete",
      apikey = "abcdefghijklmnopqrstuvqxyz123456"),
    "list"
  )
})

Sys.unsetenv("GET_MASS_BATCH_QUERY_ID_URL")

web$stop()
