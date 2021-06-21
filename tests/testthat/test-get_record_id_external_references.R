library(chemspiderapi)

context("get_record_id_external_references")

test_that("fails if no record_id is provided.", {
  
  expect_error(
    get_record_id_external_references()
  )
  
})

test_that("fails if a NULL record_id is provided.", {
  
  expect_error(
    get_record_id_external_references(record_id = NULL)
  )
  
})

test_that("fails if a record_id is not a numeric vector.", {
  
  expect_error(
    get_record_id_external_references(record_id = "record_id")
  )
  
})

test_that("fails if multiple record_id's are provided.", {
  
  expect_error(
    get_record_id_external_references(record_id = c("123", "456"))
  )
  
})

test_that("fails if more than 20 data sources are provided.", {
  
  expect_error(
    get_record_id_external_references(record_id = 2424L, 
                                      data_sources = letters)
  )
  
})

test_that("fails if no API key is provided.", {
  
  expect_error(
    get_record_id_external_references(record_id = 2424L, data_sources = NULL)
  )
  
})

test_that("fails if NULL is provided as API key.", {
  
  expect_error(
    get_record_id_external_references(record_id = 2424L, data_sources = NULL, 
                                      apikey = NULL)
  )
  
})

test_that("fails if more than one API key is provided.", {
  
  expect_error(
    get_record_id_external_references(record_id = 2424L, data_sources = NULL, 
                                      apikey = c("API key one", "API key two"))
  )
  
})

test_that("fails if a numeric API key is provided.", {
  
  expect_error(
    get_record_id_external_references(record_id = 2424L, data_sources = NULL, 
                                      apikey = 1234567890)
  )
  
})

test_that("fails if a non 32-character length API key is provided.", {
  
  expect_error(
    get_record_id_external_references(record_id = 2424L, data_sources = NULL, 
                                      apikey = "abcdefghijklmnopqrstuvqxyz")
  )
  
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$get("/2424/externalreferences", function(req, res) {
  res$
    set_status(200)$
    send(charToRaw(paste0("{\"externalReferences\":[{\"source\":\"1717 CheMal",
                          "l\",\"sourceUrl\":\"http://www.1717chem.com\",\"ex",
                          "ternalId\":\"BT000262\",\"externalUrl\":\"http://3",
                          "bsccorp.com/botanic-extracts-enzymes-and-coenzymes",
                          "/BT000262\"}]}")))
})

web <- webfakes::new_app_process(app)

Sys.setenv("GET_RECORD_ID_URL" = web$url())

test_that("returns a proper response.", {
  
  expect_type(
    get_record_id_external_references(
      record_id = 2424L, apikey = "abcdefghijklmnopqrstuvqxyz123456"),
    "list"
  )
  
})

Sys.unsetenv("GET_RECORD_ID_URL")

web$stop()
