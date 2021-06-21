library(chemspiderapi)

context("get_data_sources")

test_that("returns nothing if API key length is wrong.", {
  
  expect_error(
    get_data_sources(apikey = "A wrong API key")
  )
  
})

test_that("returns nothing if API key type is wrong.", {
  
  expect_error(
    get_data_sources(apikey = 1234567890)
  )
  
})

test_that("returns nothing if multiple API keys are provided.", {
  
  expect_error(
    get_data_sources(apikey = c("A wrong API key", "Another wrong API key"))
  )
  
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$get("/", function(req, res) {
  res$
    set_status(200)$
    send(
      charToRaw(paste0("{\"dataSources\":[\"abcr\",\"Acros Organics\",\"Activ",
                       "ate Scientific\"]}"))
      )
})

web <- webfakes::new_app_process(app)

Sys.setenv("GET_DATA_SOURCES_URL" = web$url())

test_that("returns a proper response.", {
  
  expect_type(
    get_data_sources(apikey = "abcdefghijklmnopqrstuvqxyz123456"),
    "list"
  )
  
})

Sys.unsetenv("GET_DATA_SOURCES_URL")

web$stop()
