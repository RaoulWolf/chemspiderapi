library(chemspiderapi)

context("get_data_sources")

test_that("get_data_sources() returns nothing if API key length is wrong.", {
  expect_error(
    get_data_sources(
      apikey = "A wrong API key"
      )
    )
})

test_that("get_data_sources() returns nothing if API key type is wrong.", {
  expect_error(
    get_data_sources(
      apikey = 1234567890L
      )
    )
})

test_that("get_data_sources() returns nothing if multiple API keys are provided.", {
  expect_error(
    get_data_sources(
      apikey = c(
        "A wrong API key", 
        "Another wrong API key"
        )
      )
    )
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$get("/", function(req, res) {
  res$
    set_status(200)$
    send(
      charToRaw("{\"dataSources\":[\"abcr\",\"Acros Organics\",\"Activate Scientific\"]}")
      )
})

web <- webfakes::new_app_process(app)

Sys.setenv("GET_DATA_SOURCES_URL" = web$url())

apikey <- paste(
  sample(c(0:9, letters), size = 32, replace = TRUE), 
  collapse = ""
  )

test_that("get_data_sources() returns a proper response.", {
  expect_type(
    get_data_sources(
      apikey = apikey
      ),
    "list"
  )
})

Sys.unsetenv("GET_DATA_SOURCES_URL")

web$stop()
