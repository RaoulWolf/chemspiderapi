library(chemspiderapi)

context("get_query_id_status")

test_that("get_query_id_status() returns an NA_integer_ vector if query ID is wrong", {
  expect_error(
    get_query_id_status(query_id = "A wrong query ID",
                       count = FALSE, 
                       message = FALSE,
                       apikey = keyring::key_get("R Keyring Service", "ChemSpider API Key"))
    )
})

test_that("get_query_id_status() returns an NA_character_ vector if apikey is wrong", {
  expect_error(
    get_query_id_status(query_id = "abcdefgh-abcd-abcd-abcd-abcdefghijkl",
                       count = FALSE, 
                       message = FALSE,
                       apikey = "A wrong apikey")
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

Sys.setenv("GET_QUERY_ID_URL" = web$url())

test_that("get_query_id_status() returns a proper response.", {
  expect_type(
    get_query_id_status(query_id = "fe7fe60b-0b67-4b24-9d9b-1cf01b75f844",
                       apikey = "abcdefghijklmnopqrstuvqxyz123456",
                       coerce = TRUE),
    "list"
  )
})

Sys.unsetenv("GET_QUERY_ID_URL")

web$stop()
