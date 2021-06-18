library(chemspiderapi)

context("get_query_id_results")

test_that("get_query_id_results() fails if query ID is wrong", {
  expect_error(
    get_query_id_results(query_id = "A wrong query ID",
                         status = "Complete",
                         apikey = "abcdefghijklmnopqrstuvqxyz123456")
    )
})

test_that("get_query_id_results() fails if apikey is wrong", {
  expect_error(
    get_query_id_results(query_id = "abcdefgh-abcd-abcd-abcd-abcdefghijkl",
                        status = "Complete",
                        apikey = "A wrong apikey")
    )
})

test_that("get_query_id_results() returns an NA_integer_ vector if status is Incomplete", {
  expect_error(
    get_query_id_results(query_id = "abcdefgh-abcd-abcd-abcd-abcdefghijkl",
                        status = "Incomplete",
                        apikey = "abcdefghijklmnopqrstuvqxyz123456")
    )
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$get("/abcdefgh-abcd-abcd-abcd-abcdefghijkl/results", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"results\":[{\"formula\":\"C9H8O4\",\"results\":[954,99021436]}]}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("GET_QUERY_ID_URL" = web$url())

test_that("get_query_id_results() returns a proper response.", {
  expect_type(
    get_query_id_results(query_id = "abcdefgh-abcd-abcd-abcd-abcdefghijkl",
                         status = "Complete",
                         apikey = "abcdefghijklmnopqrstuvqxyz123456",
                         coerce = TRUE),
    "list"
  )
})

Sys.unsetenv("GET_QUERY_ID_URL")

web$stop()
