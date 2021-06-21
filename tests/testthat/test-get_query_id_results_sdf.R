library(chemspiderapi)

context("get_query_id_results_sdf")

test_that("fails if no query_id is provided.", {
  
  expect_error(
    get_query_id_results_sdf()
  )
  
})

test_that("fails if a NULL query_id is provided.", {
  
  expect_error(
    get_query_id_results_sdf(query_id = NULL)
  )
  
})

test_that("fails if a NULL status is provided.", {
  
  expect_error(
    get_query_id_results_sdf(query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261")
  )
  
})

test_that("fails if a multiple status are provided.", {
  
  expect_error(
    get_query_id_results_sdf(query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261",
                             status = NULL)
  )
  
})

test_that("fails if no API key is provided.", {
  
  expect_error(
    get_query_id_results_sdf(query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261",
                             status = "Complete")
  )
  
})

test_that("fails if NULL is provided as API key.", {
  
  expect_error(
    get_query_id_results_sdf(query_id = "0c98889f-5e8b-4974-aabb-31ab28c54261",
                             status = "Complete", apikey = NULL)
  )
  
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$get("/2754e139-60e6-475b-9a0a-bf1591cb6a79/results/sdf", function(req, res) {
  res$
    set_status(200)$
    send(charToRaw(paste0("{\"results\":\"H4sIAAAAAAAAC6WUW2+bMBSA3y3lP/hhDyD",
                          "VxscGg6cVKTONiBZgC+tuL1NKmVKpaatcFE1T//sOabJEIdOmY",
                          "FlgPg7fORwjCKV9m3ijyc1CKCnBgC+kTAih4DeTir9MoJ+kEIJ",
                          "QHILjChllslmFB8w2y3/OjQU4BEq/WLSW8jyL5EqE6mQt+TkWg",
                          "bUY3fGN9h06w6K4H8ioa198roUOuu7R3tKlLwEPtYAXixARnGf",
                          "R3ICKutayt3TpS8SFDv2ue7TvSxfL/ntR3BjlH1iK/7VgEZgdj",
                          "igi1aaIMMcJqtsUA4M2xcDwFG3+P/KIYmB0isKJyjDQtKmhIFo",
                          "GRACtWETQ7kND1bEho/QqT0hM37xf3WT1bUxASt0jvQYNH+x0G",
                          "JPN6RJKz4JMIcwLrwLmM1BOwKQL0gHfBWAR0yxkIBzlGgbgTTU",
                          "z6YXPglReAFOp2lqzQUzs918gn1M8hs95sbvxGbMb4FKbYIvKb",
                          "Di6KjHe5o61rnUuC7eCCodTgWu3UclkOaHl42pe1QtU+OKw/nf",
                          "1z5hkWfHl21s7sh+GH5MBu04Hg0G/+Fr2Wb6NteUwiYkPalfmu",
                          "LToCnaVjOsf9bx+2GQIhfnz1PV4FJPpcvn02vPW6zWvpvVs8XR",
                          "3W8959TjzLF7eVZN7Vi7nq2q5mte8ycGny9l943iFo0d+A5/iF",
                          "p3PBgAA\"}")))
})

web <- webfakes::new_app_process(app)

Sys.setenv("GET_QUERY_ID_URL" = web$url())

test_that("returns a proper response.", {
  expect_type(
    get_query_id_results_sdf(query_id = "2754e139-60e6-475b-9a0a-bf1591cb6a79",
                             status = "Complete",
                             apikey = "abcdefghijklmnopqrstuvqxyz123456",
                             decode = TRUE),
    "list"
  )
})

Sys.unsetenv("GET_QUERY_ID_URL")

web$stop()
