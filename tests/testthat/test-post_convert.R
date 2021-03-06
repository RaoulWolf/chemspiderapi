library(chemspiderapi)

context("post_convert")

test_that("fails if input is wrong", {
  
  expect_error(
    post_convert(input = "Not an InChIKey", input_format = "InChIKey",
                 output_format = "SMILES")
  )
  
})

test_that("fails if input format is wrong", {
  
  expect_error(
    post_convert(input = "BSYNRYMUTXBXSQ-UHFFFAOYSA-N",
                 input_format = "Not an input format",
                 output_format = "SMILES")
  )
  
})

test_that("fails if output format is wrong", {
  
  expect_error(
    post_convert(input = "BSYNRYMUTXBXSQ-UHFFFAOYSA-N", 
                 input_format = "InChIKey", 
                 output_format = "Not an output format")
  )
  
})

test_that("fails if apikey is wrong", {
  
  expect_error(
    post_convert(input = "BSYNRYMUTXBXSQ-UHFFFAOYSA-N",
                 input_format = "InChIKey", output_format = "SMILES",
                 apikey = "A wrong apikey")
  )
  
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$post("/", function(req, res) {
  res$
    set_status(200)$
    send(charToRaw("{\"output\":\"Cn1cnc2c1c(=O)n(C)c(=O)n2C\"}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("POST_CONVERT_URL" = web$url())

test_that("returns a proper response.", {
  
  expect_type(
    post_convert(input = paste0("InChI=1S/C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(",
                                "3)8(14)11(6)2/h4H,1-3H3"),
                 input_format = "InChI",
                 output_format = "SMILES",
                 apikey = "abcdefghijklmnopqrstuvqxyz123456"),
    "list"
  )
  
})

Sys.unsetenv("POST_CONVERT_URL")

web$stop()
