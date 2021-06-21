library(chemspiderapi)

context("check_format")

test_that("fails if no input is provided.", {
  
  expect_error(
    .check_format()
    )
  
})

test_that("fails if NULL is provided as input.", {
  
  expect_error(
    .check_format(input = NULL)
  )
  
})

test_that("fails if multiple inputs are provided.", {
  
  expect_error(
    .check_format(input = c("RYYVLZVUVIJVGH-UHFFFAOYSA-N", 
                            "CN1C=NC2=C1C(=O)N(C(=O)N2C)C"))
  )
  
})

test_that("fails if a non-character input is provided.", {
  
  expect_error(
    .check_format(input = 123)
  )
  
})

test_that("fails if no input_format is provided.", {
  
  expect_error(
    .check_format(input = "RYYVLZVUVIJVGH-UHFFFAOYSA-N", 
                  output_format = "SMILES")
  )
  
})

test_that("fails if NULL is provided as inputFormat.", {
  
  expect_error(
    .check_format(input = "RYYVLZVUVIJVGH-UHFFFAOYSA-N", 
                  input_format = NULL, output_format = "SMILES")
  )
  
})

test_that("fails if no outputFormat is provided.", {
  
  expect_error(
    .check_format(input = "RYYVLZVUVIJVGH-UHFFFAOYSA-N", 
                  input_format = "InChIKey")
  )
  
})

test_that("fails if NULL is provided as output_format.", {
  
  expect_error(
    .check_format(input = "RYYVLZVUVIJVGH-UHFFFAOYSA-N", 
                  input_format = "InChIKey", output_format = NULL)
  )
  
})

test_that("remains silent when correct inputs are provided.", {
  
  expect_silent(
    .check_format(input = "RYYVLZVUVIJVGH-UHFFFAOYSA-N", 
                  input_format = "InChIKey", output_format = "InChI")
  )
  
})

test_that("fails if the inchi string is incomplete.", {
  
  expect_error(
    .check_format(input = "C2H6O/c1-2-3/h3H,2H2,1H3", 
                  input_format = "InChI", output_format = "SMILES")
  )
  
})
