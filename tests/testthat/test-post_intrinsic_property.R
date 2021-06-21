library(chemspiderapi)

context("post_intrinsic_property")

test_that("post_intrinsic_property() fails if property is not formula and no mass is provided", {
  expect_error(
    post_intrinsic_property(property = "molecularWeight", range = 0.002)
    )
})

test_that("post_intrinsic_property() fails if property is not formula and no range is provided", {
  expect_error(
    post_intrinsic_property(property = "molecularWeight", mass = 150)
  )
})

test_that("post_intrinsic_property() fails if property is formula and no formula is provided", {
  expect_error(
    post_intrinsic_property(property = "formula")
  )
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$post("/", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"queryId\":\"e95e0aea-ee69-4590-8e94-5addc43a3876\"}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("POST_INTRINSIC_PROPERTY_URL" = web$url())

test_that("post_intrinsic_property() returns a proper response.", {
  expect_type(
    post_intrinsic_property(property = "MolecularWeight",
                            mass = 194, 
                            range = 0.5,
                            apikey = "abcdefghijklmnopqrstuvqxyz123456"),
    "list"
  )
})

Sys.unsetenv("POST_INTRINSIC_PROPERTY_URL")

web$stop()

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$post("/", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"queryId\":\"e95e0aea-ee69-4590-8e94-5addc43a3876\"}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("POST_INTRINSIC_PROPERTY_URL" = web$url())

test_that("post_intrinsic_property() returns a proper response.", {
  expect_type(
    post_intrinsic_property(property = "NominalMass",
                            mass = 194, 
                            range = 0.5,
                            apikey = "abcdefghijklmnopqrstuvqxyz123456"),
    "list"
  )
})

Sys.unsetenv("POST_INTRINSIC_PROPERTY_URL")

web$stop()

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$post("/", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"queryId\":\"e95e0aea-ee69-4590-8e94-5addc43a3876\"}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("POST_INTRINSIC_PROPERTY_URL" = web$url())

test_that("post_intrinsic_property() returns a proper response.", {
  expect_type(
    post_intrinsic_property(property = "AverageMass",
                            mass = 194, 
                            range = 0.5,
                            apikey = "abcdefghijklmnopqrstuvqxyz123456"),
    "list"
  )
})

Sys.unsetenv("POST_INTRINSIC_PROPERTY_URL")

web$stop()

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$post("/", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"queryId\":\"e95e0aea-ee69-4590-8e94-5addc43a3876\"}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("POST_INTRINSIC_PROPERTY_URL" = web$url())


Sys.unsetenv("POST_INTRINSIC_PROPERTY_URL")

web$stop()

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$post("/", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"queryId\":\"e95e0aea-ee69-4590-8e94-5addc43a3876\"}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("POST_INTRINSIC_PROPERTY_URL" = web$url())

test_that("post_intrinsic_property() returns a proper response.", {
  expect_type(
    post_intrinsic_property(property = "Formula",
                            formula = "C8H10N4O2",
                            apikey = "abcdefghijklmnopqrstuvqxyz123456"),
    "list"
  )
})

Sys.unsetenv("POST_INTRINSIC_PROPERTY_URL")

web$stop()
