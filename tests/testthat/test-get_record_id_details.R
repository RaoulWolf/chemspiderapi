library(chemspiderapi)

context("get_record_id_details")

test_that("fails if record ID is wrong", {
  
  expect_error(
    get_record_id_details(record_id = "A wrong query ID")
  )
  
})

test_that("fails if apikey is wrong", {
  
  expect_error(
    get_record_id_details(record_id = 2157L, apikey = "A wrong apikey")
  )
  
})

# app <- webfakes::new_app()
# app$use(webfakes::mw_json())
# app$get("/2424/details?fields=SMILES,Formula", function(req, res) {
#   res$
#     set_status(200)$
#     send(charToRaw(paste0("{\"id\":2424,\"smiles\":\"Cn1cnc2c1c(=O)n(c(=O)n2C",
#                           ")C\",\"formula\":\"C_{8}H_{10}N_{4}O_{2}\"}")))
# })
# 
# web <- webfakes::new_app_process(app)
# 
# Sys.setenv("GET_RECORD_ID_URL" = web$url())
# 
# test_that("get_record_id_details() returns a proper response.", {
#   expect_type(
#     get_record_id_details(record_id = 2424L, fields = c("SMILES", "Formula"),
#                           apikey = "abcdefghijklmnopqrstuvqxyz123456"),
#     "list"
#   )
# })
# 
# Sys.unsetenv("GET_RECORD_ID_URL")
# 
# web$stop()
