#' @title Post a name to obtain a ChemSpider query ID
#' @description Functionality to POST the name of a compound and obtain a ChemSpider query ID for subsequent use in \code{chemspiderapi::get_queryId_status()} and \code{chemspiderapi::get_queryId_results()}.
#' @details Allowed entries for \code{orderBy} are: \code{"recordId"} (default), \code{"massDefect"}, \code{"molecularWeight"}, \code{"referenceCount"}, \code{"dataSourceCount"}, \code{"pubMedCount"}, and \code{"rscCount"}.\cr
#' \cr
#' Allowed entries for \code{orderDirection} are: \code{"ascending"} (default) and \code{"descending"}.
#' @param name A character string of the compound name.
#' @param order_by A character string indicating by which parameter the results should be arranged (NOT case sensitive); see Details.
#' @param order_direction A character string indicating which in which direction the results should be arranged (NOT case sensitive); see Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the queryId string as (named) character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/name}
#' @examples \dontrun{
#' ## Post the name of caffeine to obtain a query ID
#' name <- "caffeine"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_name(name = name, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_name <- function(name, order_by = "recordId", 
                      order_direction = "ascending", apikey = NULL) {
  
  .check_name(name)
  
  .check_order(order_by, order_direction)
  
  .check_apikey(apikey)
  
  data <- list("name" = name, "orderBy" = order_by, 
               "orderDirection" = order_direction)
  
  data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- Sys.getenv("POST_NAME_URL", 
                    unset = "https://api.rsc.org/compounds/v1/filter/name")
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle = handle, customrequest = "POST", 
                      postfields = data)
  
  curl::handle_setheaders(handle = handle, .list = header)
  
  result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  .check_status_code(result$status_code)
  
  content <- rawToChar(result$content)
  
  content <- jsonlite::fromJSON(content)

  content
  
}
