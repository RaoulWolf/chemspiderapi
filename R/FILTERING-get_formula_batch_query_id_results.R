#' @title Get results of a formula batch query from ChemSpider
#' @description Get results of a formula batch query from ChemSpider after \code{get_formula_batch_queryId_status()} returns \code{"Complete"}.
#' @details Before running \code{get_formula_batch_queryId_results()}, make sure \code{get_formula_batch_queryId_status()} returns \code{"Complete"}.
#' @param query_id A valid 36-character query ID, as returned by \code{post_formula_batch()}.
#' @param status A character string indicating the query status as returned by \code{get_formula_batch_queryId_status()}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the (integer) ChemSpider IDs as \code{list} or \code{data.frame}.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/filter/formula/batch/{queryId}/results}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples 
#' \dontrun{
#' ## Obtain the result from a mass batch query
#' apikey <- "a valid 32-character ChemSpider API key"
#' query_id <- "a valid 36-character ChemSpider query ID"
#' status <- get_formula_batch_queryId_status(query_id = query_id, apikey = apikey)
#' get_formula_batch_query_id_results(query_id = query_id, status = status, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON
#' @export
get_formula_batch_query_id_results <- function(query_id, status, 
                                               apikey = NULL) {
  
  .check_query_id(query_id)
  
  .check_status(status)
  
  .check_apikey(apikey)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  base_url <- Sys.getenv(
    "GET_FORMULA_BATCH_QUERY_ID_URL", 
    unset = "https://api.rsc.org/compounds/v1/filter/formula/batch/"
    )
  
  url <- paste0(base_url, query_id, "/results")
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle = handle, customrequest = "GET")
  
  curl::handle_setheaders(handle = handle, .list = header)
  
  result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  .check_status_code(result$status_code)
  
  content <- rawToChar(result$content)
  
  content <- jsonlite::fromJSON(content)
  
  content
}
