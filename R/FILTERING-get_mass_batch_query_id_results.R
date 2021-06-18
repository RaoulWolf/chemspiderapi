#' @title Get results of a mass batch query from ChemSpider
#' @description Get results from a ChemSpider query after \code{get_mass_batch_queryId_status()} returns \code{"Complete"}.
#' @details Before running \code{get_mass_batch_queryId_results()}, make sure \code{get_mass_batch_query_id_status()} returns \code{"Complete"}.
#' @param query_id A valid 36-character query ID, as returned by \code{post_mass_batch()}.
#' @param status status A character string indicating the query status as returned by \code{get_mass_batch_queryId_status()}
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param coerce \code{logical}: should the list be coerced to a data.frame? Defaults to \code{FALSE}.
#' @return Returns the record IDs of a mass batch query 
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/filter/mass/batch/{queryId}/results}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples 
#' \dontrun{
#' ## Obtain the result from a mass batch query
#' apikey <- "a valid 32-character ChemSpider apikey"
#' query_id <- "a valid 36-character ChemSpider queryId"
#' get_mass_batch_query_id_results(query_id = query_id, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON
#' @export
get_mass_batch_query_id_results <- function(
  query_id, 
  status, 
  apikey, 
  coerce = FALSE
  ) {
  
  .check_query_id(query_id)
  
  .check_status(status)
  
  .check_apikey(apikey)
  
  .check_coerce(coerce)
  
  header <- list(
    "Content-Type" = "", 
    "apikey" = apikey
    )
  
  base_url <- Sys.getenv(
    "GET_MASS_BATCH_QUERY_ID_URL", 
    unset = "https://api.rsc.org/compounds/v1/filter/mass/batch/"
    )
  
  url <- paste0(
    base_url, 
    query_id, 
    "/results"
    )
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(
    handle = handle, 
    customrequest = "GET"
    )
  
  curl::handle_setheaders(
    handle = handle, 
    .list = header
    )
  
  raw_result <- curl::curl_fetch_memory(
    url = url, 
    handle = handle
    )
  
  .check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  
  if (coerce) {
    result <- as.data.frame(
      result, 
      stringsAsFactors = FALSE
      )
  }
  
  result
}
