#' @title Get results of a ChemSpider query
#' @description This function is used to retrieve the results of a ChemSpider query after \code{chemspiderapi::get_queryId_status()} returns \code{"Complete"}.
#' @details Before running \code{chemspiderapi::get_queryId_results()}, make sure \code{chemspiderapi::get_queryId_status()} returns \code{"Complete"}. In fact, this function will return \code{"NA"} if the status is not \code{"Complete"}.\cr
#' \cr
#' If the results have been truncated because there were more results than the maximum number of results permitted (by default, 10,000), a warning is issued. If this happens, you can split your requests into smaller batches.\cr
#' \cr
#' To batch your requests, call this function with two optional parameters, \code{start} and \code{count}. Both take integer values. \code{start} is the number of the record to start with (zero-based), and \code{count} is the number of records to return. For example, to request results 200-300, use \code{start = 200L} and \code{count = 100L}.
#' @param query_id A valid 36-character ChemSpider query ID string; see Details.
#' @param start Optional: An integer value giving the position from which to start the retrieval of query results. See Details.
#' @param count Optional: An integer value giving the the number of query results to retrieve. See Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param status A character string indicating the query status as returned by \code{chemspiderapi::get_queryId_status()}
#' @return A character vector indicating the status of the query; see Details.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/filter/{queryId}/results}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples
#' \dontrun{
#' ## Get results of a query from ChemSpider
#' query_id <- "A valid 36-character Chemspider query ID"
#' apikey <- "A valid 32-character Chemspider API key"
#' status <- get_query_id_status(query_id = query_id, apikey = apikey)
#' get_query_id_results(query_id = query_id, status = status, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON
#' @export
get_query_id_results <- function(query_id, status, start = NULL, count = NULL,
                                 apikey = NULL) {
  
  .check_query_id(query_id)
  
  .check_status(status)
  
  .check_start_and_count(start, count)
  
  .check_apikey(apikey)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  base_url <- Sys.getenv(
    "GET_QUERY_ID_URL", 
    unset = "https://api.rsc.org/compounds/v1/filter/"
    )
  
  if (is.null(start) && is.null(count)) {
    url <- paste0(base_url, query_id, "/results")
  }
  
  if (!is.null(start) && is.null(count)) {
    url <- paste0(base_url, query_id, "/results?start=", start)
  }
  
  if (is.null(start) && !is.null(count)) {
    url <- paste0(base_url, query_id, "/results?count=", count)
  }
  
  if (!is.null(start) && !is.null(count)) {
    url <- paste0(base_url, query_id, "/results?start=", start, "&count=", 
                  count)
  }
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle = handle, customrequest = "GET")
  
  curl::handle_setheaders(handle = handle, .list = header)
  
  result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  .check_status_code(result$status_code)
  
  .check_limited_to_max_allowed(result)

  content <- rawToChar(result$content)
  
  content <- jsonlite::fromJSON(content)
  
  content
}
