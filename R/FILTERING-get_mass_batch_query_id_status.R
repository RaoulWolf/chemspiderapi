#' GET the status for a mass batch query from ChemSpider
#' 
#' This function is used to return the status of a query from \code{post_mass_batch()}.
#' 
#' Call this endpoint with a \code{queryId} obtained from \code{post_mass_batch()}.\cr
#' \cr
#' If the query is still ongoing, returns a warning and a character vector of the query status as \code{Incomplete}. It is recommended to wait at least ten seconds before checking the status again.\cr
#' \cr
#' If the query is finalized, returns a data frame of the query status with \code{status}, \code{count} and \code{message}. The \code{status} can be either \code{Complete}, \code{Suspended}, \code{Failed}, or \code{Not Found}.\cr
#' \cr
#' Says ChemSpider:\cr
#' \cr
#' \emph{"A status of Suspended can be caused if the results could not be compiled within a reasonable amount of time. Create a new filter request with more restrictive parameters.\cr
#' \cr
#' A status of Failed can be caused if the backend system could not compile the results. Create a new filter request and, if the same outcome occurs, apply more restrictive parameters.\cr
#' \cr
#' A status of Not Found can be caused if the Query ID has not been registered or has expired. Create a new filter request."}\cr
#' \cr
#' If both \code{count} and \code{message} are set to \code{FALSE}, \code{get_mass_batch_queryId_status()} returns the \code{status} as character vector.\cr
#' \cr
#' If the status is \code{Complete}, the results of the query can be obtained from \code{get_mass_batch_results()}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{map_chr()}.\cr
#' 
#' @param query_id A valid 36-character ChemSpider query ID obtained from \code{post_mass_batch()}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the query status as \code{list}, \code{data.frame} or character \code{vector}.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/filter/mass/batch/{queryId}/status}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples 
#' \dontrun{
#' ## Get the status of a mass batch query from ChemSpider
#' queryId <- "a valid 36-character ChemSpider queryId"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' get_mass_batch_queryId_status(queryId = queryId, apikey = apikey)}
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON
#' @export
get_mass_batch_query_id_status <- function(query_id, apikey = NULL) {
  
  .check_query_id(query_id) 
  
  .check_apikey(apikey)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  base_url <- Sys.getenv(
    "GET_MASS_BATCH_QUERY_ID_URL", 
    unset = "https://api.rsc.org/compounds/v1/filter/mass/batch/"
    )
  
  url <- paste0(base_url, query_id, "/status")
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle = handle, customrequest = "GET")
  
  curl::handle_setheaders(handle = handle, .list = header)
  
  result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  .check_status_code(result$status_code)
  
  content <- rawToChar(result$content)
  
  content <- jsonlite::fromJSON(content)
  
  content
}
