#' @title Get external ChemSpider data sources
#' @description Returns all external ChemSpider data sources.
#' @details "Many other endpoints let you restrict which sources are used to lookup the requested query. Restricting the sources makes queries faster."\cr
#' \cr
#' Returns all available external ChemSpider data sources.\cr
#' \cr
#' This function is most useful for narrowing down \code{data_sources} in other chemspiderapi functions, for example \code{get_record_id_external_references()}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return A \code{list} of characters.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/lookups/datasources}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## Get external data sources of ChemSpider
#' get_data_sources(apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON 
#' @export
get_data_sources <- function(
  apikey = NULL
  ) {
  
  .check_apikey(apikey)
  
  header <- list(
    "Content-Type" = "", 
    "apikey" = apikey
    )
  
  url <- Sys.getenv(
    "GET_DATA_SOURCES_URL", 
    unset = "https://api.rsc.org/compounds/v1/lookups/datasources"
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
  
  result <- curl::curl_fetch_memory(
    url = url, 
    handle = handle
    )

  .check_status_code(result$status_code)

  content <- rawToChar(result$content)
  content <- jsonlite::fromJSON(content)
  
  content
}
