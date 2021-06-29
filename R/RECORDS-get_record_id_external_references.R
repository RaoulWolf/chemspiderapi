#' @title Get external references of a record ID
#' @description GET a list of external references for a ChemSpider ID.
#' @details It is recommended to specify which \code{data_sources} to load, as some substances have a substantial amount of references (>10'000). Use \code{chemspiderapi::get_datasources()} for a complete list of all \code{data_sources}.
#' @param record_id A valid (integer) ChemSpider ID.
#' @param data_sources Either a character string, a vector of characters, or a list of characters detailing which \code{data_sources} to look up. If left as is, will return all \code{data_sources}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return A list containing the external references.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/records/{recordId}/externalreferences}
#' @examples 
#' \dontrun{
#' ## Get the PubChem external reference for caffeine
#' record_id <- 2424L
#' data_sources <- "PubChem"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' get_record_id_external_references(record_id = record_id, 
#'                                   data_sources = data_sources, 
#'                                   apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON
#' @export 
get_record_id_external_references <- function(record_id, data_sources = NULL, 
                                              apikey = NULL) {
  
  .check_record_id(record_id)

  .check_data_sources(data_sources)
  
  .check_apikey(apikey)
  
  base_url <- Sys.getenv("GET_RECORD_ID_URL",
                         unset = "https://api.rsc.org/compounds/v1/records/")
  
  if (!is.null(data_sources)) {
    
    if (length(data_sources) > 1) {
      
      data_sources <- paste(data_sources, collapse = ",")
      
    }
    
    url <- paste0(base_url, record_id, "/externalreferences?dataSources=", 
                  data_sources)
    
  } else {
    
    url <- paste0(base_url, record_id, "/externalreferences")
    
  }

  header <- list("Content-Type" = "", "apikey" = apikey)
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle = handle, customrequest = "GET")
  
  curl::handle_setheaders(handle = handle, .list = header)
  
  result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  .check_status_code(result$status_code)
  
  content <- rawToChar(result$content)
  
  content <- jsonlite::fromJSON(content)
  
  content
  
}
