#' @title Get the MOL file of a ChemSpider record
#' @description This function is used to download a single .MOL file from ChemSpider.
#' @details "If successful, returns a SDF file containing the single record for the matching record."\cr
#' \cr 
#' Call this endpoint with \code{recordId} as an integer. To save the MOL/SDF file, see the vignette "Saving MOL Files of Chemicals".
#' @param record_id A valid (integer) ChemSpider ID.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return A character string containing the (human-readable) .MOL file.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/records/{recordId}/mol} 
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples
#' \dontrun{
#' ## Get the MOL file for caffeine
#' record_id <- 2424L
#' apikey <- "a valid 32-character ChemSpider apikey"
#' get_record_id_mol(
#'   record_id = record_id, 
#'   apikey = apikey
#'   )
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON
#' @export
get_record_id_mol <- function(
  record_id, 
  apikey
  ) {
  
  .check_record_id(record_id)
  
  .check_apikey(apikey)
  
  header <- list(
    "Content-Type" = "", 
    "apikey" = apikey
    )
  
  base_url <- Sys.getenv(
    "GET_RECORD_ID_URL",
    unset = "https://api.rsc.org/compounds/v1/records/"
    )
  
  url <- paste0(
    base_url, 
    record_id, 
    "/mol"
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
  
  content <- rawToChar(raw_result$content)
  content <- jsonlite::fromJSON(content)

  content
}
