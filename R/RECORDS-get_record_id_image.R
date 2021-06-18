#' @title Get a PNG image of a ChemSpider record
#' @description This function is used to obtain a 250 x 250 pixel PNG image file of a ChemSpider record ID, e.g., after \code{chemspiderapi::get_query_id_results()}.
#' @details Returns a numeric (double) array. To save the picture, see the vignette "Saving PNG Images of Chemicals".
#' @param record_id A valid (integer) ChemSpider ID.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return A numeric array.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/records/{recordId}/image} 
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## Get the PNG image for caffeine
#' record_id <- 2424L
#' apikey <- "a_valid_ChemSpider_API_key"
#' get_record_id_image(
#'   record_id = record_id, 
#'   apikey = apikey
#'   )
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite base64_dec fromJSON
#' @export 
get_record_id_image <- function(
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
    "/image"
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
  
  # if (decode) {
  #   content$image <- jsonlite::base64_dec(content$image)
  # }
  
  content
}
