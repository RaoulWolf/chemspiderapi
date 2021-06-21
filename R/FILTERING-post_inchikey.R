#' @title Post an InChIKey as ChemSpider query
#' @description Functionality to post an InChIKey to obtain a \code{queryId} for use in \code{chemspiderapi::get_status()} and \code{chemspiderapi::get_results()}.
#' @details The validity criteria for InChIKeys are outlined here: \url{https://www.inchi-trust.org/technical-faq/#13.1}. If certain criteria are not met by the input \code{inchikey}, \code{chemspideR::post_inchikey()} returns an error message (and does not perform an API query). In the case of a non-standard \code{inchikey}, a warning is issued but the query will be performed.
#' @param inchikey A valid 27-character InChIKey; see Details.
#' @param apikey A valid 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the queryId string as (named) character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/inchikey}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## Post the InChIKey of caffeine to obtain a queryId
#' inchikey <- "RYYVLZVUVIJVGH-UHFFFAOYSA-N"
#' apikey <- "A valid 32-character Chemspider API key"
#' post_inchikey(inchikey = inchikey, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_inchikey <- function(inchikey, apikey = NULL) {
  
  .check_inchikey(inchikey)
  
  .check_apikey(apikey)

  data <- list("inchikey" = inchikey)
  
  data <- jsonlite::toJSON(data, auto_unbox = TRUE)

  header <- list("Content-Type" = "", "apikey" = apikey)

  url <- Sys.getenv(
    "POST_INCHIKEY_URL",
    unset = "https://api.rsc.org/compounds/v1/filter/inchikey"
    )
  
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
