#' @title Get details for a ChemSpider record
#' @description This function returns record details from ChemSpider.
#' @details "Call this endpoint with a Record ID as an integer.\cr
#' \cr
#' The available fields are: SMILES, Formula, InChI, InChIKey, StdInChI, StdInChIKey, AverageMass, MolecularWeight, MonoisotopicMass, NominalMass, CommonName, ReferenceCount, DataSourceCount, PubMedCount, RSCCount, Mol2D, Mol3D."
#' @param record_id A valid (integer) ChemSpider ID.
#' @param fields Either a single character string, a character vector, or a character list stating which fields to return. Alternatively, \code{"all"} returns all possible \code{fields}. \code{fields} is NOT case sensitive, but see details for a list of possible entries.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param simplify_formula \code{logical}: Should formula strings be simplified? Defaults to \code{FALSE}.
#' @return A \code{data.frame} if multiple columns are returned, or a vector of the appropriate type if only one \code{field} is returned.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/records/{recordId}/details}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## Get the record details for caffeine
#' record_id <- 2424L
#' apikey <- "A valid 32-character Chemspider API key"
#' get_record_id_details(record_id = record_id, 
#'                       fields = c(
#'                         "SMILES", 
#'                         "Formula", 
#'                         "MolecularWeight", 
#'                         "CommonName"
#'                         ), 
#'                       apikey = apikey
#'                       )
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON
#' @export
get_record_id_details <- function(record_id, fields = "all", apikey = NULL, 
                                  simplify_formula = FALSE) {
  
  .check_record_id(record_id)

  .check_fields(fields)

  .check_apikey(apikey)
  
  if (length(fields) == 1) {
    
    if (fields == "all") {
      
      fields <- paste("SMILES", "Formula", "InChI", "InChIKey", "StdInChI", 
                      "StdInChIKey", "AverageMass", "MolecularWeight",
                      "MonoisotopicMass", "NominalMass", "CommonName",
                      "ReferenceCount", "DataSourceCount", "PubMedCount",
                      "RSCCount", "Mol2D", "Mol3D", sep = ",")
      
    } else {
      
      fields <- fields
      
    }
  } else {
    
    fields <- paste(fields, collapse = ",")
    
  }

  header <- list("Content-Type" = "", "apikey" = apikey)

  base_url <- Sys.getenv("GET_RECORD_ID_URL", 
                         unset = "https://api.rsc.org/compounds/v1/records/")
  
  url <- paste0(base_url, record_id, "/details?fields=", fields)

  handle <- curl::new_handle()

  curl::handle_setopt(handle = handle, customrequest = "GET")

  curl::handle_setheaders(handle = handle, .list = header)

  result <- curl::curl_fetch_memory(url = url, handle = handle)

  .check_status_code(result$status_code)

  content <- rawToChar(result$content)
  
  content <- jsonlite::fromJSON(content)

  if (simplify_formula && "formula" %in% names(content)) {
    
    result$formula <- gsub(pattern = "[[:punct:]]", replacement = "", 
                           result$formula)
    
  }
  
  content
  
}
