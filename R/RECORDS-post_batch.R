#' @title Post a batch of ChemSpider record IDs
#' @description Post a batch query of up to 100 ChemSpider record IDs.
#' @details "The available fields are: SMILES, Formula, InChI, InChIKey, StdInChI, StdInChIKey, AverageMass, MolecularWeight, MonoisotopicMass, NominalMass, CommonName, ReferenceCount, DataSourceCount, PubMedCount, RSCCount, Mol2D, Mol3D."\cr
#' \cr
#' If a \code{recordId} is not found, it is silently dropped.
#' @param record_ids A vector of integer ChemSpider IDs.
#' @param fields A character string indicating which fields to return; see Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param simplify_formula \code{logical}: Should formula strings be simplified? Defaults to \code{FALSE}.
#' @return A data frame (if multiple fields are returned), or a vector of adequate type if only one field is required.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/records/batch}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples
#' \dontrun{
#' ## Post a query for the first 50 ChemSpider entries.
#' record_ids <- 1:50
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_batch(
#'   record_ids = record_ids, 
#'   apikey = apikey
#'   )
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_batch <- function(record_ids, fields = "all", apikey = NULL, 
                       simplify_formula = FALSE) {
  
  .check_record_ids(record_ids)
  
  .check_fields(fields)
  
  .check_apikey(apikey)
  
  if (length(fields) == 1L) {
    
    if (fields == "all") {
      
      fields <- c("SMILES", "Formula", "InChI", "InChIKey", "StdInChI", 
                  "StdInChIKey", "AverageMass", "MolecularWeight", 
                  "MonoisotopicMass", "NominalMass", "CommonName", 
                  "ReferenceCount", "DataSourceCount", "PubMedCount", 
                  "RSCCount", "Mol2D", "Mol3D")
      
    } else {
      
      fields <- fields
      
    }
    
  } else {
    
    fields <- I(fields)
    
  }
  
  data <- list("recordIds" = record_ids, "fields" = fields)
  
  data <- jsonlite::toJSON(data)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- Sys.getenv("POST_BATCH_URL",
                    unset = "https://api.rsc.org/compounds/v1/records/batch")
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle = handle, customrequest = "POST", 
                      postfields = data)
  
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
