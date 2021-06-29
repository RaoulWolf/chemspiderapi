.check_status <- function(status) {
  
  if (is.null(status)) {
    
    stop("No ChemSpider query \"status\" provided.", call. = FALSE)
    
  }
  
  if (length(status) > 1) {
    
    stop("This function can only handle a single status entry.", call. = FALSE)
    
  }
  
  if (!is.character(status)) {
    
    stop("The provided ChemSpider query status needs to be a character vector.", 
         call. = FALSE)
    
  }
  
  if (tolower(status) != "complete") {
    
    stop("Query not yet complete. Try again later.", call. = FALSE)
    
  }

}
