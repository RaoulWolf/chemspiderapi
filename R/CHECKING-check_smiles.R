.check_smiles <- function(smiles) {
  
  if (is.null(smiles)) {
    
    stop("No \"smiles\" provided.", call. = FALSE)
    
  }
  
  if (length(smiles) > 1) {
    
    stop(paste("This function can only handle a single \"smiles\" entry.",
               "\nFor functional programming, try using it in apply() or",
               "purrr::map()."), call. = FALSE)
    
  }
  
  if (!is.character(smiles)) {
    
    stop("The provided \"smiles\" is not a character vector.", call. = FALSE)
    
  }
  
  if (tolower(substr(x = smiles, start = 1, stop = 6)) == tolower("InChI=")) {
    
    stop("The provided \"smiles\" seems to be an InChI string.", call. = FALSE)
    
  }

}
