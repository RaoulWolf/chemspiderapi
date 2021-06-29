.check_inchikey <- function(inchikey) {
  
  if (is.null(inchikey)) {
    
    stop("No \"inchikey\" provided.", call. = FALSE)
    
  }
  
  if (length(inchikey) > 1) {
    
    stop(paste("This function can only handle a single \"inchikey\" entry.",
               "\nFor functional programming, try using it in apply() or", 
               "purrr::map()."), call. = FALSE)
    
  }
  
  if (typeof(inchikey) != "character") {
    
    stop("The \"inchikey\" should be a 25- or 27-character string.", 
         call. = FALSE)
    
  }
  
  if (length(unlist(strsplit(inchikey, split = "-"))) != 3 &&
      length(unlist(strsplit(inchikey, split = "-"))) != 2) {
    
    stop(paste("The provided \"inchikey\" must be hyphen-divided into two",
               "(non-standard) or three (standard) parts."), call. = FALSE)
         
  }
  
  if (length(unlist(strsplit(inchikey, split = "-"))) == 3 &&
      nchar(inchikey) != 27) {
    
    stop("The provided (standard) \"inchikey\" is not a 27-character string.",
         call. = FALSE)
    
  }
  
  if (length(unlist(strsplit(inchikey, split = "-"))) == 2 &&
      nchar(inchikey) != 25) {
    
    stop(paste("The provided (non-standard) \"inchikey\" is not a",
               "25-character string."), call. = FALSE)
    
  }
  
  if (nchar(unlist(strsplit(inchikey, split = "-"))[1]) != 14) {
    
    stop("The first part of the \"inchikey\" should be 14 characters long.",
         call. = FALSE)
    
  }

  if (substr(unlist(strsplit(inchikey, split = "-"))[2], 
             start = 9, stop = 9) != "S") {
    
    warning(paste("This is not a standard \"inchikey\";", 
                  "performing API query regardless."), call. = FALSE)
    
  }

}
