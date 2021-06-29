.check_isotopic <- function(isotopic) {
  
  if (length(isotopic) != 1) {
    
    stop("Only one \"isotopic\" is possible.", call. = FALSE)
  }
  
  if (!is.character(isotopic)) {
    
    stop(paste("The provided isotopic is not \"any\",", 
               "\"labeled\", or \"unlabeled\"."), call. = FALSE)
    
  }
  
  if (!any(tolower(isotopic) %in% c("any", "labeled", "unlabeled"))) {
    
    stop(paste("The provided isotopic is not \"any\", \"labeled\",",
               "or \"unlabeled\"."), call. = FALSE)
    
  }
  
}
