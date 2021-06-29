.check_data_sources <- function(data_sources) {
  
  if (!is.null(data_sources) && length(data_sources) > 20) {
    
    stop("Only up to 20 different \"data_sources\" are allowed.", 
         call. = FALSE)
    
  }
  
}
