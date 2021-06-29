.check_record_ids <- function(record_ids) {
  
  if (is.null(record_ids)) {
    
    stop("No \"record_ids\" provided.", call. = FALSE)
    
  }
  
  if (length(record_ids) < 2) {
    
    stop("This function is meant for multiple \"record_ids\" entries.",
         call. = FALSE)
    
  }
  
  if (length(record_ids) > 100) {
    
    stop("This function can only handle up to 100 \"record_ids\".",
         call. = FALSE)
    
  }
  
  if (!is.numeric(record_ids)) {
    
    stop("Please provide a valid (integer) \"record_ids\".", call. = FALSE)
    
  }
  
  if (is.double(record_ids)) {
    
    record_ids <- as.integer(record_ids)
    
    warning("The \"record_ids\" were transformed from double to integer.", 
            call. = FALSE)
    
  }
  
}
