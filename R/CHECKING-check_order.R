.check_order <- function(order_by, order_direction) {
  
  if (!is.null(order_by) && length(order_by) > 1) {
    
    stop("Only a single \"order_by\" entry is supported.", call. = FALSE)
    
  }
  
  if (!is.null(order_direction) && length(order_direction) > 1) {
    
    stop("Only a single \"order_direction\" entry is supported.", 
         call. = FALSE)
    
  }
  
  if (!is.null(order_by) && !is.character(order_by)) {
    
    stop("Please provide a valid input for \"order_by\".", call. = FALSE)
    
  }

  if (!is.null(order_direction) && !is.character(order_direction)) {
    
    stop("Please provide a valid input for \"order_direction\".", 
         call. = FALSE)
    
  }
  
  if (!is.null(order_by) && !any(tolower(order_by) %in% 
                                 c("recordid", "massdefect", "molecularweight",
                                   "referencecount", "datasourcecount", 
                                   "pubmedcount", "rsccount"))) {
    
    stop("Please provide a valid input for \"order_by\".", call. = FALSE)
    
  }
  
  if (!is.null(order_direction) && !any(tolower(order_direction) %in%
                                        c("ascending", "descending"))) {
    
    stop(paste("Please use either \"ascending\" or \"descending\" as",
               "input for \"order_direction\"."), call. = FALSE)
    
  }
  
}
