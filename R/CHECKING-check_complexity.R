.check_complexity <- function(complexity) {
  
  if (length(complexity) != 1) {
    
    stop("Only one \"complexity\" is possible.", call. = FALSE)
    
  }
  
  if (!any(tolower(complexity) %in% c("any", "single", "multiple"))) {
    
    stop(paste("The provided complexity is not",
               "\"any\", \"single\", or \"multiple\"."), call. = FALSE)
    
  }
  
}
