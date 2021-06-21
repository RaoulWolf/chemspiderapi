.check_limited_to_max_allowed <- function(result) {
  
  if ("limitedToMaxAllowed" %in% names(result) && result$limitedToMaxAllowed) {
    
    message <- paste(
      paste("The query has resulted in > 10'000 entries.",
            "Only the first 10'000 are returned."),
      "Consider splitting the request using \"start\" and \"count\".",
      sep = "\n")
    
    warning(message, call. = FALSE)
      
  }
  
}