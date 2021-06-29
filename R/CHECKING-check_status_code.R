.check_status_code <- function(status_code) {
  
  if (status_code != 200) {
    
    if (status_code == 400) {
      
      error_message <- paste("ChemSpider response:\n",
                             "400: Bad Request.",
                             "Check the request you sent and try again.\"")
      
    }
    
    if (status_code == 401) {
      
      error_message <- paste("ChemSpider response:\n",
                             "401: Unauthorized.",
                             "Check you have supplied the correct API key and",
                             "that you have sent it as an HTTP Header called",
                             "'apikey'.\"")
      
    }
    
    if (status_code == 404) {
      
      error_message <- paste("ChemSpider response:\n",
                             "404: Not Found.",
                             "The requested endpoint URL is not recognized.",
                             "Change your request and try again.\"")
      
    }
    
    if (status_code == 405) {
      
      error_message <- paste("ChemSpider response:\n",
                             "405: Method Not Allowed.",
                             "The verb is incorrect for the endpoint.",
                             "Change your request and try again.\"")
      
    }
    
    if (status_code == 413) {
      
      error_message <- paste("ChemSpider response:\n",
                             "413: Payload Too Large.",
                             "The request you sent was too big to handle.",
                             "Change your request and try again.\"")
      
    }
    
    if (status_code == 429) {
      
      error_message <- paste("ChemSpider response:\n",
                             "429: Too Many Requests.",
                             "Send fewer requests, or use rate-limiting to",
                             "slow them down, then try again.\"")
      
    }
    
    if (status_code == 500) {
      
      error_message <- paste("ChemSpider response:\n",
                             "500: Internal Server Error.", 
                             "Wait and try again.\"")
    }
    
    if (status_code == 503) {
      
      error_message <- paste("ChemSpider response:\n",
                             "503: Service Unavailable.",
                             "Wait and try again.\"")
      
    }
    
    if (!any(status_code %in% c(400, 401, 404, 405, 413, 429, 500, 503))) {
      
      error_message <- "No ChemSpider response was provided."
      
    }
    
    message <- paste("No valid results were obtained.", error_message)
    
    stop(message, call. = FALSE)
  }

}
