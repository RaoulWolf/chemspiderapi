.check_status_code <- function(status_code) {
  
  if (status_code != 200L) {
    
    if (status_code == 400L) {
      error_message <- paste(
        "ChemSpider Response:",
        "400: Bad Request. Check the request you sent and try again.\"",
        sep = "\n"
        )
    }
    
    if (status_code == 401L) {
      error_message <- paste(
        "ChemSpider Response:",
        "401: Unauthorized. Check you have supplied the correct API key and that you have sent it as an HTTP Header called 'apikey'.\"",
        sep = "\n"
        )
    }
    
    if (status_code == 404L) {
      error_message <- paste(
        "ChemSpider Response:",
        "404: Not Found. The requested endpoint URL is not recognized. Change your request and try again.\"",
        sep = "\n"
        )
    }
    
    if (status_code == 405L) {
      error_message <- paste(
        "ChemSpider Response:",
        "405: Method Not Allowed. The verb is incorrect for the endpoint. Change your request and try again.\"",
        sep = "\n"
        )
    }
    
    if (status_code == 413L) {
      error_message <- paste(
        "ChemSpider Response:",
        "413: Payload Too Large. The request you sent was too big to handle. Change your request and try again.\"",
        sep = "\n"
        )
    }
    
    if (status_code == 429L) {
      error_message <- paste(
        "ChemSpider Response:",
        "429: Too Many Requests. Send fewer requests, or use rate-limiting to slow them down, then try again.\"",
        sep = "\n"
        )
    }
    
    if (status_code == 500L) {
      error_message <- paste(
        "ChemSpider Response:",
        "500: Internal Server Error. Wait and try again.\"",
        sep = "\n"
        )
    }
    
    if (status_code == 503L) {
      error_message <- paste(
        "ChemSpider Response:",
        "503: Service Unavailable. Wait and try again.\"",
        sep = "\n"
        )
    }
    
    if (!any(status_code %in% 
             c(
               400L, 
               401L, 
               404L, 
               405L, 
               413L, 
               429L, 
               500L, 
               503L
               ))) {
      error_message <- "No ChemSpider Response was provided."
    }
    
    message <- paste(
      "No valid results were obtained.", 
      error_message
      )
    
    stop(
      message, 
      call. = FALSE
      )
  }

}
