.check_apikey <- function(apikey) {
  
  if (is.null(apikey)) {
    stop(
      "No ChemSpider \"apikey\" provided.", 
      call. = FALSE
      )
  }
  
  if (length(apikey) > 1) {
    stop(
      paste(
        "This function can only handle a single ChemSpider \"apikey\" entry.",
        "For functional programming, try using it in apply() or purrr::map().",
        sep = "\n"),
      call. = FALSE
      )
  }
  
  if (!is.character(apikey)) {
    stop(
      "The ChemSpider \"apikey\" should be a 32-character string.", 
      call. = FALSE
      )
  }
  
  if (nchar(apikey) != 32L) {
    stop(
      "Please use a valid 32-character ChemSpider \"apikey\".", 
      call. = FALSE
      )
  }
}
