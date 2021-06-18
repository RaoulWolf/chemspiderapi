.check_record_id <- function(record_id) {
  
  if (is.null(record_id)) {
    stop(
      "No \"record_id\" provided.", 
      call. = FALSE
      )
  }
  
  if (length(record_id) > 1L) {
    stop(
      paste("This function can only handle a single \"record_id\" entry.",
            "For functional programming, try using it in *apply() or purrr::map().",
            sep = "\n"
      ),
      call. = FALSE)
  }
  
  if (!is.numeric(record_id)) {
    stop(
      "Please provide a valid (integer) \"record_id\".",
      call. = FALSE)
  }
  
  if (is.double(record_id)) {
    record_id <- as.integer(record_id)
    warning(
      "The \"record_id\" was transformed from double to integer.", 
      call. = FALSE)
  }
  
}
