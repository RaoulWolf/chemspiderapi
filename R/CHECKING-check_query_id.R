.check_query_id <- function(query_id) {
  
  if (length(query_id) > 1L) {
    stop(
      paste(
        "This function can only handle one \"query_id\" entry.",
        "For functional programming, try using *apply() or purrr::map().",
        sep = "\n"),
      call. = FALSE
      )
  }
  
  if (!is.character(query_id)) {
    stop(
      "The \"query_id\" should be a character string.", 
      call. = FALSE
      )
  }
  
  if (nchar(query_id) != 36L) {
    stop(
      "Please use a valid 36-character \"query_id\".", 
      call. = FALSE
      )
  }
  
  if (length(unlist(strsplit(
    query_id, 
    split = "-"
    ))) != 5L) {
    stop(
      "The \"query_id\" should be hyphen-divided into five parts.", 
      call. = FALSE
      )
  }
  
  if (nchar(unlist(strsplit(
    query_id, 
    split = "-"
    ))[1]) != 8L) {
    stop(
      "The first part of the \"query_id\" should be 8 characters long.", 
      call. = FALSE
      )
  }
  
  if (nchar(unlist(strsplit(
    query_id, 
    split = "-"
    ))[2]) != 4L) {
    stop(
      "The second part of the \"query_id\" should be 4 characters long.", 
      call. = FALSE
      )
  }
  
  if (nchar(unlist(strsplit(
    query_id,
    split = "-"
    ))[3]) != 4L) {
    stop(
      "The third part of the \"query_id\" should be 4 characters long.", 
      call. = FALSE
      )
  }
  
  if (nchar(unlist(strsplit(
    query_id,
    split = "-"
    ))[4]) != 4L) {
    stop(
      "The fourth part of the \"query_id\" should be 4 characters long.", 
      call. = FALSE
      )
  }

}
