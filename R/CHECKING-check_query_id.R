.check_query_id <- function(query_id) {
  
  if (length(query_id) > 1) {
    
    stop(paste("This function can only handle one \"query_id\" entry.",
               "\nFor functional programming, try using *apply() or", 
               "purrr::map()."), call. = FALSE)
    
  }
  
  if (!is.character(query_id)) {
    
    stop("The \"query_id\" should be a character string.", call. = FALSE)
    
  }
  
  if (nchar(query_id) != 36) {
    
    stop("Please use a valid 36-character \"query_id\".", call. = FALSE)
    
  }
  
  if (length(unlist(strsplit(query_id, split = "-"))) != 5) {
    
    stop("The \"query_id\" should be hyphen-divided into five parts.",
         call. = FALSE)
    
  }
  
  if (nchar(unlist(strsplit(query_id, split = "-"))[1]) != 8) {
    
    stop("The first part of the \"query_id\" should be 8 characters long.",
         call. = FALSE)
    
  }
  
  if (nchar(unlist(strsplit(query_id, split = "-"))[2]) != 4) {
    
    stop("The second part of the \"query_id\" should be 4 characters long.",
         call. = FALSE)
    
  }
  
  if (nchar(unlist(strsplit(query_id, split = "-"))[3]) != 4) {
    
    stop("The third part of the \"query_id\" should be 4 characters long.",
         call. = FALSE)
    
  }
  
  if (nchar(unlist(strsplit(query_id, split = "-"))[4]) != 4) {
    
    stop("The fourth part of the \"query_id\" should be 4 characters long.",
         call. = FALSE)
  }

}
