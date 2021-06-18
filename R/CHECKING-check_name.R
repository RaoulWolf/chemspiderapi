.check_name <- function(
  name
  ) {
  
  if (
    is.null(
      x = name
      )
    ) {
    stop(
      "No \"name\" provided.", 
      call. = FALSE
      )
  }
  
  if (
    length(
      x = name
      ) > 1
    ) {
    stop(
      "This function can only handle a single \"name\" entry.\nFor functional programming, try using it in apply() or purrr::map().", 
      call. = FALSE
      )
  }
  
  if (
    !is.character(
      x = name
      )
    ) {
    stop(
      "The prodived \"name\" is not a character vector.", 
      call. = FALSE
      )
  }
  
}
