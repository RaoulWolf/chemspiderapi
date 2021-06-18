.check_isotopic <- function(
  isotopic
  ) {
  
  if (
    length(
      x = isotopic
      ) != 1L
    ) {
    stop(
      "Only one \"isotopic\" is possible.", 
      call. = FALSE
      )
  }
  
  if (
    !is.character(
      x = isotopic
      )
    ) {
    stop(
      "The provided isotopic is not \"any\", \"labeled\", or \"unlabeled\".", 
      call. = FALSE
      )
  }
  
  if (
    !any(
      tolower(
        x = isotopic
        ) %in% 
      c(
        "any", 
        "labeled", 
        "unlabeled"
        )
      )
    ) {
    stop(
      "The provided isotopic is not \"any\", \"labeled\", or \"unlabeled\".", 
      call. = FALSE
      )
  }
}
