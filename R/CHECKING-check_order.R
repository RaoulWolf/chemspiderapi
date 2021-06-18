.check_order <- function(
  orderBy, 
  orderDirection
  ) {
  
  if (
    !is.null(
      x = orderBy
      ) && 
    length(
      x = orderBy
      ) > 1L
    ) {
    stop(
      "Only a single \"orderBy\" entry is supported.", 
      call. = FALSE
      )
  }
  
  if (
    !is.null(
      x = orderDirection
      ) && 
    length(
      x = orderDirection
      ) > 1L
    ) {
    stop(
      "Only a single \"orderDirection\" entry is supported.", 
      call. = FALSE
      )
  }
  
  if (
    !is.null(
      x = orderBy
      ) && 
    !is.character(
      x = orderBy
      )
    ) {
    stop(
      "Please provide a valid input for \"orderBy\".", 
      call. = FALSE
      )
  }

  if (
    !is.null(
      x = orderDirection
      ) && 
    !is.character(
      x = orderDirection
      )
    ) {
    stop(
      "Please provide a valid input for \"orderDirection\".", 
      call. = FALSE
      )
  }
  
  if (
    !is.null(
      x = orderBy
      ) && 
    !any(
      tolower(
        x = orderBy
        ) %in%
      c(
        "recordid", 
        "massdefect", 
        "molecularweight", 
        "referencecount", 
        "datasourcecount", 
        "pubmedcount", 
        "rsccount"
        )
      )
    ) {
    stop(
      "Please provide a valid input for \"orderBy\".", 
      call. = FALSE
      )
  }
  
  if (
    !is.null(
      x = orderDirection
      ) && 
    !any(
      tolower(
        x = orderDirection
        ) %in%
      c(
        "ascending", 
        "descending"
        )
      )
    ) {
    stop(
      "Please use either \"ascending\" or \"descending\" as input for \"orderDirection\".", 
      call. = FALSE
      )
  }
  
}
