.check_inchikey <- function(
  inchikey
  ) {
  
  if (
    is.null(
      inchikey
      )
    ) {
    stop(
      "No \"inchikey\" provided.", 
      call. = FALSE
      )
  }
  
  if (
    length(
      inchikey
      ) > 1L
    ) {
    stop(
      "This function can only handle a single \"inchikey\" entry.\nFor functional programming, try using it in apply() or purrr::map().", 
      call. = FALSE
      )
  }
  
  if (
    typeof(
      inchikey
      ) != "character"
    ) {
    stop(
      "The \"inchikey\" should be a 25- or 27-character string.", 
      call. = FALSE
      )
  }
  
  if (
    length(
      x = unlist(
        x = strsplit(
          x = inchikey, 
          split = "-"
          )
        )
      ) != 
    3L && 
    length(
      x = unlist(
        x = strsplit(
          x = inchikey, 
          split = "-"
          )
        )
      ) != 
    2L) {
    stop(
      "The provided \"inchikey\" must be hyphen-divided into two (non-standard) or three (standard) parts.", 
      call. = FALSE
      )
  }
  
  if (
    length(
      x = unlist(
        x = strsplit(
          x = inchikey, 
          split = "-"
          )
        )
      ) == 
    3L && 
    nchar(
      x = inchikey
      ) != 27L
    ) {
    stop(
      "The provided (standard) \"inchikey\" is not a 27-character string.", 
      call. = FALSE
      )
  }
  
  if (
    length(
      x = unlist(
        x = strsplit(
          x = inchikey, 
          split = "-"
          )
        )
      ) == 
    2L && 
    nchar(
      x = inchikey
      ) != 25L
    ) {
    stop(
      "The provided (non-standard) \"inchikey\" is not a 25-character string.", 
      call. = FALSE
      )
  }
  
  if (
    nchar(
      x = unlist(
        x = strsplit(
          x = inchikey, 
          split = "-"
          )
        )[1]
      ) != 14L
    ) {
    stop(
      "The first part of the \"inchikey\" should be 14 characters long.", 
      call. = FALSE
      )
  }

  if (
    substr(
      x = unlist(
        x = strsplit(
          x = inchikey, 
          split = "-"
          )
        )[2], 
      start = 9L, 
      stop = 9L
      ) != "S"
    ) {
    warning(
      "This is not a standard \"inchikey\"; performing API query regardless.", 
      call. = FALSE
      )
  }

}
