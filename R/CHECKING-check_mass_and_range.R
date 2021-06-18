.check_mass_and_range <- function(
  mass, 
  range
  ) {
  
  if (
    any(
      !is.numeric(
        x = mass
        )
      )
    ) {
    stop(
      "The provided \"mass\" is not a valid (double) number.", 
      call. = FALSE
      )
  }
  
  if (
    any(
      !is.numeric(
        x = range
        )
      )
    ) {
    stop(
      "The provided \"range\" is not a valid (double) number.", 
      call. = FALSE
      )
  }
  
  if (
    !is.null(
      x = mass
      ) && 
    any(
      mass < 1
      ) || 
    !is.null(
      x = mass
      ) && 
    any(
      mass > 11000
      )
    ) {
    stop(
      "The provided \"mass\" is outside ChemSpider's settings [1,11000].", 
      call. = FALSE
      )
  }
  
  if (
    !is.null(
      x = range
      ) && any(
        range < 0.0001
        ) || 
    !is.null(
      x = range
      ) && any(
        range > 100
        )
    ) {
    stop(
      "The provided \"range\" is outside ChemSpider's settings [0.0001,100].", 
      call. = FALSE
      )
  }
  
  if (
    length(
      x = mass
      ) != 
    length(
      x = range
      )
    ) {
    stop(
      "Every \"mass\" needs a \"range\", and vice verca. Please provide equal length vectors.", 
      call. = FALSE
      )
  }
  
}
