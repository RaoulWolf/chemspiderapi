#' @title Post a batch of monoisotopic masses and their ranges to obtain a query ID
#' @description Functionality to post up to 20 monoisotopic masses and their range to obtain a \code{queryId} for use in \code{chemspiderapi::get_mass_batch_queryId_status()} and \code{chemspiderapi::get_mass_batch_queryId_results()}.
#' @details \emph{"If \code{dataSources} is not specified, all known sources are searched. This will take longer.\cr
#' \cr
#' Optionally, you can also submit \code{orderBy} and \code{orderDirection} to specify the sort order for the results. If you do not specify a value for \code{orderBy}, results are sorted by \code{recordId} by default.\cr
#' \cr
#' Valid values for \code{orderBy} are \code{recordId}, \code{massDefect}, \code{molecularWeight}, \code{referenceCount}, \code{dataSourceCount}, \code{pubMedCount}, \code{rscCount}.\cr
#' \cr
#' Valid values for \code{orderDirection} are \code{ascending}, \code{descending}."}
#' @param mass A vector of (double) numbers corresponding to the atomic (monoisotopic) masses you are inquiring. Has to be within the range of [1,11000].
#' @param range A vector of (double) numbers corresponding to the ranges for the above masses. Has to be within the range of [0.0001,0.001].
#' @param data_sources Optional: A character vector specifying which data source to use. Use \code{chemspiderapi::get_datasources()} for a complete list of data sources. If none are specified (the default), will search all data sources, which can take substantially longer time to complete.
#' @param order_by A character vector indicating by which parameter to order. Defaults to \code{recordId}; see Details for options.
#' @param order_direction A character vector indicating in which direction to order; either \code{ascending} (default) or \code{descending}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the queryId string as (named) character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/mass/batch}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## Post two atomic (monoisotopic) masses and their ranges
#' mass <- c(194, 303)
#' range <- c(0.0005, 0.001)
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_mass_batch(mass = mass, range = range, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_mass_batch <- function(mass, range, data_sources = NULL, 
                            order_by = "recordId", 
                            order_direction = "ascending", apikey = NULL) {
  
  .check_mass_and_range(mass, range)
  
  .check_data_sources(data_sources)
  
  .check_order(order_by, order_direction)
  
  .check_apikey(apikey)
  
  masses <- list()
  
  for (i in seq_along(mass)) {
    
    masses[[i]] <- list("mass" = mass[i], "range" = range[i])
    
  }
  
  if (!is.null(data_sources)) {
    
    if (length(data_sources) == 1) {
      
      data_sources <- I(data_sources)
      
    } 
    
    data <- list("masses" = masses, "dataSources" = data_sources, 
                 "orderBy" = order_by, "orderDirection" = order_direction)
    
  } else {
    
    data <- list("masses" = masses, "orderBy" = order_by, 
                 "orderDirection" = order_direction)
    
  }
  
  data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- Sys.getenv(
    "POST_MASS_BATCH_URL",
    unset = "https://api.rsc.org/compounds/v1/filter/mass/batch")
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle = handle, customrequest = "POST",
                      postfields = data)
  
  curl::handle_setheaders(handle = handle, .list = header)
  
  result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  .check_status_code(result$status_code)
  
  content <- rawToChar(result$content)
  
  content <- jsonlite::fromJSON(content)
  
  content
  
}
