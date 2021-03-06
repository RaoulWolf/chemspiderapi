#' @title Post a monoisotopic mass and its range to obtain a query ID
#' @description Functionality to POST an atomic mass and its range to obtain a \code{queryId} for use in \code{chemspiderapi::get_queryId_status()} and \code{chemspiderapi::get_queryId_results()}.
#' @details \emph{"Submit mass as a double between 1 and 11000 Atomic Mass Units, and a range between 0.0001 and 100 via POST. For example, if you specify a mass of 40 and a range of 5, all monoisotopic masses between 35 and 45 are searched.\cr
#' \cr
#' [...] If \code{dataSources} is not specified, all known sources are searched. This will take longer.\cr
#' \cr
#' Optionally, you can also submit \code{orderBy} and \code{orderDirection} to specify the sort order for the results. If you do not specify a value for \code{orderBy}, results are sorted by [\code{recordId}] by default.\cr
#' \cr
#' Valid values for \code{orderBy} are \code{recordId}, \code{massDefect}, \code{molecularWeight}, \code{referenceCount}, \code{dataSourceCount}, \code{pubMedCount}, \code{rscCount}.\cr
#' \cr
#' Valid values for \code{orderDirection} are \code{ascending}, \code{descending}."}
#' @param mass A (double) number corresponding to the atomic mass (Da or g/mol) you are inquiring. Has to be within the range of [1,11000].
#' @param range The range for the above mass, also as (double) number. Has to be within the range of [0.0001,100].
#' @param data_sources Optional: A character vector specifying which data source to use. Use \code{chemspiderapi::get_datasources()} for a complete list of data sources. If none are specified (the default), will search all data sources, which can take substantially longer time to complete.
#' @param order_by A character vector indicating by which parameter to order. Defaults to \code{recordId}; see Details for options.
#' @param order_direction A character vector indicating in which direction to order; either \code{ascending} (default) or \code{descending}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the queryId string as (named) character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/mass}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## Post the approximate atomic mass of caffeine and a sensible range
#' mass <- 194
#' range <- 0.002
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_mass(mass = mass, range = range, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_mass <- function(mass, range, data_sources = NULL, order_by = "recordId",
                      order_direction = "ascending", apikey = NULL) {
  
  .check_mass_and_range(mass, range)
  
  .check_data_sources(data_sources)
  
  .check_order(order_by, order_direction)
  
  .check_apikey(apikey)
  
  if (!is.null(data_sources)) {
    
    if (length(data_sources) == 1) {
      
      data_sources <- I(data_sources)
      
    } 
    
    data <- list("mass" = mass, "range" = range, "dataSources" = data_sources,
                 "orderBy" = order_by, "orderDirection" = order_direction)
    
  } else {
    
    data <- list("mass" = mass, "range" = range, "orderBy" = order_by, 
                 "orderDirection" = order_direction)
    
  }
  
  data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- Sys.getenv("POST_MASS_URL",
                    unset = "https://api.rsc.org/compounds/v1/filter/mass")
  
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
