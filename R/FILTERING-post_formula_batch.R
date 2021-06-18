#' @title Post a batch of chemical formulas
#' @description Functionality to post a batch of up to 100 formulas to obtain a \code{queryId} for use in \code{chemspiderapi::get_formula_batch_queryId_status()} and \code{chemspiderapi::get_formula_batch_queryId_results()}.
#' @details Possible values for \code{orderBy} are: \code{"recordId"} (default), \code{"massDefect"}, \code{"molecularWeight"}, \code{"referenceCount"}, \code{"dataSourceCount"}, \code{"pubmedCount"}, and \code{"rscCount"}.\cr
#' \cr
#' Possible values for \code{orderDirection} are: \code{"ascending"} (default) and \code{"descending"}.\cr
#' \cr
#' \emph{"If dataSources is not specified, all known sources are searched. This will take longer."}
#' @param formulas A character vector of up to 100 chemical formulas.
#' @param data_sources Optional: Either a single character string or a vector of character string specifying the data sources. A list of possible data sources can be obtained from \code{chemspiderapi::get_datasources()}.
#' @param order_by A character string indicating by which parameter the results should be ordered; see Details.
#' @param order_direction A character string indicating in which direction the results should be ordered; see Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param coerce \code{logical}: should the list be coerced to a data.frame? Defaults to \code{FALSE}.
#' @return Returns the queryId string as (named) character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/formula/batch}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## Post the formulas of caffeine and cocaine to get a query ID
#' formulas <- c("C9H8O4", "C17H21NO4")
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_formula_batch(formulas = formulas, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_formula_batch <- function(
  formulas, 
  data_sources = NULL, 
  order_by = "recordId", 
  order_direction = "ascending", 
  apikey, 
  coerce = FALSE
  ) {
  
  .check_formulas(formulas)
  
  .check_data_sources(data_sources)
  
  .check_order(
    order_by, 
    order_direction
    )
  
  .check_apikey(apikey)
  
  .check_coerce(coerce)
  
  if (!is.null(data_sources)) {
    if (length(data_sources) == 1L) {
      data_sources <- I(data_sources)
    } 
    data <- list(
      "formulas" = formulas, 
      "dataSources" = data_sources, 
      "orderBy" = order_by, 
      "orderDirection" = order_direction
      )
  } else {
    data <- list(
      "formulas" = formulas, 
      "orderBy" = order_by, 
      "orderDirection" = order_direction
      )
  }
  
  data <- jsonlite::toJSON(
    data, 
    auto_unbox = TRUE
    )
  
  header <- list(
    "Content-Type" = "", 
    "apikey" = apikey
    )
  
  url <- Sys.getenv(
    "POST_FORMULA_BATCH_URL", 
    unset = "https://api.rsc.org/compounds/v1/filter/formula/batch"
    )
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(
    handle = handle, 
    customrequest = "POST", 
    postfields = data
    )
  
  curl::handle_setheaders(
    handle = handle, 
    .list = header
    )
  
  raw_result <- curl::curl_fetch_memory(
    url = url, 
    handle = handle
    )
  
  .check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  
  if (coerce) {
    result <- as.data.frame(
      result, 
      stringsAsFactors = FALSE
      )
  }
  
  result
}
