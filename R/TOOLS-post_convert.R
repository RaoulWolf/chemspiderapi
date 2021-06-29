#' @title Convert between chemical identifiers
#' @description Functionality to convert between different chemical identifier formats: InChI, InChIKey, Mol, and SMILES.
#' @details "Specify the input format as a string called 'input_format', and the output as a string called 'output_format'. Allowed conversions: from InChI to InChIKey, from InChI to Mol file, from InChI to SMILES, from InChIKey to InChI, from InChIKey to Mol file, from Mol file to InChI, from Mol file to InChIKey, from SMILES to InChI."\cr
#' \cr
#' The identifier names are NOT case sensitive!
#' @param input A character string in the specified \code{input_format}.
#' @param input_format A character string indicating which format the input has. Can be one of the following: \code{InChI}, \code{InChIKey}, \code{Mol}, or \code{SMILES}. See Details for possible conversions.
#' @param output_format A character string indicating which type of output is desired. Can be one of the following: \code{InChI}, \code{InChIKey}, \code{Mol}, or \code{SMILES}. See Details for possible conversions.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return A list with one character string containing the converted identifier
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/tools/convert}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## Convert the InChI string of caffeine to a SMILES formula
#' input <- "InChI=1S/C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3"
#' input_format <- "InChI"
#' output_format <- "SMILES"
#' apikey <- "a valid API key"
#' post_convert(input = input, input_format = input_format, 
#'              output_format = output_format, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export    
post_convert <- function(input, input_format, output_format, apikey = NULL) {
  
  .check_format(input, input_format, output_format)
  
  .check_apikey(apikey)
  
  data <- list("input" = input, "inputFormat" = input_format, 
               "outputFormat" = output_format)
  
  data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- Sys.getenv("POST_CONVERT_URL",
                    unset = "https://api.rsc.org/compounds/v1/tools/convert")
  
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
