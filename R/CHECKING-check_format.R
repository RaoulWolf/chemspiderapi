.check_format <- function(input, input_format, output_format) {
  
  if (is.null(input)) {
    
    stop("Please provide an \"input\".", call. = FALSE)
    
  }
  
  if (length(input) > 1) {
    
    stop(paste("Please provide a single \"input\" string.",
               paste("For functional programming,", 
                     "try post_convert() in *apply() or purrr::map*()."),
               sep = "\n"), call. = FALSE)
    
  }
  
  if (!is.character(input)) {
    
    stop("The provided \"input\" is not a character vector.", call. = FALSE)
    
  }
  
  if (is.null(input_format)) {
    
    stop("Please provide an \"input_format\".", call. = FALSE)
  }
  
  if (is.null(output_format)) {
    
    stop("Please provide an \"output_format\".", call. = FALSE)
    
  }  
  
  if (!any(tolower(input_format) %in% c("inchi", "inchikey", "smiles", 
                                        "mol"))) {
    
    stop(paste("Please provide a valid \"input_format\".", 
               "See Documentation for details."), call. = FALSE)
    
  }
  
  if (!any(tolower(output_format) %in% c("inchi", "inchikey", "smiles", 
                                         "mol"))) {
    
    stop(paste("Please provide a valid \"output_format\".", 
               "See Documentation for details."), call. = FALSE)
    
  }
  
  if (tolower(input_format) == "inchi") {
    
    .check_inchi(input)
    
  }

  if (tolower(input_format) == "inchikey") {
    
    .check_inchikey(input)
    
  }

  if (tolower(input_format) == "smiles") {
    
    .check_smiles(input)
    
  }
  
  if (tolower(input_format) == "inchi" && !any(tolower(output_format) %in% 
                                               c("inchikey", "mol", 
                                                 "smiles"))) {
    
    stop("This conversion is not supported. See Documentation for details.", 
         .call = FALSE)
    
  }

  if (tolower(input_format) == "inchikey" && !any(tolower(output_format) %in% 
                                                  c("inchi", "mol"))) {
    
    stop("This conversion is not supported. See Documentation for details.", 
         .call = FALSE)
    
  }

  if (tolower(input_format) == "mol" && !any(tolower(output_format) %in% 
                                             c("inchi", "inchikey"))) {
    
    stop("This conversion is not supported. See Documentation for details.",
         .call = FALSE)
    
  }

  if (tolower(input_format) == "smiles" && tolower(output_format) != "inchi") {
    
    stop("This conversion is not supported. See Documentation for details.",
         .call = FALSE)
    
  }
  
}
