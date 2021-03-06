.check_elements <- function(include_elements, exclude_elements) {
  
  if (is.null(include_elements)) {
    
    stop("No \"include_elements\" provided.", call. = FALSE)
    
  }
  
  if (is.null(exclude_elements)) {
    
    stop("No \"exclude_elements\" provided.", call. = FALSE)
  }
  
  if (length(include_elements) != sum(tolower(include_elements) %in% 
          tolower(c("H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na",
                    "Mg", "Al", "Si", "P", "S", "Cl", "Ar", "K", "Ca", "Sc",
                    "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga",
                    "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y", "Zr", "Nb",
                    "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb",
                    "Te", "I", "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd", "Pm", 
                    "Sm","Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu", 
                    "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", 
                    "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th", "Pa", 
                    "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm", "Md", 
                    "No", "Lr", "Rf", "Db", "Sg", "Bh", "Hs", "Mt", "Ds", "Rg", 
                    "Cn", "Nh", "Fl", "Mc", "Lv", "Ts", "Og")))) {
    
    stop(paste("One or more provided inputs to \"include_elements\" are not", 
               "part of the periodic table"), call. = FALSE)
    
  }
  
  if (length(exclude_elements) != sum(tolower(exclude_elements) %in%
          tolower(c("H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na",
                    "Mg", "Al", "Si", "P", "S", "Cl", "Ar", "K", "Ca", "Sc",
                    "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga",
                    "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y", "Zr", "Nb",
                    "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb",
                    "Te", "I", "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd", "Pm", 
                    "Sm","Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu", 
                    "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", 
                    "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th", "Pa", 
                    "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm", "Md", 
                    "No", "Lr", "Rf", "Db", "Sg", "Bh", "Hs", "Mt", "Ds", "Rg", 
                    "Cn", "Nh", "Fl", "Mc", "Lv", "Ts", "Og")))) {
    
    stop(paste("One or more provided inputs to \"exclude_elements\" are not", 
               "part of the periodic table"), call. = FALSE)
    
  }
  
  if (length(include_elements) > 15) {
    
    stop("ChemSpider only supports up to 15 entries in \"include_elements\".",
         call. = FALSE)
    
  }
  
  if (length(exclude_elements) > 100) {
    stop("ChemSpider only supports up to 100 entries in \"exclude_elements\".",
         call. = FALSE)
    
  }
  
}
