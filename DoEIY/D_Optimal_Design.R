D_Optimal_Designs <- function(levels, components, factor_types, nruns) {

  # Build model formula from supplied terms (components)
  formula <- as.formula(paste("~", paste(components, collapse = " + ")))
  
  # Extract factor names robustly from the formula
  var_names <- unique(all.vars(formula))
  
  num_factors <- length(levels)
  
  # Build candidate set
  candidate_list <- lapply(seq_len(num_factors), function(i) {
    ft <- factor_types[i]
    k  <- levels[i]
    
    if (ft == "Continuous") {
      # Use coded range [-1, 1] for better stability when using optFederov
      seq(-1, 1, length.out = k) 
    } else if (ft == "Discrete") {
      # Treat as numeric, equally spaced in [-1, 1]
      if (k == 2) {c(-1, 1)} else {seq(-1, 1, length.out = k)}
    } else if (ft == "Categorical") {
      # Nominal factor with k levels (labeled 1..k)
      factor(seq_len(k))
    } else {
      stop("Unknown factor type '", ft,
           "'. Use 'Continuous', 'Discrete', or 'Categorical'.")     
    }
  })
      
  candidate_set <- do.call(expand.grid, candidate_list)
  
  # Rename columns to match variables referenced in the formula
  colnames(candidate_set) <- var_names
  
  # Ensure the number of rows in the candidate set exceedes the number of runs selected by the user
  if (nruns > nrow(candidate_set)) {                                
    stop("nruns (", nruns, ") exceeds size of candidate set (", nrow(candidate_set), "). Increase levels or reduce nruns.") 
  }
      
  # Generate the D-optimal design
  Design <- optFederov(formula, data = candidate_set, nTrials = nruns)$design
  rownames(Design) <- NULL
  
  return(Design)
}
