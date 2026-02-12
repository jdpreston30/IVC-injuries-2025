#' Compute Fisher's Method P-Value for ternG Output
#'
#' Manually recalculates p-values for 2x2 categorical comparisons using Fisher's preferred method.
#' Use this to correct p-values in ternG output after generation.
#'
#' @param data The same data frame passed to ternG
#' @param group_var The grouping variable name
#' @param categorical_vars Vector of categorical variable names to recalculate
#' @return Named vector of corrected p-values
#' @examples
#' corrected_pvals <- compute_fisher_pvalues(mydata, "treatment", c("sex", "readmission"))
compute_fisher_pvalues <- function(data, group_var, categorical_vars) {
  
  pvals <- setNames(numeric(length(categorical_vars)), categorical_vars)
  
  for (var in categorical_vars) {
    # Filter out missing values
    df_clean <- data %>%
      dplyr::filter(!is.na(.data[[var]]), !is.na(.data[[group_var]]))
    
    if (nrow(df_clean) == 0) {
      pvals[var] <- NA
      next
    }
    
    # Create contingency table
    tab <- table(df_clean[[group_var]], df_clean[[var]])
    
    # Only compute for 2x2 tables
    if (all(dim(tab) == c(2, 2))) {
      pvals[var] <- fisher_method(tab)$p.value
    } else {
      # For non-2x2, use standard Fisher-Freeman-Halton
      pvals[var] <- fisher.test(tab)$p.value
    }
  }
  
  return(pvals)
}


#' Print Corrected P-Values for Manual Entry
#'
#' Helper function to display corrected p-values in a format ready for manuscript.
#' 
#' @param pvals Named vector from compute_fisher_pvalues()
#' @param digits Number of decimal places (default 2)
#' @return Invisible NULL (prints to console)
print_corrected_pvalues <- function(pvals, digits = 2) {
  cat("\n=== Corrected P-Values (Fisher's Method) ===\n\n")
  
  for (var in names(pvals)) {
    p <- pvals[var]
    if (is.na(p)) {
      cat(sprintf("%-20s: NA\n", var))
    } else if (p == 1.0) {
      cat(sprintf("%-20s: p = 1.00\n", var))
    } else if (p >= 0.99) {
      cat(sprintf("%-20s: p = %.2f\n", var, p))
    } else if (p < 0.001) {
      cat(sprintf("%-20s: p < 0.001\n", var))
    } else {
      cat(sprintf("%-20s: p = %.2f\n", var, p))
    }
  }
  
  cat("\n")
  invisible(NULL)
}
