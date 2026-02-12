#' Fisher's Preferred Method for 2-Sided Exact Test
#'
#' Implements R.A. Fisher's preferred approach for two-tailed exact tests
#' by doubling the smaller one-tailed p-value (max 1.0).
#' 
#' This differs from the Fisher-Irwin test (R's default fisher.test) which
#' sums probabilities of tables as or more extreme in both tails.
#' 
#' For regulatory and medical applications, Fisher's method aligns with
#' demonstrating superiority at the one-tailed 0.025 level, consistent
#' with chi-square test with Yates' continuity correction for larger samples.
#'
#' @param x A 2x2 contingency table (matrix or table object), or a factor for first variable
#' @param y Optional factor for second variable (if x is a factor)
#' @param alternative Character string specifying alternative hypothesis.
#'   Must be "two.sided" (default), "greater", or "less"
#' @param conf.level Confidence level for odds ratio confidence interval
#' @return A list with class "htest" containing:
#'   \item{p.value}{Two-sided p-value using Fisher's method}
#'   \item{estimate}{Odds ratio estimate}
#'   \item{conf.int}{Confidence interval for odds ratio}
#'   \item{method}{Description of test method}
#'   \item{data.name}{Name of the data}
#'   \item{alternative}{Alternative hypothesis}
#' @references 
#' Fisher, R.A. (1935). The Design of Experiments.
#' Yates, F. (1984). Tests of significance for 2×2 contingency tables. 
#'   Journal of the Royal Statistical Society, Series A.
#' @examples
#' # 2x2 table
#' tab <- matrix(c(10, 5, 3, 12), nrow = 2)
#' fisher_method(tab)
#' 
#' # Compare with R's default
#' fisher.test(tab)  # Fisher-Irwin method
#' fisher_method(tab)  # Fisher's preferred method
fisher_method <- function(x, y = NULL, alternative = "two.sided", conf.level = 0.95) {
  
  # Handle input: convert to contingency table if factors provided
  if (!is.null(y)) {
    if (!is.factor(x)) x <- factor(x)
    if (!is.factor(y)) y <- factor(y)
    data.name <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    x <- table(x, y)
  } else {
    data.name <- deparse(substitute(x))
    if (!is.matrix(x) && !is.table(x)) {
      stop("x must be a matrix, table, or a factor (with y also a factor)")
    }
  }
  
  # Validate alternative
  alternative <- match.arg(alternative, c("two.sided", "greater", "less"))
  
  # For non-2x2 tables, use Fisher-Freeman-Halton (standard fisher.test)
  if (!all(dim(x) == c(2, 2))) {
    warning("Table is not 2x2. Using Fisher-Freeman-Halton exact test (standard R implementation).")
    result <- stats::fisher.test(x, alternative = alternative, conf.level = conf.level)
    result$method <- "Fisher-Freeman-Halton exact test"
    return(result)
  }
  
  # For 2x2 tables: compute one-tailed p-values using hypergeometric distribution
  # Get table margins
  m1 <- sum(x[1, ])  # row 1 total
  m2 <- sum(x[2, ])  # row 2 total
  n1 <- sum(x[, 1])  # col 1 total
  n2 <- sum(x[, 2])  # col 2 total
  k <- x[1, 1]       # observed count in cell [1,1]
  N <- sum(x)        # total
  
  # Compute p-value for observed table
  p_obs <- dhyper(k, m1, m2, n1)
  
  # Range of possible values for cell [1,1] given margins
  k_min <- max(0, n1 - m2)
  k_max <- min(m1, n1)
  k_all <- k_min:k_max
  
  # Compute probabilities for all possible tables
  p_all <- dhyper(k_all, m1, m2, n1)
  
  # One-tailed p-values
  # "greater": P(X >= observed)
  p_greater <- sum(p_all[k_all >= k])
  # "less": P(X <= observed)  
  p_less <- sum(p_all[k_all <= k])
  
  # Fisher's method for two-sided: double the smaller one-tailed p-value, max 1.0
  if (alternative == "two.sided") {
    p_value <- min(2 * min(p_greater, p_less), 1.0)
  } else if (alternative == "greater") {
    p_value <- p_greater
  } else {
    p_value <- p_less
  }
  
  # Get odds ratio and confidence interval from standard fisher.test
  # (These are computed consistently regardless of p-value method)
  std_result <- stats::fisher.test(x, alternative = alternative, conf.level = conf.level)
  
  # Construct result object
  result <- list(
    p.value = p_value,
    estimate = std_result$estimate,
    conf.int = std_result$conf.int,
    null.value = std_result$null.value,
    alternative = alternative,
    method = if (alternative == "two.sided") {
      "Fisher's exact test (two-sided via doubling minimum one-tailed p-value)"
    } else {
      paste("Fisher's exact test (", alternative, ")", sep = "")
    },
    data.name = data.name
  )
  
  class(result) <- "htest"
  return(result)
}
