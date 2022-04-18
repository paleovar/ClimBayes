#' Negative logarithm of posterior density (up to normalization constant)
#'
#' The posterior is proportional to likelihood * prior by Bayes theorem,
#' the normalization by the marginal is not included.
#'
#' @param X vector of parameter values.
#' @param params List of parameters;
#' relevant entries: those relevant for `log_likelihood`
#' and those for `prior`.
#' @param sol (optional) Vector of EBM solution for this value of X.
#' This can be used to speed up the code and avoid repeated calculations.
#' @return Value of the negative log of posterior density, up to normalization constant.
#' @export
neg_log_post <- function(X, params, sol = NULL) {
  prior_val = prior(X, params)
  if(prior_val == 0) {
    return(Inf)
  } else {
    return(- log_likelihood(X, params, sol = sol) - log(prior_val))
  }
}
