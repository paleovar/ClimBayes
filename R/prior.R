#' Prior density
#'
#' Computes the prior density for the parameter values in `X`,
#' assuming priors for individual parameters are independent of each other.
#'
#' @param X Vector of parameter values.
#' @param params List of parameters.
#' Relevant parameters:
#' * `prior_type`
#' * lower and upper bounds for prior intervals of parameters
#' (e.g. `lambda1_lb` and `lambda1_ub` define bounds for parameter `lambda1`)
#' * if `type == "beta"`:  `prior_beta_shape1` and `prior_beta_shape2`
#' are the shape parameters for the beta distribution (either of length 1 to specify
#' one shape for all parameters or of length equal to `length(X)`,
#' such that each parameter has a differently shaped prior)
#' @return Value of the prior density.
#' @export
prior <- function(X, params) {

  X_names = params$X_names

  prior = 1
  type = params$prior_type

  for (i in 1:length(X)) {
    value = X[i]
    var_name = X_names[i]
    if (type == "unif") {
      lb = params[[paste0(var_name, "_lb")]]
      ub = params[[paste0(var_name, "_ub")]]
      prior = prior * (lb <= value) * (ub >= value) / (ub - lb)
    } else if (type == "beta") {
      lb = params[[paste0(var_name, "_lb")]]
      ub = params[[paste0(var_name, "_ub")]]
      b1 = params$prior_beta_shape1
      b2 = params$prior_beta_shape2
      if(length(b1) > 1) {
        b1 = b1[i]
      }
      if(length(b2) > 1) {
        b2 = b2[i]
      }
      prior = prior * (dbeta((value - lb)  / (ub - lb) , b1, b2)
                       / (ub-lb) * (lb <= value) * (ub >= value))
    }
  }
  return(prior)
}
