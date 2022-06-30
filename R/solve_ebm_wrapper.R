#' Wrapper for `solve_ebm`
#'
#' General EBM solver which extracts correct arguments for `solve_ebm`
#' and calls it.
#' Usually the wrapper is not called by the user,
#' but instead used in other MCMC functions.
#'
#' @param X vector of parameter values.
#' @param params List of parameters.
#' Relevant parameters:
#' * `X_names`: parameter names corresponding to the values in `X`
#' (e.g. the default for a one-box model will be `X_names = c("lambda1", "T0", "F0")`,
#' and `X` is of length three)
#' default values of parameters which are not estimated (e.g.
#' `Cap_default`),
#' * `forc_vals` (vector of forcing values)
#' @return Vector of EBM solution.
#' @export
solve_ebm_wrapper <- function(X, params) {

  X_names = params$X_names
  forc_vals = params$forc_vals
  n_years = length(forc_vals)

  if(is.null(forc_vals)) {
    stop("Error in solve_ebm_wrapper: Params list has no entry forc_vals.")
  }

  vars = extract_variables(X, params)
  lambda_vec = vars$lambda
  weights_vec = vars$weights
  Cap = vars$Cap
  T0 = vars$T0
  F0 = vars$F0

  sol <- solve_ebm(lambda_vec, weights_vec, T0, F0, Cap, forc_vals)
  return(sol)
}
