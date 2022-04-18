#' Forward operator in Bayesian inference for linear multibox models
#'
#' Solves the EBM and evaluates it at the time points given in `obs_grid`.
#'
#' @export
forward_op <- function(X, params, sol = NULL) {
  if(is.null(sol)) {
    sol <- solve_ebm_wrapper(X, params)
  }
  if(length(params$obs_grid) == 0) {
    stop("Error in forward operator: Please provide an observation grid.")
  }
  sol[params$obs_grid]
}
