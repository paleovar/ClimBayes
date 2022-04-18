#' ODE solver for linear multibox EBMs
#'
#' Solves the linear multibox EBMs for a vector of $\lambda$ and weights values.
#' @param lambda: Vector of values for feedback parameters, one for each box
#' @param weights: Vector of weights values, one for each box;
#' should sum up to 1, otherwise an error is thrown.
#' @param F0: Initial forcing value in $W / m^2$.
#' @param Cap: Heat capacity in $\mathrm{W yr m^{−2} K^{−1}}$.
#' @param forc_vals: Vector of forcing values (yearly data),
#' anomalies w.r.t. first data point.
#' @return: Vector of EBM solution, one entry for each year,
#' anomalies w.r.t. first data point.
#' @export
#' @examples
#' # Response to step increase in forcing
#' lambda <- 0.1
#' weights <- 1 # in one-box model, weights are always one
#' forc_vals <- c(0, numeric(99) + 10)
#' solve_ebm(0.1, 1, 0, 10.1, forc_vals)
solve_ebm <- function(lambda, weights, F0, Cap, forc_vals) {
  if(!all.equal(sum(weights), 1)) {
    stop("Error in solve_ebm: Weights do not sum to 1.")
  }
  if(!all.equal(forc_vals[1], 0)) {
    stop("Error in solve_ebm: First forcing value not 0.")
  }
  n_years = length(forc_vals)
  sol <- ode_helper(n_years, lambda, weights, forc_vals, F0)
  return(1/Cap * sol)
}
