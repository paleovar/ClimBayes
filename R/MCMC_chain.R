#' Compute a single Markov chain with the Metropolis Hastings algorithm
#'
#' Called by `MCMC_run`.
#'
#' @param X_start Initial values for parameters
#' @param X_names variable names of corresponding parameters
#' @param params List of parameters;
#' relevant entries (for more details, see vignette for config file):
#' * `alpha`: weighting factor for adaptive covariance scheme
#' * `s`: proposal variance
#' * `M`: number of samples
#' @return List with entries:
#' * `samplesMCMC`: Matrix of samples, one row for each sample, one column for each variable
#' * `sol_matrix`: Matrix of model solutions, one row for each sample, one column for each year
#' @export
MCMC_chain <- function(X_start, params) {
  X_names = params$X_names
  d = length(X_names)
  s = params$s
  alpha = params$alpha
  sigma = diag(s, d)

  X <- X_start
  sol_X <- solve_ebm_wrapper(X, params)
  p_X <- neg_log_post(X, params, sol = sol_X)

  M = params$M
  samplesMCMC = matrix(NA, nrow = M, ncol = d)
  sol_matrix = matrix(NA, nrow = M, ncol = params$n_years)

  burn_in = params$burn_in
  start = burn_in + 1
  end = M

  adaptive_start = round(burn_in / 2, 0)

  for (m in 1:M) {
    # print(paste0("sample number: ", m))
    if (m <= adaptive_start + 10) {
      X_prop <- mvtnorm::rmvnorm(1, mean = X, sigma = sigma)
    } else {
      # adaptive MCMC scheme
      sigma = (alpha * cov(samplesMCMC[adaptive_start:(m-1), , drop = FALSE])
               + (1-alpha) * diag(s, d))
      X_prop <- mvtnorm::rmvnorm(1, mean = X, sigma = sigma)
    }

    u <- runif(1, 0, 1)

    sol_prop <- solve_ebm_wrapper(X_prop, params)
    p_prop <- neg_log_post(X_prop, params, sol = sol_prop)

    acc_prob <- exp(min(0, - p_prop + p_X))

    tryCatch(
      {
        x <- u <= acc_prob
        if(x) {
        }
      },
      error = function(cond) {
        message("Failed in MCMC sampling")
        message(p_prop)
        message(p_X)
        message(cond)
        stop("Stopping now")
      }
    )

    if (u <= acc_prob) {
      samplesMCMC[m, ] <- X_prop
      sol_matrix[m, ] <- sol_prop
      sol_X = sol_prop
      p_X = p_prop
      X = X_prop
    } else {
      samplesMCMC[m, ] <- X
      sol_matrix[m, ] <- sol_X
    }
  }
  return(list(samplesMCMC = samplesMCMC,
              sol_matrix = sol_matrix))
}
