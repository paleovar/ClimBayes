
#' Function to compute MCSE, based on book by Kruschke
#' (Doing Bayesian Data Analysis)
#'
#' For each column in the chain, compute
#' \deqn{\text{MCSE} = \frac{\sigma_x}{\sqrt{\text{ESS}}}},
#' where \eqn{\sigma_x} is the standard deviation of the respective column
#' @param chain Matrix of MCMC chain, each row a sample, each column a parameter
#' @param ESS Vector effective sample sizes for each column;
#' if NULL, will be computed using package coda
#' @return Vector of MCSE values, one entry for each column in the chain
#' @noRd
MCSE <- function(chain, ESS = NULL) {

  if(is.null(ESS)) {
    ESS <- coda::effectiveSize(coda::mcmc(chain))
  }
  d = ncol(chain)
  return(sapply(1:d, function(j)
    sd(chain[, j]) / sqrt(ESS[j])))
}

#' Helper function to computer degrees of freedom,
#' required for termination method based on Flegal
#' @param n number of samples
#' @return Degrees of freedom
#' @noRd
dof_MCSE <- function(n) {
  # n is number of samples
  b = floor(sqrt(n))
  a = floor(n / b)
  return(a)
}

#' Helper function to extract samples from chain list,
#' separates burn_in samples from rest,
#' and concatenates the chain list to one long chain
extract_samples <- function(chain_list, first_sample) {

  # concatenate to one long chain for results
  # each ROW is a sample, each COLUMN a variable
  selected_samples <- lapply(chain_list, function(chain)
    chain$samplesMCMC[first_sample:nrow(chain$samplesMCMC),  , drop = FALSE])
  long_chain <- do.call(rbind, selected_samples)

  # save burn_in samples for later
  first_samples <- lapply(chain_list, function(chain)
    chain$samplesMCMC[1:(first_sample-1),  , drop = FALSE])
  burnin_samples <- do.call(rbind, first_samples)

  return(list(long_chain = long_chain,
              burnin_samples = burnin_samples,
              chain_list_sel = selected_samples))
}

#' Helper function to extract solution matrix from chain list,
#' separates burn_in samples from rest,
#' and concatenates the chain list to one long chain

extract_sol_matrix <- function(chain_list, first_sample) {
  # extracted solution matrix from chain_list
  # in long_chain_sol, each ROW is a sample EBM-solution, each COL a point in time
  selected_sol <- lapply(chain_list, function(chain)
    chain$sol_matrix[first_sample:nrow(chain$samplesMCMC), , drop = FALSE])
  long_chain_sol <- do.call(rbind, selected_sol)

  # save burn_in samples for later
  first_sol <- lapply(chain_list, function(chain)
    chain$sol_matrix[1:(first_sample-1),  , drop = FALSE])
  burnin_sol <- do.call(rbind, first_sol)

  return(list(long_chain_sol = long_chain_sol,
              burnin_sol = burnin_sol))
}
