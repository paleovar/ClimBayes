#' Analyze influence of noise in observational data on parameter estimates
#'
#' Function computes uncertainties in Bayesian parameter estimates due to noise in observations.
#' It generates synthetic data as the EBM solution with given variables `vars` and forcing `forc`
#' plus noise with SDs `sd_white` and `sd_ar1`. Then fits the EBM to these synthetic observations
#' and calculates the difference of the data-generating parameter values and the estimates.
#' This is repeated for each combination of SDs in `sd_white_list` and `sd_ar1_list` and `reps` times.
#' Internally, the function uses `noise_sens_single_it`.
#'
#' @param sd_white_list Vector of SD values for white noise.
#' @param sd_ar1_list Vector of SD values for AR(1) noise.
#' @param reps Number of repetitions.
#' @param forc Vector of forcing values or data frame with columns "year" and "forcing".
#' @param vars List of data-generating variables with entries
#' - lambda
#' - weights
#' - Cap
#' - F0
#' @param start_year First of forcing considered (only relevant if `forc` is a data frame).
#' @param end_year Last year of forcing considered (inclusive; only relevant if `forc` is a data frame).
#' @param config_file Path to config file or full config list.
#' @param config Name of config to use.
#' @return Tibble with one row for each iteration and columns:
#' - `sd_white`: SD of white noise used in data-generation
#' - `sd_ar1`: SD of AR(1) noise used in data-generation
#' - `rep`: Number of repetition
#' - `delta_lambda1`: Difference of data-generating value (i.e. that in `vars$lambda[1]`)
#' and the parameter estimate
#' - (possibly further columns `delta_lambda2` etc.)
#' @export
#' @examples
#' vars = list(lambda = 0.1, Cap = 10.1, weights = 1, F0 = 0)
#' noise_sensitivity(c(0, 0.1), c(0, 0.1), 10,
#' forc = c(0, numeric(99) + 10), vars = vars)
noise_sensitivity <- function(sd_white_list, sd_ar1_list,
                              reps,
                              forc, vars,
                              start_year = NULL, end_year = NULL,
                              config_file = "ebm_fit_config_noise_sens.yml",
                              config = "noise_sens") {

  # try all combinations of SD values in both lists
  # each combination reps
  results <- tidyr::crossing(sd_white = sd_white_list,
                             sd_ar1 = sd_ar1_list,
                             rep = 1:reps)

  for(i in 1:length(vars$lambda)) {
    results[[paste0('delta_lambda', i)]] <- 0
  }

  # not very efficient because the setup is done multiple times
  # even for the same set of parameters
  # but the MCMC stuff is most costly anyways, so I guess it doesn't matter
  for (i in 1:nrow(results)) {
    # do single iteration
    sd_white = as.numeric(results[i, 'sd_white'])
    sd_ar1 = as.numeric(results[i, 'sd_ar1'])

    res_list <- noise_sens_single_it(forc, vars, sd_white, sd_ar1,
              start_year, end_year,
              config_file, config)
    print(res_list)

    # compute errors and store them in results data frame
    for(j in 1:length(vars$lambda)) {
      results[i, paste0('delta_lambda', j)] <-
        res_list$posteriors$parameters[[paste0('lambda', j)]][['mean']] - vars$lambda[j]
    }
  }
  return(results)
}



