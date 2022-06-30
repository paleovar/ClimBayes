#' Main function
#'
#' Fit EBM to data
#' @param obs Vector of temperature observation values or data frame with columns `year` and `temperature`.
#' @param forc Vector of forcing values or data frame with columns `year` and `forcing`.
#' @param n_boxes Number of boxes in the EBM.
#' @param start_year First of forcing considered (only relevant if `forc` or `obs` is a data frame,
#' otherwise entire vector is taken).
#' @param end_year Last year of forcing considered (inclusive; only relevant if `forc` or `obs` is a data frame,
#' otherwise entire vector is taken).
#' @param detrending Boolean - indicates if observations and forcing
#' should be detrended linearly before applying the Bayesian fit.
#' @param config_file Path to config file or full config list.
#' @param config Name of config to use.
#' @return Object of class `ebm_fit_result`, inherits from `mcmc_result`.
#' In addition contains an entry
#' *  `meta`: list with entries
#'     + `n_boxes`
#'     + `start_year`
#'     + `end_year`
#'
#' Just like objects of class `mcmc_result`, it also contains entries:
#' * `input_params`: Input parameters (copy of `params`)
#' * `posteriors`: List that contains posterior means, medians and variance for each estimated parameter
#' * `samples`: List with entries
#'     + `parameters`
#' (matrix of samples,  one row for each sample, one column for each variable)
#'     + `model_fit` (matrix of EBM solutions (for all samples (potentially large!),
#' one row for each sample, one column for each point in time).
#' Note the ordering: first the burn-in samples from all chains, then the rest.
#' * `diagnostics`: (only if diagnostics is TRUE) list with diagnostic criteria
#' @export
ebm_fit <- function(obs, forc, n_boxes,
                    start_year = NULL,
                    end_year = NULL,
                    detrending = FALSE,
                    config_file = system.file('extdata/ebm_fit_config.yml', package = 'ClimBayes'),
                    config = "experimental"
                    ) {

  obs_vec <- obs_vec_from_input(obs, start_year, end_year)
  forc_vec <- forc_vec_from_input(forc, start_year, end_year)

  # error handling
  if(length(obs_vec) != length(forc_vec)) {
    stop("Please supply observations and forcing of the same length.")
  }

  if(detrending) {
   obs_vec <- detrend(obs_vec)
   forc_vec <- detrend(forc_vec)
  }

  params <- list(
    y_obs = obs_vec - obs_vec[1],
    forc_vals = forc_vec - forc_vec[1]
  )

  n_years = length(obs_vec)
  if(is.character(config_file)) {
    config_list <- yaml::read_yaml(config_file, eval.expr = TRUE)[[config]]
  } else {
    config_list = config_file[[config]]
  }
  params <- c(params,
              params_from_config(n_boxes, n_years, config_list))
  res_list <- MCMC_run(params)
  res_list$meta$n_boxes <- n_boxes
  if(is.null(start_year)) {
    start_year <- 0
  }
  res_list$meta$start_year <- start_year
  res_list$meta$end_year <- start_year + n_years - 1
  class(res_list) <- c("ebm_fit_result", class(res_list))

  return(res_list)
}
