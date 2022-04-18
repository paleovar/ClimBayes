#' Single iteration to check influence of noise sensitivity
#'
#' Generates synthetic data as the EBM solution with given variables `vars` and forcing `forc`
#' plus noise with SDs `sd_white` and `sd_ar1`. Then fits the EBM to these synthetic observations
#' and returns the result.
#'
#' @param forc Vector of forcing values or data frame with columns `year` and `forcing`.
#' @param vars list of data-generating variables with entries
#' * `lambda`
#' * `weights`
#' * `Cap`
#' * `F0`
#' @param sd_white SD of white noise
#' @param sd_ar1 SD of AR(1) noise
#' @param start_year First of forcing considered (only relevant if `forc` is a data frame).
#' @param end_year Last year of forcing considered (inclusive; only relevant if `forc` is a data frame).
#' @param config_file Path to config file or full config list.
#' @param config Name of config to use.
#' @return Object of class `ebm_fit` - the result of the fit to the synthetic observations.
#' @export
#' @examples
#' vars = list(lambda = 0.1, Cap = 10.1, weights = 1, F0 = 0)
#' noise_sens_single_it(c(0, numeric(99) + 10), vars, 0.1, 0, 1, 100)
noise_sens_single_it <- function(forc, vars,
                                 sd_white, sd_ar1,
                                 start_year = NULL, end_year = NULL,
                                 config_file = "ebm_fit_config_noise_sens.yml",
                                 config = "noise_sens") {

  # convert forcing input into correct vector
  forc_vec <- forc_vec_from_input(forc, start_year, end_year)

  # helper parameters
  n_years = length(forc_vec)
  n_boxes = length(vars$lambda)

  # extract variables
  lambda_vec = vars$lambda
  weights_vec = vars$weights
  Cap = vars$Cap
  F0 = vars$F0

  # extract the params from config file
  if(is.character(config_file)) {
    config_file <- yaml::read_yaml(config_file, eval.expr = TRUE)
  }

  # set correct noise variables
  # theoretically, this could all be set in the config file
  # but the noise parameters depend on the function arguments sd_ar1 and sd_white
  # so for more flexibility, we set them here again
  boxes <- paste0(english::as.english(n_boxes), "_box")
  config_file[[config]]$noise$ar1_fixed$lambda[[boxes]] = lambda_vec
  if(length(lambda_vec) > 1) {
    config_file[[config]]$noise$ar1_fixed$weights[[boxes]] = weights_vec[2:length(weights_vec)]
  }
  config_file[[config]]$noise$ar1_fixed$SD_ar1 = sd_ar1
  config_file[[config]]$noise$ar1_fixed$SD_white = sd_white


  # generate synthetic data
  # generated with forcing values and fixed parameters
  sol <- solve_ebm(lambda_vec, weights_vec, F0, Cap,
                                  forc_vec - forc_vec[1])
  noise <- as.numeric(gen_noise(1, lambda_vec, weights_vec, length(forc_vec),
                                sd_white, sd_ar1))
  data <- sol + noise

  # fit model again to synthetic data
  res_list <- ebm_fit(data, forc, n_boxes, start_year, end_year,
                      config_file = config_file, config = config)

  return(res_list)
}
