#' Generate AR(1) noise for class `ebm_fit_result`
#'
#' Wrapper for `gen_noise`.
#' Extracts the posterior means from the `ebm_fit_result` and uses them in `gen_noise`
#' to generate samples of AR(1) noise. The parameters `lambda` and `weights`
#' of the AR(1) noise equal the posterior means.
#' The SD of the AR(1) noise is chosen s.t. it equals the SD of the residual from the fit.
#'
#' @param n Number of samples.
#' @param res_list Object of class `ebm_fit_result`
#' @return Matrix where each row is a sample of the noise,
#' each column corresponds to a point in time.
#' @export
gen_noise_from_ebm_fit <- function(n, res_list) {
  if(is.null(res_list$posteriors$model_fit$mean)) {
    message("No mean of model_fit present in data. Possibly you used an old data set.
            Using median instead.")
    fit <- res_list$posteriors$model_fit$median
  } else {
    fit <- res_list$posteriors$model_fit$mean
  }

  # generate samples from noise with corresponding parameters
  # first, find SD of the noise
  sd_resid <- sd(res_list$input_params$y_obs - fit)

  # generate noise with parameters equal to posterior means
  lw <- get_post_means(res_list)
  lambda <- lw$lambda
  weights <- lw$weights
  # again call same function as above
  noise <- gen_noise(n, lambda, weights, res_list$input_params$n_years,
                     0, sd_resid)
  noise
}
