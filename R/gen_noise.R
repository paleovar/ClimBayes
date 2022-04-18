#' Generate noise (AR(1) plus white)
#'
#' @param n Number of samples.
#' @param lambda Vector of lambda values.
#' @param weights Vector of weights values.
#' @param n_years Number of years (i.e. length of the noise).
#' @param sd_white SD of white noise.
#' @param sd_ar1 SD of AR(1) noise.
#' @return Matrix where each row is a sample,
#' each column corresponds to a point in time.
#' @examples
#' # only noisy time series with a bit of correlation
#' as.vector(gen_noise(1, 0.1, 1, 51, 0.1, 0.3))
#' # fit plus noise
#' solve_ebm(0.1, 1, 0, 10, c(0, rep(5,50))) + as.vector(gen_noise(1, 0.1, 1, 51, 0.1, 0.3))
#' @export
gen_noise <- function(n, lambda, weights, n_years, sd_white, sd_ar1) {

  if(sd_white == 0 & sd_ar1 == 0) {
    return(numeric(n_years))
  }
  sx <- sx_from_sd(sd_ar1, lambda, weights)
  sigma <- setup_sigma(n_years, lambda, weights, 1,
                       sx, sd_white)
  noise <- mvtnorm::rmvnorm(n, rep(0, nrow(sigma)), sigma)
  return(noise)
}


