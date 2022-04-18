#' Log-likelihood function for linear multibox models
#'
#' @param X Vector of parameter values.
#' @param params List of parameters; relevant entries:
#' * `y_obs`: vector of temperature observations
#' * `noise_process`: `"none"` (then only white noise is considered)
#' or `"fixed_cov"` (then noise where the inverse of the covariance is `sigma_e_inv`)
#' * `meas_noise`: (only if `noise_process = "none"`) SD of the white noise
#' * `sigma_e_inv`: (only if `noise_process = "fixed_cov"`)
#' Inverse of covariance matrix
#' * all parameters relevant for the forward operator `forward_op`.
#' @param sol (optional) Vector of EBM solution for this value of X.
#' This can be used to speed up the code and avoid repeated calculations.
#' @return Value of the log of likelihood density.
#' @export
log_likelihood <- function(X,
                           params,
                           sol = NULL
) {
  y_obs = params$y_obs

  meas_noise = params$meas_noise

  phi_x <- forward_op(X, params, sol = sol)

  if (params$noise_process == "none") {
    if(meas_noise == 0) {
      stop("If noise_process is none, need parameter meas_noise > 0.")
    }
    return(-0.5 / (meas_noise^2) * sum((y_obs - phi_x)^2))
  } else if (params$noise_process == "fixed_cov") {
    diff = y_obs - phi_x
    first <- my_mm(params$sigma_e_inv, diff)
    return(as.numeric(-0.5 * t(my_mm(t(first), diff))))
  } else {
    stop("Option for noise process in log_likelihood not implemented.")
  }

}


