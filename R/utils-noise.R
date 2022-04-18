
# ---------- Convert internal noise factor and sd for AR(1) noise ----

#' Get SD of the AR(1) noise from the `noise_factor`.
#'
#' The `noise_factor` is also $\sigma_x in the literature, hence the name.
#' It is the scaling factor
#' in front of the OU-processes, i.e. for $\sigma$ from the paper, we have
#' $\sigma_x = \sigma / C$ with heat capacity $C$.
#'
#' @param sx The parameter `noise_factor`.
#' @param lambda Vector of lambda values.
#' @param weights Vector of weights values.
#' @return Value of $sigma_x$.
#' @noRd
sd_from_sx <- function(sx, lambda, weights) {
  var_terms <- outer(weights, weights) *
    outer(lambda, lambda, function(x, y) 1/(x+y))
  sd <- sx * sqrt(sum(var_terms))
  return(sd)
}

#' Get the `noise_factor` from the SD of the AR(1) noise.
#'
#' The `noise_factor` is also $\sigma_x in the literature, hence the name.
#' It is the scaling factor
#' in front of the OU-processes, i.e. for $\sigma$ from the paper, we have
#' $\sigma_x = \sigma / C$ with heat capacity $C$.
#'
#' @param sd The SD of the AR(1) noise.
#' @param lambda Vector of lambda values.
#' @param weights Vector of weights values.
#' @return Corresponding value of the `noise_factor` in the parameters list.
#' @noRd
sx_from_sd <- function(sd, lambda, weights) {
  var_terms <- outer(weights, weights) *
    outer(lambda, lambda, function(x, y) 1/(x+y))
  sx <- sd / sqrt(sum(var_terms))
  return(sx)
}





