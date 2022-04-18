# -------- Helper for solver and density ------------

#' Helper function to extract values of single variables from `X` and `params`
#'
#' @param X Vector of parameter values.
#' @param param Parameter list including default values and `X_names`,
#' specifying which parameters should be estimated.
#' @return List with parameter values, split up into entries:
#' * lambda
#' * weights
#' * F0
#' * Cap
#' @noRd
extract_variables <- function(X, params) {
  X_names = params$X_names
  F0 = params$F0_default
  Cap = params$C_default

  # overwrite if the above variables are part of tuning parameters
  # inefficient, can speed this up!
  for(i in 1:length(X)) {
    var_name = X_names[i]
    assign(var_name, X[i])
  }
  n_boxes = sum(startsWith(X_names, "lambda"))
  lambda_vec <- X[startsWith(X_names, "lambda")]

  if(n_boxes == 1) {
    weights_vec = 1
  } else if(sum(startsWith(X_names, "weights")) == 0) {
    weights_vec = numeric(n_boxes - 1)
    for(i in 2:n_boxes) {
      var_name = paste0("weights", i)
      weights_vec[i - 1] = params[[paste0(var_name,"_default")]]
    }
    weights_vec <- c(1 - sum(weights_vec), weights_vec)
  } else if (n_boxes == sum(startsWith(X_names, "weights")) + 1) {
    weights_vec <- X[startsWith(X_names, "weights")]
    weights_vec <- c(1 - sum(weights_vec), weights_vec)
  }
  return(list(
    lambda = lambda_vec,
    weights = weights_vec,
    F0 = F0,
    Cap = Cap
  ))
}
