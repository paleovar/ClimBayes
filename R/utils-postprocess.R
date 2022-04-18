#' Function to compute the Root Mean Square Error (RMSE)
#' @param y_obs Vector of observation values
#' @param temp Vector of  temperature values to compare with observations,
#' should be of the same length as observations
#' @return RMSE
#' @noRd
RMSE <- function(y_obs, temp) {
  if(length(temp) != length(y_obs)) {
    stop("Cannot calculate RMSE due to different length")
  }
  sqrt(1 / length(y_obs) * sum((y_obs - temp)^2))
}

#' Helper function to create a dictionary of Latex labels
#' @noRd
create_labs_dict <- function(X_names) {
  d = length(X_names)
  labs_dict = list("F0" = unname(latex2exp::TeX("$F_0$")),
                   "Cap" = unname(latex2exp::TeX("$C$")))
  for(j in 1:d) {
    var_name = X_names[j]
    if(startsWith(var_name, "lambda")) {
      ind = sub("lambda", "", var_name)
      labs_dict[[var_name]] = unname(latex2exp::TeX(paste0("$\\lambda_", ind, "$")))
    } else if(startsWith(var_name, "weights")) {
      ind = sub("weights", "", var_name)
      labs_dict[[var_name]] = unname(latex2exp::TeX(paste0("$\\w_", ind, "$")))
    }
  }
  return(labs_dict)
}
