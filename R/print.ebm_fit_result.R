#' Print method for class `ebm_fit_result`.
#'
#' @export
print.ebm_fit_result <- function(res_list) {
  cat(paste0("Fitted ", res_list$meta$n_boxes, "-box EBM to data \n",
             "from years ", res_list$meta$start_year, " to ",
             res_list$meta$end_year, "\n"))
  cat("Estimated parameters: \n")
  for(i in 1:length(res_list$input_params$X_names)) {
    var_name = res_list$input_params$X_names[i]
    cat(
      paste0("Parameter ", var_name, " with mean: ",
      round(as.numeric(res_list$posteriors$parameters[[var_name]][["mean"]]), 3), "\n"
    ))
  }
  cat(paste0("RMSE: ",
             round(RMSE(res_list$input_params$y_obs, res_list$posteriors$model_fit$median), 3),
             "\n"))
}
