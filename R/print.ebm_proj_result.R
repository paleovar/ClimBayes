#' Print method for class `ebm_proj_result`.
#'
#' @export
print.ebm_proj_result <- function(res_list) {
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
  n_years_fit = length(res_list$posteriors$model_fit$mean)
  cat(paste0("Temperature of model fit in year ", res_list$meta$end_year, ": ",
             round(res_list$posteriors$model_fit$mean[n_years_fit], 2), " K.\n"))
  n_years_total = length(res_list$projection$result$mean)
  cat(paste0("Then projected for further ", n_years_total - n_years_fit, " years. \n"))
  cat(paste0("Projection at year ",
             res_list$meta$end_year +  n_years_total - n_years_fit, ": ",
             round(res_list$projection$result$mean[n_years_total], 2),
             " K. \n"))
}
