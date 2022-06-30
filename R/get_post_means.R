#' Helper function to extract posterior means from `ebm_fit_result`
#'
#' @param res_list Object of class `ebm_fit_result`.
#' @return List with entries
#' * `lambda`: Vector of posterior means for all `lambda` parameters
#' * `weights`: Vector of posterior means for all `weights` parameters (including the first one,
#  such that all values add to one)
#' @export
get_post_means <- function(res_list) {

  lambda = numeric(res_list$meta$n_boxes)
  weights = numeric(res_list$meta$n_boxes - 1)
  for(j in 1:res_list$meta$n_boxes) {
    lambda[j] = res_list$posteriors$parameters[[paste0("lambda", j)]]$mean
    if(j < res_list$meta$n_boxes & res_list$meta$n_boxes > 1) {
      weights[j] = res_list$posteriors$parameters[[paste0("weights", j)]]$mean
    }
  }
  weights = c(weights, 1 - sum(weights))
  return(list(lambda = lambda, weights = weights))
}
