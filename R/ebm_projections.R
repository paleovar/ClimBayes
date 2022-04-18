#' Generate temperature projections from EBM fit and additional forcing series.
#'
#' Takes result of `ebm_fit` and uses the posterior parameters to project the EBM
#' further into the future.
#'
#' @param res_list Object of class `ebm_fit_result`
#' @param forc Forcing for which the projections should be computed. Vector of forcing values or data frame with columns `year` and `forcing`.
#' @param start_year First of forcing considered (only relevant if `forc` is a data frame).
#' @param end_year Last year of forcing considered (inclusive; only relevant if `forc` is a data frame).
#' @param cred_int Boolean -
#' * If TRUE, credible intervals are computed also (much slower than without!)
#' * If FALSE, only the posterior means of the parameters are extracted and EBM projection is computed with this.
#' @return Object of class `ebm_proj_result`, inherits from `ebm_fit_result`.
#' In addition to the entries of `ebm_fit_result` (see documentation there), it contains an entry:
#' * `projection`
#'     + `input`: list with entry `forc_vals`
#'     + `result`: list with `median, mean, lower_quant, upper_quant`
#' @export
ebm_projection <- function(res_list, forc,
                           start_year = NULL,
                           end_year = NULL,
                           cred_int = FALSE) {

  forc_vec = forc_vec_from_input(forc, start_year=start_year, end_year=end_year)

  X_names = res_list$input_params$X_names
  burn_in = res_list$input_params$burn_in
  n_chains = res_list$input_params$n_chains

  n_years <- res_list$input_params$n_years
  params <- list(forc_vals = c(res_list$input_params$forc_vals,
                               forc_vec - forc_vec[1] +
                                 res_list$input_params$forc_vals[n_years]),
                 C_default = res_list$input_params$C_default,
                 F0_default = res_list$input_params$F0_default,
                 X_names = X_names)
  res_list$projection$input <- list(forc_vals = params$forc_vals)

  if(!cred_int) {
    # to do: convert structure from res_list to vector

    d = length(X_names)
    post_means = numeric(d)
    parameter_results = res_list$posteriors$parameters
    for (j in 1:d) {
      var_name = X_names[j]
      post_means[j] = parameter_results[[var_name]][["mean"]]
    }
    X = post_means
    sol = solve_ebm_wrapper(X, params)
    res_list$projection$result <- list(mean = sol)
  }

  if(cred_int) {
    samples <- res_list$samples$parameters
    start = max(nrow(samples) - 1e4, (n_chains * burn_in + 1))
    all_sols <- t(apply(samples[start:nrow(samples), ], 1,
                        function(X) solve_ebm_wrapper(X, params)))
    # now each ROW is a sample solution for the scenario
    temp_quantiles <- apply(all_sols, 2, quantile,
                            probs = c(0.025, 0.5, 0.975))
    temp_mean <- apply(all_sols, 2, mean)
    temp = temp_quantiles[2, ]
    T_lower = temp_quantiles[1, ]
    T_upper = temp_quantiles[3, ]
    res_list$projection$result <- list(median = temp,
                                lower_quant = T_lower,
                                upper_quant = T_upper,
                                mean = temp_mean)
  }

  class(res_list) <- c("ebm_proj_result", class(res_list))
  return(res_list)
}
