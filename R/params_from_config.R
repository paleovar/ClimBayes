#' Convert config list into parameter list
#'
#' Helper function to extract internal parameter list from config list
#'
#' @param n_boxes Number of boxes of multibox EBM.
#' @param n_years Number of years considered.
#' @param config_list A nested list of configuration parameters. This should
#' only be a single config, i.e. it usually is a sublist of
#' `yaml::read_yaml("ebm_fit_config.yml")`.
#' @return List of parameters.
#' @export
#' @examples
#' params_from_config(1, 100, yaml::read_yaml("ebm_fit_config.yml")$default)
params_from_config <- function(n_boxes, n_years, config_list) {
  # --- load config file ---
  params <- list()

  # --- define parameter names to be estimated ---
  X_names <- paste0("lambda", 1:n_boxes)
  boxes <- paste0(english::as.english(n_boxes), "_box")

  if(n_boxes > 1) {
    if(config_list$parameters$weights == 'estimate') {
      X_names = c(X_names, paste0("weights", 2:n_boxes))
    } else if(config_list$parameters$weights == 'fixed'){
      for(i in 2:n_boxes) {
        params[[paste0("weights", i, "_default")]] =
          config_list$parameter_defaults[[boxes]][["weights"]][i - 1]
      }
    } else {
      stop("Please change configuration of weights.")
    }
  }

  if(config_list$parameters$Cap == 'estimate') {
    X_names = c(X_names, "Cap")
  } else {
    params$C_default = config_list$parameter_defaults$Cap
  }

  if(config_list$parameters$F0 == 'estimate') {
    X_names = c(X_names, "F0")
  } else {
    params$F0_default = config_list$parameter_defaults$F0
  }

  params$X_names = X_names

  # --- parameters related to observation grid ---

  params$n_years = n_years
  params$obs_grid_step = 1
  # note: in the future this could be something like
  # config_list$observations$observation_grid_step
  n_observations = length(seq(1, n_years, params$obs_grid_step))
  obs_grid = seq(1, n_years, by = params$obs_grid_step)
  params$obs_grid = obs_grid

  # --- Parameters related to prior distributions ---
  if(n_boxes > 1) {
    for(j in 2:n_boxes) {
      params[[paste0("weights", j, "_lb")]] <- 0
      params[[paste0("weights", j, "_ub")]] <- 1
    }
  }

  if(config_list$priors$type == "uniform") {
    params$prior_type = "unif"
  } else if(config_list$priors$type == "beta") {
    params$prior_type = "beta"
    boxes <- paste0(english::as.english(n_boxes), "_box")
    shape1 = numeric(0)
    shape2 = numeric(0)
    for(name in X_names) {
      if(startsWith(name, "lambda")) {
        i <- as.numeric(gsub(".*?([0-9]+)$", "\\1", name))
        shape1 <- c(shape1, config_list$priors$beta_shape1$lambda[[boxes]][i])
        shape2 <- c(shape2, config_list$priors$beta_shape2$lambda[[boxes]][i])
      } else if(startsWith(name, "weights")) {
        i <- as.numeric(gsub(".*?([0-9]+)$", "\\1", name))
        shape1 <- c(shape1, config_list$priors$beta_shape1$weights[[boxes]][i - 1])
        shape2 <- c(shape2, config_list$priors$beta_shape2$weights[[boxes]][i - 1])
      } else if(name == "F0") {
        shape1 <- c(shape1, config_list$priors$beta_shape1$F0)
        shape2 <- c(shape2, config_list$priors$beta_shape2$F0)
      } else if(name == "Cap") {
        shape1 <- c(shape1, config_list$priors$beta_shape1$Cap)
        shape2 <- c(shape2, config_list$priors$beta_shape2$Cap)
      }
    }
    params$prior_beta_shape1 = shape1
    params$prior_beta_shape2 = shape2
  }

  # bounds of prior intervals
  if(config_list$parameters$Cap == 'estimate') {
    params$Cap_lb = config_list$priors$Cap_bounds[1]
    params$Cap_ub = config_list$priors$Cap_bounds[2]
  }
  if(config_list$parameters$F0 == 'estimate') {
    params$F0_lb = config_list$priors$F0_bounds[1]
    params$F0_ub = config_list$priors$F0_bounds[2]
  }

  for(i in 1:n_boxes) {
    params[[paste0("lambda", i, "_lb")]] =
      config_list$priors[[boxes]][[paste0("lambda", i, "_bounds")]][1]
    params[[paste0("lambda", i, "_ub")]] =
      config_list$priors[[boxes]][[paste0("lambda", i, "_bounds")]][2]
  }

  # --- extract mean of lambda and weights values for later use ---

  lambda_mean = numeric(n_boxes)
  for (i in 1:n_boxes) {
    var = paste0("lambda", i)
    lambda_mean[i] = (params[[paste0(var, "_ub")]] -
                        params[[paste0(var, "_lb")]]) / 2.0
  }

  if(n_boxes > 1) {
    if(config_list$parameters$weights == 'estimate') {
      weights_mean = rep(0.5, n_boxes)
    } else if(config_list$parameters$weights == 'fixed'){
      weights_mean = config_list$parameter_defaults[[boxes]][["weights"]]
      weights_mean = c(1 - sum(weights_mean), weights_mean)

    } else {
      stop("Please change configuration of weights.")
    }
  } else {
    weights_mean = 1
  }


  # --- parameters related to noise in likelihood ---

  if(config_list$noise$type == 'white_fixed') {
    params$noise_process = "none"
    params$meas_noise = config_list$noise$white_fixed$SD
  } else if (config_list$noise$type == 'white_iterative'){
    params$noise_process = "iterative_white"
    params$meas_noise = config_list$noise$white_iterative$SD
  } else if(config_list$noise$type == 'ar1_iterative') {
    params$noise_process = "iterative_correlated"
    params$meas_noise = config_list$noise$ar1_iterative$SD_white
    sd_ar1_default = config_list$noise$ar1_iterative$SD_ar1_default
    params$noise_factor = sx_from_sd(sd_ar1_default,
                                             lambda_mean, weights_mean)
  } else if(config_list$noise$type == 'ar1_fixed') {
    params$noise_process = "fixed_cov"
    boxes <- paste0(english::as.english(n_boxes), "_box")

    lambda = config_list$noise$ar1_fixed$lambda[[boxes]]
    if(n_boxes > 1) {
      weights = c(1 - sum(config_list$noise$ar1_fixed[[boxes]]$weights[[boxes]]),
                  config_list$noise$ar1_fixed$weights[[boxes]])
    } else {
      weights = 1
    }
    sd_ar1 = config_list$noise$ar1_fixed$SD_ar1
    noise_factor = sx_from_sd(sd_ar1, lambda, weights)
    sigma_noise = setup_sigma(n_years, lambda, weights, params$obs_grid_step,
                              noise_factor,
                              config_list$noise$ar1_fixed$SD_white)
    params$sigma_e_inv = solve(sigma_noise)
  }

  # --- Parameters for MCMC algorithm ---

  params$M <- config_list$metropolis_hastings$n_samples
  params$burn_in <- (params$M /
                       config_list$metropolis_hastings$burn_in_proportion)
  proposal_variance = config_list$metropolis_hastings$proposal_variance
  if(proposal_variance == "auto") {
    scaling = 1e-4
    s_lambda = numeric(n_boxes)
    for (i in 1:n_boxes) {
      s_lambda[i] = lambda_mean[i] * scaling
    }
    s = s_lambda
    if(n_boxes > 1 & config_list$parameters$weights == 'estimate') {
      s = c(s, rep(scaling, times = n_boxes - 1))
    }
    if(config_list$parameters$Cap == 'estimate') {
      s = c(s, scaling * (params$Cap_ub - params$Cap_lb) * 0.5)
    }
    if(config_list$parameters$F0 == 'estimate') {
      s = c(s, scaling * (params$F0_ub - params$F0_lb) * 0.5)
    }
    params$s <- s
  } else if(proposal_variance == "default") {
    prop_defaults = config_list$metropolis_hastings$proposal_variance_defaults

    s_lambda = numeric(n_boxes)
    for (i in 1:n_boxes) {
      s_lambda[i] = prop_defaults$lambda[[boxes]][i]
    }
    s = s_lambda
    if(n_boxes > 1 & config_list$parameters$weights == 'estimate') {
      s = c(s, rep(prop_defaults$weights, times = n_boxes - 1))
    }
    if(config_list$parameters$Cap == 'estimate') {
      s = c(s, prop_defaults$Cap)
    }
    if(config_list$parameters$F0 == 'estimate') {
      s = c(s, prop_defaults$F0)
    }
    params$s <- s
  } else {
      stop("Option for proposal variance not implemented.
          Please adjust config file.")
  }
  params$alpha <- config_list$metropolis_hastings$alpha
  params$n_chains <- config_list$metropolis_hastings$n_chains
  params$dynamic_termination <- config_list$metropolis_hastings$dynamic_termination
  params$dynamic_stepsize <- config_list$metropolis_hastings$n_samples_dynamic
  error_tol = config_list$metropolis_hastings$error_tolerance

  epsilon_lambda = numeric(n_boxes)
  for (i in 1:n_boxes) {
    var = paste0("lambda", i)
    epsilon_lambda[i] = lambda_mean[i] * error_tol
  }
  epsilon = epsilon_lambda
  if(n_boxes > 1) {
    epsilon = c(epsilon, rep(0.5 * error_tol, times = n_boxes - 1))
  }
  if(config_list$parameters$Cap == 'estimate') {
    epsilon = c(epsilon, error_tol * (params$Cap_ub - params$Cap_lb) * 0.5)
  }
  if(config_list$parameters$F0 == 'estimate') {
    epsilon = c(epsilon, error_tol * (params$F0_ub - params$F0_lb) * 0.5)
  }
  params$epsilon = epsilon
  params$gelman_threshold = config_list$metropolis_hastings$gelman_diagnostic

  # --- extra ----
  params$return_solution_matrix = config_list$extra$return_solution_matrix
  params$parallel = config_list$extra$parallel
  params$parallel_free_cores = config_list$extra$parallel_free_cores

  return(params)
}

