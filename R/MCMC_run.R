#' Bayesian inference via MCMC approach
#'
#' Computes one or several Markov Chains with a Metropolis Hastings algorithm
#'
#' @param params Full parameter list.
#' @param diagnostics Boolean - if TRUE, criteria for performance diagnostics are
#' calculated and returned.
#' @return Object of class `mcmc_result`, a nested list with the following entries:
#' * `input_params`: Input parameters (copy of `params`)
#' * `posteriors`: List that contains posterior means, medians and variance for each estimated parameter
#' * `samples`: List with entries
#'     + `parameters`
#' (matrix of samples,  one row for each sample, one column for each variable)
#'     + `model_fit` (matrix of EBM solutions (for all samples (potentially large!),
#' one row for each sample, one column for each point in time).
#' Note the ordering: first the burn-in samples from all chains, then the rest.
#' * `diagnostics`: (only if diagnostics is TRUE) list with diagnostic criteria
#' * `results_first_it`: (only if the noise is set in an iterative scheme)
#' some results of the first iteration
#' @export
MCMC_run <- function(params, diagnostics = TRUE) {
  res_list <- list()
  res_list[["input_params"]] <- params

  X_names = params$X_names
  d = length(X_names)
  burn_in = params$burn_in
  M = params$M
  n_chains = params$n_chains

  if(M %% 2 != 0) {
    M = M + 1
  }
  if((M - burn_in) %% 2 != 0) {
    # ensure the number of remaining samples is even
    burn_in = burn_in + 1
  }
  first_sample = burn_in + 1

  if(is.null(params$fine_integration)) {
    params$fine_integration = FALSE
  }

  # iterative noise parameter estimation
  if(params$noise_process == "iterative_correlated") {
    cat("Starting first iteration")

    # --- setup noise parameters for first iteration ---
    n_boxes <- sum(startsWith(X_names, "lambda"))
    lambda_noise <- numeric(n_boxes)

    for (i in 1:n_boxes) {
      var_name = paste0("lambda", i)
      lb = params[[paste0(var_name, "_lb")]]
      ub = params[[paste0(var_name, "_ub")]]
      lambda_noise[i] = lb + 1/2 * (ub - lb)
    }

    if(sum(startsWith(X_names, "weights"))> 0) {
      weights_noise <- rep(1/n_boxes, n_boxes)
    } else if (n_boxes > 1) {
      weights_noise = numeric(n_boxes - 1)
      for(i in 2:n_boxes) {
        var_name = paste0("weights", i)
        weights_noise[i - 1] = params[[paste0(var_name,"_default")]]
      }
      weights_noise = c(1 - sum(weights_noise), weights_noise)
    } else {
      weights_noise = 1
    }

    sigma_e = setup_sigma(length(params$y_obs), lambda_noise, weights_noise,
                          params$obs_grid_step, params$noise_factor,
                          params$meas_noise)
    sigma_e_inv = solve(sigma_e)
    params$sigma_e_inv = sigma_e_inv
    params$noise_process = "fixed_cov"

    # --- run first iteration with only half as many samples ---
    params$M <- M / 2.0
    chain_list <- MCMC_parallel_chains(params)

    # --- extract samples and solution matrix from this run ----
    samples <- extract_samples(chain_list, floor(first_sample / 2.0))$long_chain
    sol_matrix <- extract_sol_matrix(chain_list, floor(first_sample / 2.0))$long_chain_sol
    sol_mean <- apply(sol_matrix, 2, mean)
    residual <- params$y_obs - sol_mean[params$obs_grid]


    # --- set parameters for next actual run ---
    params$M <- M

    if(n_boxes == 1) {
      ind <- which(startsWith(X_names, "lambda"))
      lambda_mean = mean(samples[, ind])

      # also estimate sx
      if(sd(residual) - params$meas_noise > 0) {
        params$noise_factor <- sqrt(2 * lambda_mean) *
          (sd(residual) - params$meas_noise)
      } else {
        params$noise_factor <- 0
        warning("Measurement noise is larger than standard deviation of residual.
                Model does not include correlated noise.
                Please decrease params$meas_noise to avoid this.")
      }
      weights_mean_full <- 1
    } else {

      lambda_mean <- sapply(1:n_boxes, function(j)
        mean(samples[,startsWith(X_names, "lambda")][,j]))
      if(sum(startsWith(X_names, "weights"))> 0) {
        weights_mean <- sapply(1:(n_boxes - 1), function(j)
          mean(samples[,startsWith(X_names, "weights"), drop = FALSE][,j]))
      } else {
        weights_mean = numeric(n_boxes - 1)
        for(i in 2:n_boxes) {
          var_name = paste0("weights", i)
          weights_mean[i - 1] = params[[paste0(var_name,"_default")]]
        }
      }

      weights_mean_full <- c(1 - sum(weights_mean), weights_mean)
      if(sd(residual) - params$meas_noise > 0) {
        var_terms <- outer(weights_mean_full, weights_mean_full) *
          outer(lambda_mean, lambda_mean, function(x, y) 1/(x+y))
        params$noise_factor <- (sd(residual) - params$meas_noise) /
          sqrt(sum(var_terms))
      } else {
        params$noise_factor <- 0
        warning("Measurement noise is larger than standard deviation of residual.
                Model does not include correlated noise.
                Please decrease params$meas_noise to avoid this.")
      }
    }

    res_list$results_first_it <- list(
      lambda_mean = lambda_mean,
      sd_residual = sd(residual),
      noise_factor_estimate =  params$noise_factor
    )

    sigma_e = setup_sigma(length(params$y_obs), lambda_mean, weights_mean_full,
                          params$obs_grid_step, params$noise_factor,
                          params$meas_noise)
    sigma_e_inv = solve(sigma_e)
    params$sigma_e_inv = sigma_e_inv
    params$noise_process = "fixed_cov"
    cat("\r", "Starting second iteration")

  } else if (params$noise_process == "iterative_white") {

    # ---- first iteration ----------
    params$noise_process = "none"
    params$M <- M / 2.0
    chain_list <- MCMC_parallel_chains(params)

    # --- setting actual parameters ---
    params$M <- M
    samples <- extract_samples(chain_list, floor(first_sample / 2.0))$long_chain
    sol_matrix <- extract_sol_matrix(chain_list, floor(first_sample / 2.0))$long_chain_sol
    sol_mean <- apply(sol_matrix, 2, mean)
    residual <- params$y_obs - sol_mean[params$obs_grid]
    params$meas_noise <- sd(residual)
    # print(paste0("first estimate of sm: ", params$meas_noise))
    res_list$results_first_it <- list(
      sd_residual = sd(residual),
      meas_noise_estimate =  params$meas_noise
    )
  }

  # --------------- actual run ------------------

  chain_list <- MCMC_parallel_chains(params)

  all_samples <- extract_samples(chain_list, first_sample)
  long_chain <- all_samples$long_chain
  burnin_samples <- all_samples$burnin_samples
  chain_list_sel <- all_samples$chain_list_sel

  # ---- dynamic sample size ---------------------
  if(params$dynamic_termination) {

    if(n_chains == 1) {
      stop("Need at least two chains for dynamic termination.")
    }
    MCSE_combined <- as.vector(batchmeans::bmmat(long_chain)[, "se"])
    a = dof_MCSE(nrow(long_chain))

    n = nrow(chain_list_sel[[1]]) #number of samples in each chain
    split_chains <- c(lapply(chain_list_sel, function(chain) chain[1:floor(n/2), ]),
                      lapply(chain_list_sel, function(chain) chain[(floor(n/2) + 1):n, ]))
    gelman_diag <- coda::gelman.diag(lapply(split_chains, coda::mcmc), autoburnin = FALSE)

    if(d == 1) {
      R_hat <- gelman_diag$psrf[2]
    } else {
      R_hat <- gelman_diag$mpsrf
    }

    n_total <- nrow(long_chain)
    while((all(MCSE_combined * qt(0.95, a - 1) > params$epsilon) |
           R_hat > params$gelman_threshold) &
          n_total < 1e6) {

      if(is.null(params$dynamic_stepsize)) {
        params$dynamic_stepsize <- params$M
      }
      # new parameters
      params$M <- params$dynamic_stepsize
      params$burn_in <- 0
      # sample again

      if(!params$parallel) {
        for (j in 1:n_chains) {
          X_start <- as.vector(chain_list[[j]]$samplesMCMC[nrow(chain_list[[j]]$samplesMCMC), ])
          new_chain <- MCMC_chain(X_start, params)
          chain_list[[j]]$samplesMCMC <- rbind(chain_list[[j]]$samplesMCMC,
                                               new_chain$samplesMCMC)
          chain_list[[j]]$sol_matrix <- rbind(chain_list[[j]]$sol_matrix,
                                              new_chain$sol_matrix)
          chain_list_sel[[j]] <- rbind(chain_list_sel[[j]],
                                         new_chain$samplesMCMC)
        }
      } else {
        cores = parallel::detectCores()
        cl <- parallel::makeCluster(max(n_chains, cores - params$parallel_free_cores),
                                    type="FORK") #not to overload your computer
        doParallel::registerDoParallel(cl)

        new_chains <- foreach(j=1:n_chains) %dorng% {
          X_start <- as.vector(chain_list[[j]]$samplesMCMC[nrow(chain_list[[j]]$samplesMCMC), ])
          MCMC_chain(X_start, params)
        }
        doParallel::stopImplicitCluster()
        parallel::stopCluster(cl)

        for (j in 1:n_chains) {
          chain_list[[j]]$samplesMCMC <- rbind(chain_list[[j]]$samplesMCMC,
                                               new_chains[[j]]$samplesMCMC)
          chain_list[[j]]$sol_matrix <- rbind(chain_list[[j]]$sol_matrix,
                                              new_chains[[j]]$sol_matrix)
          chain_list_sel[[j]] <- rbind(chain_list_sel[[j]],
                                         new_chains[[j]]$samplesMCMC)
        }
      }



      long_chain <- do.call(rbind, chain_list_sel)

      MCSE_combined <- as.vector(batchmeans::bmmat(long_chain)[, "se"])
      a = dof_MCSE(nrow(long_chain))

      n = nrow(chain_list_sel[[1]]) #number of samples in each chain
      split_chains <- c(lapply(chain_list_sel, function(chain) chain[1:floor(n/2), ]),
                        lapply(chain_list_sel, function(chain) chain[(floor(n/2) + 1):n, ]))
      gelman_diag <- coda::gelman.diag(lapply(split_chains, coda::mcmc), autoburnin = FALSE)
      if(d == 1) {
        R_hat <- gelman_diag$psrf[2]
      } else {
        R_hat <- gelman_diag$mpsrf
      }


      n_total <- nrow(long_chain)
      if (n_total >= 1e6) {
        warning("Stopping criterion still not fulfilled. Stop sampling now to avoid too long runtimes.")
      }
    }
  }

  # ---- extract results ---------------------
  all_sols <- extract_sol_matrix(chain_list, first_sample)
  long_chain_sol <- all_sols$long_chain_sol
  burnin_sol <- all_sols$burnin_sol

  parameter_results <- list()
  for (j in 1:d) {
    var_name = X_names[j]
    est_mean = mean(long_chain[, j])
    med = median(long_chain[, j])
    variance <- var(long_chain[, j])
    parameter_results[[var_name]] <- list(median = med,
                                          mean = est_mean,
                                          variance = variance)
  }

  temp_quantiles <- apply(long_chain_sol, 2, quantile,
                          probs = c(0.025, 0.5, 0.975))

  temp_mean <- apply(long_chain_sol, 2, mean)

  model_fit_results <- list(
    mean = temp_mean,
    median = temp_quantiles[2, ],
    lower_quant = temp_quantiles[1, ],
    upper_quant = temp_quantiles[3, ]
  )

  samplesMCMC <- rbind(burnin_samples, long_chain)
  sol_matrix <- rbind(burnin_sol, long_chain_sol)

  if(params$return_solution_matrix) {
    res_list[['samples']] <- list(parameters = samplesMCMC,
                                  model_fit = sol_matrix)
  }
  else {
    res_list[['samples']] <- list(parameters = samplesMCMC)
  }
  res_list[['posteriors']] <- list(parameters = parameter_results,
                                   model_fit = model_fit_results)

  if(diagnostics) {
    # additionally compute MCMC diagnostics

    MCMC_diag <- list()

    ESS_combined <- coda::effectiveSize(coda::mcmc(long_chain))
    ESS_single_chains <- lapply(chain_list_sel, coda::effectiveSize)

    MCSE_combined <- as.vector(batchmeans::bmmat(long_chain)[, "se"])
    MCSE_single_chains <- lapply(chain_list_sel, function(samples)
      as.vector(batchmeans::bmmat(samples)[, "se"]))

    # compute number of batches as in batchmeans package
    # needed for degrees of freedom for t statistic

    if(n_chains > 1) {
      n = nrow(chain_list_sel[[1]]) #number of samples in each chain
      split_chains <- c(lapply(chain_list_sel, function(chain) chain[1:floor(n/2), ]),
                        lapply(chain_list_sel, function(chain) chain[(floor(n/2) + 1):n, ]))

      gelman_diag <- coda::gelman.diag(lapply(split_chains, coda::mcmc), autoburnin = FALSE)
      MCMC_diag[['gelman_diag_psrf']] <- gelman_diag$psrf
      MCMC_diag[['gelman_diag_mpsrf']] <- gelman_diag$mpsrf
    }

    MCMC_diag[['ESS_combined']] <- ESS_combined
    MCMC_diag[['ESS_single_chains']] <- ESS_single_chains
    MCMC_diag[['MCSE_combined']] <- MCSE_combined
    a = dof_MCSE(nrow(long_chain))
    MCMC_diag[['interval_halfwidth_90']] <- MCSE_combined * qt(0.90, a - 1)
    MCMC_diag[['interval_halfwidth_95']] <- MCSE_combined * qt(0.95, a - 1)
    MCMC_diag[['MCSE_single_chains']] <- MCSE_single_chains
    acc_rate <- (nrow(samplesMCMC) - sum(duplicated(samplesMCMC, MARGIN = 1))) / nrow(samplesMCMC)
    MCMC_diag[['acceptance_rate']] <- acc_rate
    #show(gelman.plot(mcmc.list(?), autoburnin = FALSE))
    res_list[['diagnostics']] <- MCMC_diag
  }

  on.exit({
    try({
      # cat("Attempting to stop cluster in MCMC run\n")
      doParallel::stopImplicitCluster()
      parallel::stopCluster(cl)
    }, silent = TRUE)
  })
  class(res_list) <- "mcmc_result"
  return(res_list)
}
