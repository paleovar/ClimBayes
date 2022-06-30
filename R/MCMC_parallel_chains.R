#' Function to run several chains with dispersed starting points,
#' possibly in parallel
#'
#' @param params Parameter list. In addition to those relevant in `MCMC_chain`,
#' the following entries are required:
#' * `parallel`: Boolean, should the computation run in parallel_
#' * `X_names`
#' * `n_chains`: Number of chains
#' @return list of chains, each element as returned from `MCMC_chain`
#' @export
MCMC_parallel_chains <- function(params) {

  if(is.null(params$parallel)) {
    params$parallel = FALSE
  }

  n_chains = params$n_chains
  X_names = params$X_names
  d = length(X_names)

  if(!params$parallel) {
    chain_list <- list()
    for (j in 1:n_chains) {
      # starting values should span the parameter space
      # but not include its boundaries
      start <- 1/(n_chains + 1) * j

      X_start = numeric(d)
      for (i in 1:d) {
        var_name = X_names[i]
        lb = params[[paste0(var_name, "_lb")]]
        ub = params[[paste0(var_name, "_ub")]]
        if(!is.null(params$chain_start_buffer)) {
          lb = lb + params$chain_start_buffer * (ub - lb)
          ub = ub - params$chain_start_buffer * (ub - lb)
        }
        X_start[i] = lb + start * (ub - lb)
      }
      chain_list[[j]] <- MCMC_chain(X_start, params)
    }
  } else {
    #setup parallel backend to use many processors
    cores = parallel::detectCores()
    cl <- parallel::makeCluster(max(n_chains, cores - params$parallel_free_cores),
                                type="FORK") # not to overload your computer
    doParallel::registerDoParallel(cl)

    chain_list <- foreach::foreach(j=1:n_chains) %dorng% {
      # starting values should span the parameter space
      # but not include its boundaries
      start <- 1/(n_chains + 1) * j

      X_start = numeric(d)
      for (i in 1:d) {
        var_name = X_names[i]
        lb = params[[paste0(var_name, "_lb")]]
        ub = params[[paste0(var_name, "_ub")]]
        if(!is.null(params$chain_start_buffer)) {
          lb = lb + params$chain_start_buffer * (ub - lb)
          ub = ub - params$chain_start_buffer * (ub - lb)
        }
        X_start[i] = lb + start * (ub - lb)
      }
      MCMC_chain(X_start, params)
    }
    doParallel::stopImplicitCluster()
    parallel::stopCluster(cl)
  }
  return(chain_list)
}
