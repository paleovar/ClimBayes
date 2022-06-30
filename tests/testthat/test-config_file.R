test_that("Parameter options", {
  config_list <- yaml::read_yaml("../testconfig.yml")

  fit <- ebm_fit(numeric(10), numeric(10), 1,
          config_file = config_list, config = "test_default")
  expect_equal(names(fit$posteriors$parameters), c("lambda1", "T0", "F0"))

  config_list$test_default$parameters$Cap <- "estimate"
  fit <- ebm_fit(numeric(10), numeric(10), 1,
                config_file = config_list, config = "test_default")
  expect_equal(names(fit$posteriors$parameters), c("lambda1", "Cap", "T0", "F0"))

  fit <- ebm_fit(numeric(10), numeric(10), 2,
                 config_file = config_list, config = "test_default")
  expect_equal(names(fit$posteriors$parameters),
               c("lambda1", "lambda2", "weights1", "Cap", "T0", "F0"))

  config_list$test_default$parameters$weights <- "fixed"
  fit <- ebm_fit(numeric(10), numeric(10), 2,
                 config_file = config_list, config = "test_default")
  expect_equal(names(fit$posteriors$parameters),
               c("lambda1", "lambda2", "Cap", "T0", "F0"))

  config_list$test_default$parameters$T0 <- "fixed"
  fit <- ebm_fit(numeric(10), numeric(10), 2,
                 config_file = config_list, config = "test_default")
  expect_equal(names(fit$posteriors$parameters),
               c("lambda1", "lambda2", "Cap", "F0"))

  config_list$test_default$parameters$F0 <- "fixed"
  fit <- ebm_fit(numeric(10), numeric(10), 2,
                 config_file = config_list, config = "test_default")
  expect_equal(names(fit$posteriors$parameters),
               c("lambda1", "lambda2", "Cap"))
})

test_that("Parameter defaults", {
  lambda = 0.3
  Cap = 20 # change Capacity from default value
  Feq = 3
  forc <- c(0, numeric(20) + Feq)
  obs <- solve_ebm(lambda, 1, 0, 0, Cap, forc)
  config_list <- yaml::read_yaml("../testconfig.yml")
  config_list$test_default$parameter_defaults$Cap = 20
  fit <- ebm_fit(obs, forc, 1,
                 config_file = config_list,
                 config = "test_default")
  Teq = 1/lambda/Cap * Feq

  expect_gte(fit$posteriors$model_fit$median[21], Teq * 0.85)
  expect_lte(fit$posteriors$model_fit$median[21], Teq * 1.15)

  lambda1 = 1
  lambda2 = 0.1
  weights1 = 0.95
  Cap = 10.1
  Feq = 3
  forc <- c(0, numeric(20) + Feq)
  obs <- solve_ebm(c(lambda1, lambda2), c(weights1, 1 - weights1), 0, 0, Cap, forc)
  config_list <- yaml::read_yaml("../testconfig.yml")
  config_list$test_default$parameters$weights = "fixed"
  config_list$test_default$parameter_defaults$two_box$weights = weights1
  fit <- ebm_fit(obs, forc, 2,
                 config_file = config_list,
                 config = "test_default")
  Teq = 1/Cap * Feq *(weights1 / lambda1 + (1 - weights1) / lambda2)

  expect_gte(fit$posteriors$model_fit$median[21], Teq * 0.85)
  expect_lte(fit$posteriors$model_fit$median[21], Teq * 1.15)
})

test_that("Noise settings", {
  config_list <- yaml::read_yaml("../testconfig.yml")
  obs = numeric(10)
  forc = numeric(10)

  config_list$test_default$noise$type = "white_iterative"
  # check that no error occurs
  expect_error(ebm_fit(obs, forc, 1,
                       config_file = config_list,
                       config = "test_default"),
               NA)

  config_list$test_default$noise$type = "ar1_fixed"
  expect_error(ebm_fit(obs, forc, 1,
                       config_file = config_list,
                       config = "test_default"),
               NA)

  config_list$test_default$noise$type = "ar1_iterative"
  config_list$test_default$noise$ar1_iterative$SD_white = 0
  expect_error(ebm_fit(obs, forc, 1,
                       config_file = config_list,
                       config = "test_default"),
               NA)
})

test_that("Prior settings, parameter bounds", {
  config_list <- yaml::read_yaml("../testconfig.yml")
  obs = numeric(10)
  forc = numeric(10)
  fit <- ebm_fit(obs, forc, 1,
                 config_file = config_list,
                 config = "test_default")
  fit2 <- ebm_fit(obs, forc, 2,
                 config_file = config_list,
                 config = "test_default")

  l1_lower = config_list$test_default$priors$one_box$lambda1_bounds[1]
  l1_upper = config_list$test_default$priors$one_box$lambda1_bounds[2]
  expect_equal(all(l1_lower <= fit$samples$parameters[,1]), TRUE)
  expect_equal(all(fit$samples$parameters[,1] <= l1_upper), TRUE)

  expect_lte(l1_lower, fit$posteriors$parameters$lambda1$lower_quant)
  expect_gte(l1_upper, fit$posteriors$parameters$lambda1$upper_quant)

  F0_lower = config_list$test_default$priors$F0_bounds[1]
  F0_upper = config_list$test_default$priors$F0_bounds[2]
  expect_equal(all(F0_lower <= fit$samples$parameters[,3]), TRUE)
  expect_equal(all(fit$samples$parameters[,3] <= F0_upper), TRUE)
  expect_lte(F0_lower, fit$posteriors$parameters$F0$lower_quant)
  expect_gte(F0_upper, fit$posteriors$parameters$F0$upper_quant)

  expect_equal(all(F0_lower <= fit2$samples$parameters[,5]), TRUE)
  expect_equal(all(fit2$samples$parameters[,5] <= F0_upper), TRUE)
  expect_lte(F0_lower, fit2$posteriors$parameters$F0$lower_quant)
  expect_gte(F0_upper, fit2$posteriors$parameters$F0$upper_quant)

  T0_lower = config_list$test_default$priors$T0_bounds[1]
  T0_upper = config_list$test_default$priors$T0_bounds[2]
  expect_equal(all(T0_lower <= fit$samples$parameters[,2]), TRUE)
  expect_equal(all(fit$samples$parameters[,2] <= T0_upper), TRUE)
  expect_lte(T0_lower, fit$posteriors$parameters$T0$lower_quant)
  expect_gte(T0_upper, fit$posteriors$parameters$T0$upper_quant)

  expect_equal(all(T0_lower <= fit2$samples$parameters[,4]), TRUE)
  expect_equal(all(fit2$samples$parameters[,4] <= T0_upper), TRUE)
  expect_lte(T0_lower, fit2$posteriors$parameters$T0$lower_quant)
  expect_gte(T0_upper, fit2$posteriors$parameters$T0$upper_quant)

  l1_lower = 0.5
  l1_upper = 1.5
  config_list$test_default$priors$one_box$lambda1_bounds[1] = l1_lower
  config_list$test_default$priors$one_box$lambda1_bounds[2] = l1_upper
  fit <- ebm_fit(obs, forc, 1,
                 config_file = config_list,
                 config = "test_default")
  expect_equal(all(l1_lower <= fit$samples$parameters[,1]), TRUE)
  expect_equal(all(fit$samples$parameters[,1] <= l1_upper), TRUE)

  T0_lower = -0.1
  T0_upper = 0.1
  config_list$test_default$priors$T0_bounds[1] = T0_lower
  config_list$test_default$priors$T0_bounds[2] = T0_upper
  fit <- ebm_fit(obs, forc, 1,
                 config_file = config_list,
                 config = "test_default")
  expect_equal(all(T0_lower <= fit$samples$parameters[,2]), TRUE)
  expect_equal(all(fit$samples$parameters[,2] <= T0_upper), TRUE)
  expect_lte(T0_lower, fit$posteriors$parameters$T0$lower_quant)
  expect_gte(T0_upper, fit$posteriors$parameters$T0$upper_quant)

  fit2 <- ebm_fit(obs, forc, 2,
                  config_file = config_list,
                  config = "test_default")
  expect_equal(all(T0_lower <= fit2$samples$parameters[,4]), TRUE)
  expect_equal(all(fit2$samples$parameters[,4] <= T0_upper), TRUE)
  expect_lte(T0_lower, fit2$posteriors$parameters$T0$lower_quant)
  expect_gte(T0_upper, fit2$posteriors$parameters$T0$upper_quant)
})

test_that("Beta prior", {
  config_list <- yaml::read_yaml("../testconfig.yml")
  obs = numeric(10)
  forc = numeric(10)
  l1_lower = 0.5
  l1_upper = 1.5
  config_list$test_default$priors$one_box$lambda1_bounds[1] = l1_lower
  config_list$test_default$priors$one_box$lambda1_bounds[2] = l1_upper
  config_list$test_default$priors$type = "beta"
  config_list$test_default$priors$beta_shape1$lambda$one_box = 10
  config_list$test_default$priors$beta_shape2$lambda$one_box = 2
  fit <- ebm_fit(obs, forc, 1,
                 config_file = config_list,
                 config = "test_default")
  expect_gte(fit$posteriors$parameters$lambda1$mean, mean(c(l1_lower, l1_upper)))

  config_list$test_default$priors$beta_shape1$lambda$one_box = 2
  config_list$test_default$priors$beta_shape2$lambda$one_box = 10
  fit <- ebm_fit(obs, forc, 1,
                 config_file = config_list,
                 config = "test_default")
  expect_lte(fit$posteriors$parameters$lambda1$mean, mean(c(l1_lower, l1_upper)))
})

test_that("Prior settings 2-box", {
  config_list <- yaml::read_yaml("../testconfig.yml")
  config_list$test_default$priors$type = "beta"
  obs = numeric(10)
  forc = numeric(10)

  l2_lower = 0.01
  l2_upper = 0.1
  l1_lower = 0.1
  l1_upper = 1.0
  config_list$test_default$priors$two_box$lambda1_bounds[1] = l1_lower
  config_list$test_default$priors$two_box$lambda1_bounds[2] = l1_upper
  config_list$test_default$priors$two_box$lambda2_bounds[1] = l2_lower
  config_list$test_default$priors$two_box$lambda2_bounds[2] = l2_upper
  fit <- ebm_fit(obs, forc, 2,
                 config_file = config_list,
                 config = "test_default")
  expect_equal(all(l1_lower <= fit$samples$parameters[,1]), TRUE)
  expect_equal(all(fit$samples$parameters[,1] <= l1_upper), TRUE)
  expect_equal(all(l2_lower <= fit$samples$parameters[,2]), TRUE)
  expect_equal(all(fit$samples$parameters[,2] <= l2_upper), TRUE)
})

test_that("MH", {
  config_list <- yaml::read_yaml("../testconfig.yml")
  config_list$test_default$metropolis_hastings$n_samples <- 1000
  config_list$test_default$metropolis_hastings$n_chains <- 2
  obs = numeric(10)
  forc = numeric(10)
  fit <- ebm_fit(obs, forc, 1,
                 config_file = config_list,
                 config = "test_default")
  expect_length(as.vector(fit$samples$parameters[,1]), 2000)


  lambda = 0.3
  Cap = 10
  Feq = 3
  forc <- c(0, numeric(20) + Feq)
  obs <- solve_ebm(lambda, 1, 0, 0, Cap, forc) + rnorm(1, 0, 0.01)
  config_list <- yaml::read_yaml("../testconfig.yml")
  config_list$test_default$metropolis_hastings$dynamic_termination <- TRUE
  config_list$test_default$metropolis_hastings$error_tolerance <- 0.1
  config_list$test_default$metropolis_hastings$gelman_diagnostic <- 1.1
  fit <- ebm_fit(obs, forc, 1,
                 config_file = config_list,
                 config = "test_default")
  Teq = 1/lambda/Cap * Feq

  expect_lte(fit$diagnostics$gelman_diag_mpsrf, 1.1)

  config_list$test_default$metropolis_hastings$chain_start_buffer <- 0.1
  expect_error(ebm_fit(obs, forc, 1,
                       config_file = config_list,
                       config = "test_default"),
               NA)
})
