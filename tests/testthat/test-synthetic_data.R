test_that("reasonable output for step function", {
  lambda = 0.3
  Cap = 10
  Feq = 3
  forc <- c(0, numeric(20) + Feq)
  obs <- solve_ebm(lambda, 1, 0, Cap, forc)
  config_list <- yaml::read_yaml("../testconfig.yml")
  config_list$test_default$metropolis_hastings$n_samples <- 1000
  fit <- ebm_fit(obs, forc, 1,
                 config_file = config_list,
                 config = "test_default")
  Teq = 1/lambda/Cap * Feq

  expect_gte(fit$posteriors$model_fit$median[21], Teq * 0.9)
  expect_lte(fit$posteriors$model_fit$median[21], Teq * 1.1)

  lambda = 0.2
  Cap = 10
  Feq = 3
  forc <- c(0, numeric(20) + Feq)
  obs <- solve_ebm(lambda, 1, 0, Cap, forc)
  config_list <- yaml::read_yaml("../testconfig.yml")
  config_list$test_default$metropolis_hastings$n_samples <- 1000
  fit <- ebm_fit(obs, forc, 1,
                 config_file = config_list,
                 config = "test_default")
  Teq = 1/lambda/Cap * Feq

  expect_gte(fit$posteriors$model_fit$median[21], Teq * 0.9)
  expect_lte(fit$posteriors$model_fit$median[21], Teq * 1.1)

  lambda = 0.2
  Cap = 10
  Feq = 2
  forc <- c(0, numeric(20) + Feq)
  obs <- solve_ebm(lambda, 1, 0, Cap, forc)
  config_list <- yaml::read_yaml("../testconfig.yml")
  config_list$test_default$metropolis_hastings$n_samples <- 1000
  fit <- ebm_fit(obs, forc, 1,
                 config_file = config_list,
                 config = "test_default")
  Teq = 1/lambda/Cap * Feq

  expect_gte(fit$posteriors$model_fit$median[21], Teq * 0.9)
  expect_lte(fit$posteriors$model_fit$median[21], Teq * 1.1)

  lambda = 0.2
  Cap = 20
  Feq = 2
  forc <- c(0, numeric(20) + Feq)
  obs <- solve_ebm(lambda, 1, 0, Cap, forc)
  config_list <- yaml::read_yaml("../testconfig.yml")
  config_list$test_default$metropolis_hastings$n_samples <- 1000
  fit <- ebm_fit(obs, forc, 1,
                 config_file = config_list,
                 config = "test_default")
  Teq = 1/lambda/Cap * Feq

  expect_gte(fit$posteriors$model_fit$median[21], Teq * 0.9)
  expect_lte(fit$posteriors$model_fit$median[21], Teq * 1.1)
})
