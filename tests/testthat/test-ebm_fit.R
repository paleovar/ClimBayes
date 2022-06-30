test_that("test that different input formats are accepted", {
  obs <- numeric(10)
  forc <- numeric(10)
  fit <- ebm_fit(obs, forc, 1,
                  config_file = "../testconfig.yml",
                  config = "test_default")

  expect_vector(fit$posteriors$model_fit$median, size = 10)

  obs <- tibble::tibble(year = 1:10, temperature = 0)
  forc <- tibble::tibble(year = 1:10, forcing = 0)
  fit <- ebm_fit(obs, forc, 1,
          config_file = "../testconfig.yml",
          config = "test_default")

  expect_vector(fit$posteriors$model_fit$median, size = 10)

  fit <- ebm_fit(obs, forc, 1,
          start_year = 1,
          end_year = 5,
          config_file = "../testconfig.yml",
          config = "test_default")

  expect_vector(fit$posteriors$model_fit$median, size = 5)
})

test_that("Test plot and print", {
  obs <- numeric(10)
  forc <- numeric(10)
  fit <- ebm_fit(obs, forc, 1,
                 config_file = "../testconfig.yml",
                 config = "test_default")

  expect_error(plot(fit), NA)
  expect_error(print(fit), NA)
})

test_that("Check fit for multibox model", {
  obs <- numeric(10)
  forc <- numeric(10)
  expect_error(ebm_fit(obs, forc, 2,
                 config_file = "../testconfig.yml",
                 config = "test_default"), NA)

})

test_that("reasonable output for synthetic data", {
  forc <- c(0, numeric(20) + 3)
  obs <- solve_ebm(0.3, 1, 0, 0, 10.1, forc)
  config_list <- yaml::read_yaml("../testconfig.yml")
  config_list$test_default$metropolis_hastings$n_samples <- 1000
  fit <- ebm_fit(obs, forc, 1,
                 config_file = config_list,
                 config = "test_default")

  expect_gte(fit$posteriors$parameters$lambda1$mean, 0.2)
  expect_lte(fit$posteriors$parameters$lambda1$mean, 0.4)

  forc <- c(0, numeric(20) + 3)
  obs <- solve_ebm(0.3, 1, 0, 0, 10.1, forc) + rnorm(21, 0, 0.01)
  config_list <- yaml::read_yaml("../testconfig.yml")
  config_list$test_default$metropolis_hastings$n_samples <- 1000
  fit <- ebm_fit(obs, forc, 1,
                 config_file = config_list,
                 config = "test_default")

  expect_gte(fit$posteriors$parameters$lambda1$mean, 0.2)
  expect_lte(fit$posteriors$parameters$lambda1$mean, 0.4)

  # two box
  forc <- c(0, numeric(50) + 3)
  obs <- solve_ebm(c(0.5, 0.05), c(0.9, 0.1), 0, 0, 10.1, forc)
  config_list <- yaml::read_yaml("../testconfig.yml")
  config_list$test_default$parameters$weights = "fixed"
  config_list$test_default$parameter_defaults$two_box$weights = 0.9
  config_list$test_default$metropolis_hastings$n_samples <- 3000
  fit <- ebm_fit(obs, forc, 2,
                 config_file = config_list,
                 config = "test_default")
  plot_fit(fit)
  print(fit)

  expect_gte(fit$posteriors$parameters$lambda2$mean, 0.03)
  expect_lte(fit$posteriors$parameters$lambda2$mean, 0.07)
  expect_gte(fit$posteriors$parameters$lambda1$mean, 0.3)
  expect_lte(fit$posteriors$parameters$lambda1$mean, 1.5)

})
