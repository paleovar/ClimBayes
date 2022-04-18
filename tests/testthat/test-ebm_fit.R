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

test_that("reasonable output for synthetic data", {
  forc <- c(0, numeric(20) + 3)
  obs <- solve_ebm(0.3, 1, 0, 10.1, forc)
  config_list <- yaml::read_yaml("../testconfig.yml")
  config_list$test_default$metropolis_hastings$n_samples <- 1000
  fit <- ebm_fit(obs, forc, 1,
                 config_file = config_list,
                 config = "test_default")

  expect_gte(fit$posteriors$parameters$lambda1$mean, 0.2)
  expect_lte(fit$posteriors$parameters$lambda1$mean, 0.4)

  forc <- c(0, numeric(20) + 3)
  obs <- solve_ebm(0.3, 1, 0, 10.1, forc) + rnorm(21, 0, 0.02)
  config_list <- yaml::read_yaml("../testconfig.yml")
  config_list$test_default$metropolis_hastings$n_samples <- 1000
  fit <- ebm_fit(obs, forc, 1,
                 config_file = config_list,
                 config = "test_default")

  expect_gte(fit$posteriors$parameters$lambda1$mean, 0.2)
  expect_lte(fit$posteriors$parameters$lambda1$mean, 0.4)

  expect_error(plot(fit), NA)
  expect_error(print(fit), NA)

})
