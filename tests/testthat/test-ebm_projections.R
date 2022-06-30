test_that("Test projection", {
  forc <- c(0, numeric(20) + 3)
  obs <- solve_ebm(0.3, 1, 0, 0, 10.1, forc)
  fit <- ebm_fit(obs, forc, 1,
                 config_file = "../testconfig.yml",
                 config = "test_default")

  forc_proj = c(0, numeric(9) + 3)
  proj <- ebm_projection(fit, forc_proj)
  expect_error(plot(proj), NA)
  expect_error(print(proj), NA)

  proj <- ebm_projection(fit, forc_proj, cred_int = TRUE)
  expect_error(plot(proj), NA)
  expect_error(print(proj), NA)
})
