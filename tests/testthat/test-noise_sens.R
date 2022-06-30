test_that("Test noise sens 1-box", {
  forc = c(0, numeric(20) + 10)
  vars = list(lambda = 0.1, weights = 1, Cap = 10.1, T0 = 0, F0 = 0)
  expect_error(noise_sens_single_it(forc, vars,
                                   sd_white = 0.5, sd_ar1 = 0.1,
                                   config_file = "../../ebm_fit_config.yml",
                                   config = "noise_sens"),
               NA)

  expect_error(noise_sensitivity(sd_white_list = c(0.5, 1),
                                 sd_ar1_list = 0,
                                 reps = 2,
                                 forc = forc,
                                 vars = vars,
                                 config_file = "../../ebm_fit_config.yml",
                                 config = "noise_sens"),
               NA)
})

test_that("Test noise sens 2-box", {
  forc = c(0, numeric(20) + 10)
  vars = list(lambda = c(1, 0.1), weights = c(0.9, 0.1), Cap = 10.1, T0 = 0, F0 = 0)
  expect_error(noise_sens_single_it(forc, vars,
                                    sd_white = 0.5, sd_ar1 = 0.1,
                                    config_file = "../../ebm_fit_config.yml",
                                    config = "noise_sens"),
               NA)

  expect_error(noise_sensitivity(sd_white_list = c(0.5, 1),
                                 sd_ar1_list = 0,
                                 reps = 2,
                                 forc = forc,
                                 vars = vars,
                                 config_file = "../../ebm_fit_config.yml",
                                 config = "noise_sens"),
               NA)

})
