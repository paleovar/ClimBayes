## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)
library(ClimBayes)

## -----------------------------------------------------------------------------
forc = c(0, numeric(99) + 10)
vars = list(lambda = 0.1, weights = 1, Cap = 10.1, T0 = 0, F0 = 0)
res_list <- noise_sens_single_it(forc, vars,
           sd_white = 0.5, sd_ar1 = 0.1,
           config_file = system.file('extdata/ebm_fit_config.yml', 
                                     package = 'ClimBayes'),
           config = "noise_sens")
print(res_list)
plot(res_list)

## ---- eval = FALSE------------------------------------------------------------
#  results <- noise_sensitivity(sd_white_list = c(0.5, 1),
#                               sd_ar1_list = 0,
#                               reps = 20,
#                               forc = forc,
#                               vars = vars,
#                               config = "noise_sens")

## ---- include = FALSE---------------------------------------------------------
# save(results, file = "data/results_noise_sensitivity.rda")
load("data/results_noise_sensitivity.rda")

## -----------------------------------------------------------------------------
print(results)

## -----------------------------------------------------------------------------
  ggplot2::ggplot(results, ggplot2::aes(x = delta_lambda1, 
                                        fill = as.factor(sd_white),
                                        col = as.factor(sd_white))) +
  ggplot2::geom_histogram(bins = 10, position = "identity", alpha = 0.5) +
  ggplot2::theme_bw() +
  ggplot2::labs(fill = "SD of white noise") + 
  ggplot2::guides(col = "none")

