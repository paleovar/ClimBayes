## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  # read in one config list
#  config_list <- yaml::read_yaml(system.file('extdata/ebm_fit_config.yml',
#                                             package = 'ClimBayes'))
#  # change a parameter in the "experimental" config directly
#  config_list$experimental$parameter_defaults$Cap <- 9
#  # run the ebm_fit with this config list
#  ebm_fit(obs, forc, 1, config_file = config_list, config = "experimental")
#  
#  # modify parameters and repeat
#  config_list$experimental$parameter_defaults$Cap <- 10
#  ebm_fit(obs, forc, 1, config_file = config_list, config = "experimental")
#  
#  config_list$experimental$parameter_defaults$Cap <- 11
#  ebm_fit(obs, forc, 1, config_file = config_list, config = "experimental")

