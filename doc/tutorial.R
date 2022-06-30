## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(ClimBayes)

## ----gen-data-----------------------------------------------------------------
forcing <- c(0, rep(3, 50))
lambda <- 0.3
sd_white <- 0.05
# calculate solution for 1-bx model with given parameters
sol <- solve_ebm(lambda, weights = 1, T0 = 0, F0 = 0, Cap = 10.1, 
                 forc_vals = forcing) 
noise <- rnorm(51, 0, sd_white)

obs <- sol + noise
plot(obs, type = "l", xlab="Time (years)", ylab="Temperature (K)", 
     main="synthetic example: step function with noise")
lines(sol, col = "darkgreen")
legend(x = "bottomright",          # Position
       legend = c("EBM sol.", "EBM sol. with noise"),  
       col = c("forestgreen", "black"),
       lwd = 1)
 

## ----fit-synth----------------------------------------------------------------
fit <- ebm_fit(obs, forcing, 
               n_boxes = 1, 
               detrending = FALSE,
               config_file = system.file('extdata/ebm_fit_config.yml', package = 'ClimBayes'), 
               config = "experimental")

## -----------------------------------------------------------------------------
print(fit)

## -----------------------------------------------------------------------------
plot(fit)

## -----------------------------------------------------------------------------
plot_fit(fit, ebm_sol = sol)

## -----------------------------------------------------------------------------
plot_fit(fit, plot_forcing=T)

## -----------------------------------------------------------------------------
print(fit$posteriors$parameters$lambda1)

## ----hadcrut-example----------------------------------------------------------
data("hadcrut")
data("schmidt")
fit_1box <- ebm_fit(hadcrut, schmidt, 
             n_boxes = 1,
             start_year = 1850, 
             end_year = 2000)
plot(fit_1box)

## ----hadcrut-ex2--------------------------------------------------------------
data("hadcrut")
data("schmidt")
fit_2box <- ebm_fit(hadcrut, schmidt, 
             n_boxes = 2,
             start_year = 1850, 
             end_year = 2000)
plot(fit_2box)
plot_two_fits(fit_1box, fit_2box)

## ----include=FALSE------------------------------------------------------------
temp_vals <- dplyr::filter(hadcrut, 1900 <= year, year <= 2000)$temperature
forc_vals <- dplyr::filter(schmidt, 1900 <= year, year <= 2000)$forcing

## ----data-save----------------------------------------------------------------
temperature_data <- data.frame(year = 1900:2000, temperature = temp_vals)
save(temperature_data, file = "../data/temperature_data.rda")
forcing_data <- data.frame(year = 1900:2000, forcing = forc_vals)
save(forcing_data, file = "../data/forcing_data.rda")

## -----------------------------------------------------------------------------
load("../data/temperature_data.rda")
load("../data/forcing_data.rda")
ebm_fit(temperature_data, forcing_data, 1)

## ---- include=FALSE-----------------------------------------------------------
file.remove("../data/temperature_data.rda")
file.remove("../data/forcing_data.rda")

## -----------------------------------------------------------------------------
ebm_fit(temperature_data, forcing_data, 1,
        start_year = 1950,
        end_year = 1970)

## ---- eval = FALSE------------------------------------------------------------
#  ebm_fit(temp_vals, forc_vals, 1)

## ----intvar-------------------------------------------------------------------
library(tidyr)
library(tibble)
library(ggplot2)

#generate samples of internal variability
noise2 <- ClimBayes::gen_noise_from_ebm_fit(3, fit_2box)

#prepare data table for plotting
noise_df2 <- as.data.frame(t(noise2))
noise_df2 <- as.data.frame(apply(noise_df2, 2, 
                          function(x) x + fit_2box$posteriors$model_fit$mean))
noise_df2$year <- 1850:2000
tidyr::pivot_longer(noise_df2, cols = starts_with("V"), 
                    names_to = "realisation", values_to = "y") ->
  noise_df2
fit_df = data.frame(year = 1850:2000, y = fit_2box$posteriors$model_fit$mean) %>%
  tibble::add_column(realisation="2-box")
sol_df = data.frame(year = 1850:2000, y = fit_2box$input_params$y_obs) %>%
  tibble::add_column(realisation="Observation")
my_colors <- RColorBrewer::brewer.pal(6, "Purples")[4:6]

#plot
ggplot2::ggplot(rbind(noise_df2, sol_df, fit_df)) +
  geom_line(aes(x = year, y = y, col=realisation)) +
  labs(x = "Time (yr CE)", y = "Temperature anomaly (K)") +
  scale_x_continuous(expand=c(0,0)) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_manual(values=c("Observation"="darkgrey",
                              "2-box"="firebrick",
                              "V1"=my_colors[1],
                              "V2"=my_colors[2],
                              "V3"=my_colors[3]),
                     breaks=c("Observation","2-box","V1","V2","V3"),
                     labels = c("HadCRUT5", "forced", "forced+internal (E1)", 
                              "forced+internal (E2)", "forced+internal (E3)")) +
  theme(legend.position=c(0.25,0.8),
        legend.box.background =  element_rect(colour = "black"),
        legend.title= element_blank(),
        legend.margin = ggplot2::margin(t=-0.02,l=0.05,b=0.05,r=0.1, unit='cm')) + 
  theme(plot.margin = margin(0,0.5,0,0, "cm"))

## ----proj---------------------------------------------------------------------
forc_proj = tibble::tibble(year = 2001:2020, forcing = 1:20 / 5)
proj1 <- ebm_projection(fit_1box, forc_proj, cred_int = TRUE)
plot(proj1, plot_forcing=F)

