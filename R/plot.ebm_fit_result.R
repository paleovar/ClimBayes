#' Plot function for marginal posteriors of all estimated parameters.
#'
#' @param res_list Object of class `ebm_fit_result`.
#' @param text_size Text size in plot.
#' @param line_size Line size in plot.
#' @param return_all Boolean to choose if the plots of the marginal posteriors of
#' single parameters should be returned separately in a list.
#' @return Grid of marginal posteriors (result from `cowplot::plot_grid`) if
#' `return_all = FALSE`. Otherwise a list with entries
#' - `gg`: Grid of marginal posteriors
#' - `plot_list`: Plot list of single plots for each parameter.
#' @export
plot_marginals <- function(res_list,
                          text_size = 10,
                          line_size = 0.5,
                          return_all = FALSE,
                          ...) {

  samples <- res_list$samples$parameters
  burn_in <- res_list$input_params$burn_in
  n_chains <- res_list$input_params$n_chains
  X_names <- res_list$input_params$X_names
  d = length(X_names)
  labs_dict <- create_labs_dict(X_names)

  plot_list <- list()
  for (j in 1:d) {
    variable_index = j
    variable_name = X_names[j]
    n_total <- nrow(samples)
    sample_vec <- samples[(burn_in * n_chains + 1):n_total, variable_index]
    samples_df <- tibble::tibble(samples = sample_vec)

    lb <- as.numeric(res_list$input_params[[paste0(variable_name, '_lb')]])
    ub <- as.numeric(res_list$input_params[[paste0(variable_name, '_ub')]])

    prior_type <- as.character(res_list$input_params[["prior_type"]])
    xseq <- seq(lb, ub, 1e-5)

    if(prior_type == "beta") {
      prior_beta_shape1 <- as.numeric(res_list$input_params[["prior_beta_shape1"]])
      prior_beta_shape2 <- as.numeric(res_list$input_params[["prior_beta_shape2"]])
      prior_vals <- sapply(xseq, function(x)
        dbeta((x - lb)  / (ub - lb) ,prior_beta_shape1, prior_beta_shape2) /
          (ub-lb) * (lb <= x) * (ub >= x))
    } else if(prior_type == "unif") {
      prior_vals <- numeric(length(xseq)) + 1 / (ub - lb)
    }

    prior_df <- tibble::tibble(x = xseq, prior_density = prior_vals)

    cols = c("Prior" = "dodgerblue3", "Posterior" = "firebrick")
    plot_list[[j]] <- ggplot2::ggplot(samples_df, ggplot2::aes(x = samples)) +
      ggplot2::labs(x = labs_dict[[variable_name]],
           y = "Density") +
     ggplot2::geom_line(ggplot2::aes(col = "Posterior"), stat="density",
                            size = line_size, adjust = 3) +
      ggplot2::geom_line(data = prior_df,
                         ggplot2::aes(x = x, y = prior_density, col = "Prior"),
                         size = line_size) +
      ggplot2::scale_color_manual(values = cols, name = NULL) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
      ggplot2::theme_bw() +
      ggplot2::theme(text = ggplot2::element_text(size = text_size),
                     legend.position = "bottom")
  }
  # extract the legend from one of the plots
  legend_b <- cowplot::get_legend(plot_list[[1]] +
                                    ggplot2::theme(legend.position="bottom"))
  # remove legends from all of them
  plot_list_nolegend <- list()
  for (j in 1:d) {
    plot_list_nolegend[[j]] <- plot_list[[j]] +
      ggplot2::theme(legend.position = "none")
  }

  joint <- cowplot::plot_grid(plotlist = plot_list_nolegend)

  title <- cowplot::ggdraw() +
    cowplot::draw_label("Probability densities", size = text_size + 2) +
    ggplot2::theme_void()

  gg <- cowplot::plot_grid(title, joint, legend_b, ncol = 1,
                           rel_heights=c(0.1, 1, 0.1))
  if(return_all) {
    return(list(gg = gg,
                plot_list = plot_list))
  }
  return(gg)
}



#' Function to plot observations and model fit from `ebm_fit`.
#'
#' @param res_list Object of class `ebm_fit_result`.
#' @param cred_int Boolean to decide if credible intervals should be plotted.
#' @param ebm_sol Vector of additional temperature values,
#' e.g. the true non-noisy EBM solution in case the observations are created synthetically
#' as solution + noise (see also the `tutorial`-vignette).
#' @param text_size Text size in plot.
#' @param line_size Line size in plot.
#' @param plot_forcing Boolean to decide if forcing should be plotted below.
#' @return A ggplot if `plot_forcing = FALSE` and the output of `cowplot::plot_grid`
#' if `plot_forcing = TRUE`.
#' @export
plot_fit <- function(res_list,
                     cred_int = TRUE,
                     ebm_sol = NULL,
                     text_size = 10,
                     line_size = 0.5,
                     plot_forcing = FALSE,
                     ...
                    ) {
  temp_vals = res_list$input_params$y_obs
  start_year = res_list$meta$start_year
  n_years = res_list$input_params$n_years

  results_model_fit <- tibble::tibble(grid = 0:(n_years - 1),
                                 temp = res_list$posteriors$model_fit$median,
                                 temp_lower = res_list$posteriors$model_fit$lower_quant,
                                 temp_upper = res_list$posteriors$model_fit$upper_quant,
                                 type = "Model est.")

  obs_grid_step = res_list$input_params$obs_grid_step
  if (is.null(obs_grid_step)) {
    obs_data <- tibble::tibble(grid = 0:(n_years-1), temp = temp_vals)
  } else {
    obs_data <- tibble::tibble(grid = seq(0, n_years-1, obs_grid_step),
                               temp = temp_vals)
  }

  obs_data['type'] <- "Observations"
  obs_data['temp_upper'] <- temp_vals
  obs_data['temp_lower'] <- temp_vals

  obs_data %>%
    dplyr::mutate(time_years = grid + start_year) ->
    obs_data

  results_model_fit %>%
    dplyr::mutate(time_years = grid + start_year) %>%
    dplyr::select(grid, time_years, temp, type, temp_lower, temp_upper) ->
    results_mean
  results <- dplyr::bind_rows(obs_data, results_mean)

  results$type <- factor(results$type, levels = c("Observations", "Model est."))
  if(is.null(ebm_sol)) {
    cols <- c("Observations" = "darkgrey", "Model est." = "firebrick")
    ltys <- c("Model est." = "solid", "Observations" = "solid")
    gg <- ggplot2::ggplot(results, ggplot2::aes(x = time_years, y = temp, color = type)) +
      ggplot2::geom_line(size = line_size) +
      ggplot2::labs(x = "Year",
           y = "Temperature anomaly [K]") +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(name = "Type", values = cols) +
      ggplot2::theme(text = ggplot2::element_text(size=text_size)) +
      ggplot2::guides(color=ggplot2::guide_legend(override.aes=list(fill=NA)))
  } else {
    cols <- c("Observations" = "darkgrey", "True EBM sol." = "forestgreen",
              "Model est." = "firebrick")
    ltys <- c("Model est." = "solid", "True EBM sol." = "solid", "
              Observations" = "solid")
    my_labels <- c("Model est." = "Model fit", "True EBM sol." = "True EBM sol.", "
              Observations" = "EBM sol. F-data")
    gg <- ggplot2::ggplot(data = tibble::tibble(x = start_year:(start_year + n_years - 1),
                               y = ebm_sol)) +
      ggplot2::geom_line(ggplot2::aes(x = x, y = y, col = "True EBM sol."), size = line_size) +
      ggplot2::geom_line(data = results, ggplot2::aes(x = time_years, y = temp, color = type),
                size = line_size) +
      ggplot2::labs(x = "Year",
           y = "Temperature anomaly [K]") +
      ggplot2::scale_color_manual(name = "Type", values = cols, labels = my_labels) +
      ggplot2::theme_bw() +
      ggplot2::theme(text = ggplot2::element_text(size=text_size)) +
      ggplot2::guides(color=ggplot2::guide_legend(override.aes=list(fill=NA)))
  }

  if (cred_int) {
    gg <- gg +
      #scale_linetype_manual(values = ltys) +
      ggplot2::geom_ribbon(data = dplyr::filter(results, type == "Model est."),
                  ggplot2::aes(x = time_years, ymax = temp_upper, ymin = temp_lower,
                               fill = "Model est."),
                  linetype = 0, alpha=.3) +
      ggplot2::scale_fill_manual(name="Confidence band", values= c("Model est." = "firebrick")) +
      ggplot2::guides(color=ggplot2::guide_legend(override.aes=list(fill=NA)))
  }

  if(plot_forcing) {
    forc_df <- tibble::tibble(time_years = obs_data$time_years,
                              forcing = res_list$input_params$forc_vals)
    gg_forc <- ggplot2::ggplot(forc_df) +
      ggplot2::geom_line(ggplot2::aes(x = time_years, y = forcing, col="forcing"),
                size = line_size, alpha = 1) +
      ggplot2::scale_color_manual(values=c("forestgreen"), name=NULL)+
      ggplot2::labs(x = "Year",
           y = expression(paste("Forcing [",plain("Wm"^"-2"),"]"))) +
      ggplot2::theme_bw() +
      ggplot2::theme(text = ggplot2::element_text(size=text_size),
                     legend.justification = "top")

    gg_legend <- cowplot::get_legend(gg + ggplot2::theme(legend.justification = "bottom"))
    gg_forc_legend <- cowplot::get_legend(gg_forc)
    joint_l <- cowplot::plot_grid(gg_legend, gg_forc_legend, align="v", ncol=1, rel_heights = c(0.5,0.5))

    gg_no_axis <- gg + ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                                  axis.ticks.x = ggplot2::element_blank(),
                                  axis.title.x = ggplot2::element_blank(),
                                  legend.position = "none")
    joint_p <- cowplot::plot_grid(gg_no_axis,
                              gg_forc + ggplot2::theme(legend.position = "none"),
                              align = "v", axis = "b", ncol = 1,
                              rel_heights = c(0.6, 0.4))
    joint <- cowplot::plot_grid(joint_p,
                                joint_l,
                                ncol = 2,
                                rel_widths = c(0.8, 0.2))
    return(joint)
  }

  return(gg)
}

#' Plot function for class `ebm_fit_result`.
#'
#' @param res_list Object of class `ebm_fit_result`.
#' @param ... Additional parameters to be passed on to `plot_marginals` or `plot_fit`.
#' @return A list with entries
#' - `gg_fit`: plot of the observations and the model fit
#' - `gg_marginals`: plot of the marginal posteriors.
#' @export
plot.ebm_fit_result <- function(res_list, ...) {
  gg_fit <- plot_fit(res_list, ...)
  gg_marginals <- plot_marginals(res_list, ...)
  return(list(gg_fit = gg_fit, gg_marginals = gg_marginals))
}
