#' Plot function for class `ebm_proj_result`
#'
#' @param res_list Object of class `ebm_proj_result` (result of `ebm_projection`).
#' @param line_size Line size in plot.
#' @param text_size Text size in plot.
#' @param cred_int Boolean to decide if credible intervals should be plotted.
#' @param plot_forcing Boolean to decide if forcing should be plotted below.
#' @return ggplot
#' @export
plot.ebm_proj_result <- function(res_list,
                                line_size = 0.8,
                                text_size = 10,
                                cred_int = TRUE,
                                plot_forcing = FALSE) {
  temp_vals = res_list$input_params$y_obs
  start_year = res_list$meta$start_year
  n_years = res_list$input_params$n_years
  n_years_total = length(res_list$projection$result$mean)

  if("lower_quant" %in% names(res_list$projection$result)) {
    results_model_fit <- tibble::tibble(grid = 0:(n_years_total - 1),
                                        temp = res_list$projection$result$mean,
                                        temp_lower = res_list$projection$result$lower_quant,
                                        temp_upper = res_list$projection$result$upper_quant,
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
  } else {
    results_model_fit <- tibble::tibble(grid = 0:(n_years_total - 1),
                                        temp = res_list$projection$result$mean,
                                        type = "Model est.")
    obs_grid_step = res_list$input_params$obs_grid_step
    if (is.null(obs_grid_step)) {
      obs_data <- tibble::tibble(grid = 0:(n_years-1), temp = temp_vals)
    } else {
      obs_data <- tibble::tibble(grid = seq(0, n_years-1, obs_grid_step),
                                 temp = temp_vals)
    }

    obs_data['type'] <- "Observations"

    obs_data %>%
      dplyr::mutate(time_years = grid + start_year) ->
      obs_data

    results_model_fit %>%
      dplyr::mutate(time_years = grid + start_year) %>%
      dplyr::select(grid, time_years, temp, type) ->
      results_mean
    results <- dplyr::bind_rows(obs_data, results_mean)
  }

  results$type <- factor(results$type, levels = c("Observations", "Model est."))
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

  if (cred_int & "lower_quant" %in% names(res_list$projection$result)) {
    gg <- gg +
      #scale_linetype_manual(values = ltys) +
      ggplot2::geom_ribbon(data = dplyr::filter(results, type == "Model est."),
                           ggplot2::aes(x = time_years, ymax = temp_upper, ymin = temp_lower,
                                        fill = "Model est."),
                           linetype = 0, alpha=.3) +
      ggplot2::scale_fill_manual(name="Confidence band", values= c("Model est." = "firebrick"))
  }
  if(plot_forcing) {
    forc_df <- tibble::tibble(time_years = 0:(n_years_total - 1) + start_year,
                              forcing = res_list$projection$input$forc_vals)
    gg_forc <- ggplot2::ggplot(forc_df) +
      ggplot2::geom_line(ggplot2::aes(x = time_years, y = forcing, col = "forcing"),
                         size = line_size, alpha = 1) +
      ggplot2::scale_color_manual(values=c("forestgreen"), name=NULL)+
      ggplot2::labs(x = "Year",
                    y = expression(paste("Forcing [",plain("Wm"^"-2"),"]"))) +
      ggplot2::theme_bw() +
      ggplot2::theme(text = ggplot2::element_text(size = text_size),
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

   # joint <- cowplot::plot_grid(gg_no_axis,
  #                              gg_forc + ggplot2::theme(legend.position = "none"),
  #                              align = "v", axis = "b", ncol = 1,
  #                              rel_heights = c(0.6, 0.4))
    #joint <- plot_grid(joint, legend_model, nrow = 2, rel_heights = c(0.9, 0.15))
    return(joint)
  }

  return(gg)
}
