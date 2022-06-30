#' Plot function to compare two fits to the same observation data.
#'
#' @param res_list1 Result of first fit.
#' @param res_list2 Result of second fit.
#' @param label1 Label for first fit, e.g. `"1-box"`.
#' @param label2 Label for second fit, e.g. `"2-box"`.
#' @param cred_int Boolean to decide if credible intervals should be plotted.
#' @param print_RMSE Boolean to decide if root mean square error (RMSE) should be
#' plotted in a separate text box.
#' @param legend_x Legend position on x-axis (passed to the plot via `ggplot2::theme(legend.position = c(legend_x, legend_y))`.
#' @param legend_y Legend position on y-axis (passed to the plot via `ggplot2::theme(legend.position = c(legend_x, legend_y))`..
#' @param text_size Text size in plot.
#' @param line_size Line size in plot.
#' @param cols A vector to specify colors for observations, the first and the second fit
#' @param subtract_mean Boolean to specify if overall mean of all time series should be subtracted before plotting
#' @return A ggplot object.
#' @export
plot_two_fits <- function(res_list1, res_list2,
         label1 = "1-box",
         label2 = "2-box",
         cred_int = TRUE,
         print_RMSE = TRUE,
         legend_x = 0.15,
         legend_y = 0.85,
         text_size = 10,
         line_size = 0.5,
         cols = c("darkgrey", "firebrick", "#E95F0A"),
         subtract_mean = FALSE){

  temp_vals <- res_list1$input_params$y_obs
  temp_vals2 <- res_list2$input_params$y_obs
  if(!all(temp_vals == temp_vals2)) {
    stop("Error in plotting 1 vs. 2 box: Indices do not fit")
  }
  names(cols) = c("Observations", label1, label2)
  # -------------- set up observation data ------------------
  obs_grid_step <- res_list1$input_params$obs_grid_step
  start_year <- res_list1$meta$start_year
  n_years <- res_list1$input_params$n_years

  if (is.null(obs_grid_step)) {
    obs_data <- tibble::tibble(grid = 0:(n_years-1), temp = temp_vals)
  } else {
    obs_data <- tibble::tibble(grid = seq(0, n_years-1, obs_grid_step),
                               temp = temp_vals)
  }

  obs_data['type'] <- "Observations"
  obs_data['temp_upper'] <- temp_vals
  obs_data['temp_lower'] <- temp_vals

  results1 <- tibble::tibble(grid = obs_data$grid,
                                 temp = res_list1$posteriors$model_fit$median,
                                 temp_lower = res_list1$posteriors$model_fit$lower_quant,
                                 temp_upper = res_list1$posteriors$model_fit$upper_quant,
                                 type = label1)
  results2 <- tibble::tibble(grid = obs_data$grid,
                                 temp = res_list2$posteriors$model_fit$median,
                                 temp_lower = res_list2$posteriors$model_fit$lower_quant,
                                 temp_upper = res_list2$posteriors$model_fit$upper_quant,
                                 type = label2)

  # calculate RMSE
  RMSE_1box <- RMSE(temp_vals, results1$temp)
  RMSE_2box <- RMSE(temp_vals, results2$temp)

  results <- dplyr::bind_rows(obs_data, results1, results2)
  results %>%
    dplyr::mutate(time_years = grid + start_year) ->
    results

  if(subtract_mean) {
    mt = mean(results$temp)
    results %>%
      dplyr::mutate(temp = temp - mt,
                    temp_lower = temp_lower - mt,
                    temp_upper = temp_upper - mt) ->
      results
  }

  results$type <- factor(results$type,
                         levels = c("Observations", label1, label2))

  if(legend_y < 0) {
    legend_pos = "bottom"
  } else {
    legend_pos = c(legend_x, legend_y)
  }

  ltys <- c(label1 = "solid", label2 = "solid", "Observations" = "solid")
  gg_data <- ggplot2::ggplot(results,
                             ggplot2::aes(x = time_years, y = temp, color = type)) +
    ggplot2::geom_line(size = line_size, alpha = 0.8) +
    ggplot2::labs(x = "Year",
                  y = "Temperature anomaly [K]") +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size = text_size),
                   legend.position = legend_pos,
                   legend.title = ggplot2::element_blank(),
                   legend.margin = ggplot2::margin(t=-0.25,l=0.05,b=0.0,r=0.05, unit='cm')) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes=list(fill=NA)))

  if(print_RMSE){
    gg_data <- gg_data +
      ggplot2::annotate("text",
                        x=max(results$time_years)-0.2*(max(results$time_years)-min(results$time_years)),
                        y=min(results$temp)+0.1*(max(results$temp)-min(results$temp)),
                        label=paste0("RMSE (", label1, "): ", round(RMSE_1box, 3), " K"), col=cols[[label1]], size=text_size/3) +
      ggplot2::annotate("text",
                        x=max(results$time_years)-0.2*(max(results$time_years)-min(results$time_years)),
                        y=min(results$temp),
                        label=paste0("RMSE (", label2, "): ", round(RMSE_2box, 3), " K"), col=cols[[label2]], size=text_size/3)
  }

  if(cred_int){
    gg_data <- gg_data +
      ggplot2::geom_ribbon(data = dplyr::filter(results, type == label1),
                           ggplot2::aes(x = time_years,
                                        ymax = temp_upper, ymin = temp_lower,
                                        fill = label1),
                           linetype = 0, alpha=.2) +
      ggplot2::geom_ribbon(data = dplyr::filter(results, type == label2),
                           ggplot2::aes(x = time_years,
                                        ymax = temp_upper, ymin = temp_lower,
                                        fill = label2),
                           linetype = 0, alpha=.2) +
      ggplot2::scale_fill_manual(name="Confidence band",
                                 values= cols,
                                 guide = "none")

  }
  return(gg_data)
}
