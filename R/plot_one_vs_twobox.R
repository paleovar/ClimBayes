#' Plot function to compare one- with two-box
#'
#' @param res_list_1box Result of one-box fit.
#' @param res_list_2box Result of two-box fit.
#' @param cred_int Boolean to decide if credible intervals should be plotted.
#' @param print_RMSE Boolean to decide if root mean square error (RMSE) should be
#' plotted in a separate text box.
#' @param legend_x Legend position on x-axis (passed to the plot via `ggplot2::theme(legend.position = c(legend_x, legend_y))`.
#' @param legend_y Legend position on y-axis (passed to the plot via `ggplot2::theme(legend.position = c(legend_x, legend_y))`..
#' @param text_size Text size in plot.
#' @param line_size Line size in plot.
#' @param cols A named vector to specify colors for `Observations`, `1-box`, `2-box`.
#' @return A ggplot object.
#' @export
plot_one_vs_two_box <- function(res_list_1box, res_list_2box,
         cred_int = TRUE,
         print_RMSE = TRUE,
         legend_x = 0.15,
         legend_y = 0.85,
         text_size = 10,
         line_size = 0.5,
         cols = c("Observations" = "darkgrey", "1-box" = "firebrick", "2-box" = "#E95F0A")){

  if(!all(names(cols) %in% c("Observations", "1-box", "2-box"))){
    print("names of cols needs to be Observations, 1-box and 2-box")
  }
  temp_vals <- res_list_1box$input_params$y_obs
  temp_vals2 <- res_list_2box$input_params$y_obs
  if(!all(temp_vals == temp_vals2)) {
    stop("Error in plotting 1 vs. 2 box: Indices do not fit")
  }

  # -------------- set up observation data ------------------
  obs_grid_step <- res_list_1box$input_params$obs_grid_step
  start_year <- res_list_1box$meta$start_year
  n_years <- res_list_1box$input_params$n_years

  if (is.null(obs_grid_step)) {
    obs_data <- tibble::tibble(grid = 0:(n_years-1), temp = temp_vals)
  } else {
    obs_data <- tibble::tibble(grid = seq(0, n_years-1, obs_grid_step),
                               temp = temp_vals)
  }

  obs_data['type'] <- "Observations"
  obs_data['temp_upper'] <- temp_vals
  obs_data['temp_lower'] <- temp_vals

  results_1box <- tibble::tibble(grid = obs_data$grid,
                                 temp = res_list_1box$posteriors$model_fit$median,
                                 temp_lower = res_list_1box$posteriors$model_fit$lower_quant,
                                 temp_upper = res_list_1box$posteriors$model_fit$upper_quant,
                                 type = "1-box")
  results_2box <- tibble::tibble(grid = obs_data$grid,
                                 temp = res_list_2box$posteriors$model_fit$median,
                                 temp_lower = res_list_2box$posteriors$model_fit$lower_quant,
                                 temp_upper = res_list_2box$posteriors$model_fit$upper_quant,
                                 type = "2-box")

  # calculate RMSE
  RMSE_1box <- RMSE(temp_vals, results_1box$temp)
  RMSE_2box <- RMSE(temp_vals, results_2box$temp)

  results <- dplyr::bind_rows(obs_data, results_1box, results_2box)
  results %>%
    dplyr::mutate(time_years = grid + start_year) ->
    results

  results$type <- factor(results$type,
                         levels = c("Observations", "1-box", "2-box"))

  if(legend_y < 0) {
    legend_pos = "bottom"
  } else {
    legend_pos = c(legend_x, legend_y)
  }

  ltys <- c("1-box" = "solid", "2-box" = "solid", "Observations" = "solid")
  gg_data <- ggplot2::ggplot(results,
                             ggplot2::aes(x = time_years, y = temp -mean(temp), color = type)) +
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
      ggplot2::annotate("text", x=max(results$time_years)-0.2*(max(results$time_years)-min(results$time_years)), y=min(results$temp)+0.1*(max(results$temp)-min(results$temp)), label=paste0("RMSE (1-box): ",round(RMSE_1box, 3), " K"), col=cols[["1-box"]], size=text_size/3) +
      ggplot2::annotate("text", x=max(results$time_years)-0.2*(max(results$time_years)-min(results$time_years)), y=min(results$temp), label=paste0("RMSE (2-box): ",round(RMSE_2box, 3), " K"), col=cols[["2-box"]], size=text_size/3)
  }

  if(cred_int){
    gg_data <- gg_data +
      ggplot2::geom_ribbon(data = dplyr::filter(results, type == "1-box"),
                           ggplot2::aes(x = time_years,
                                        ymax = temp_upper-mean(temp), ymin = temp_lower-mean(temp), fill = "1-box"),
                           linetype = 0, alpha=.2) +
      ggplot2::geom_ribbon(data = dplyr::filter(results, type == "2-box"),
                           ggplot2::aes(x = time_years, ymax = temp_upper-mean(temp), ymin = temp_lower -mean(temp), fill = "2-box"),
                           linetype = 0, alpha=.2) +
      ggplot2::scale_fill_manual(name="Confidence band",
                                 values= c("1-box" = "firebrick", "2-box" =  "#E95F0A"),
                                 guide = "none")

    }

   return(gg_data)
  }


