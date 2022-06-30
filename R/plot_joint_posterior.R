#' Plot function for joint posteriors
#'
#' Plot of joint posterior of two variables.
#'
#' @param res_list Result of fit.
#' @param var1 Name of first variable, e.g. `lambda1`
#' @param var2 Name of second variable, e.g. `lambda2`
#' @param xlim Vector for limits of x axis. If `NA`, will be determined automatically,
#' or via `prior_bounds_as_lim` (see below).
#' @param ylim Vector for limits of y axis
#' @param prior_bounds_as_lim If TRUE, the bounds of prior intervals will be taken as
#' the limits of the plot axes. If FALSE and also no `xlim` / `ylim` specified,
#' then the limits are determined automatically.
#' @param text_size Text size in plot.
#' @param colourbar_breaks Breaks for `scale_fill_distiller`.
#' @export
#' @examples
#' plot_joint_posterior(res_list, "lambda1", "lambda2")
plot_joint_posterior <- function(res_list,
                                 var1 = "lambda1", var2 = "lambda2",
                                 xlim = NA, ylim = NA,
                                 prior_bounds_as_lim = FALSE,
                                 text_size = 10, colourbar_breaks = NA) {

  # need the correct X_names
  samples <- res_list$samples$parameters
  burn_in <- res_list$input_params$burn_in
  n_chains <- res_list$input_params$n_chains
  X_names <- res_list$input_params$X_names

  var1_lb <- as.numeric(res_list$input_params[[paste0(var1, '_lb')]])
  var1_ub <- as.numeric(res_list$input_params[[paste0(var1, '_ub')]])

  var2_lb <- as.numeric(res_list$input_params[[paste0(var2, '_lb')]])
  var2_ub <- as.numeric(res_list$input_params[[paste0(var2, '_ub')]])

  colnames(samples) <- X_names
  samples <- tibble::as_tibble(samples)
  samples %>% dplyr::filter(dplyr::row_number() > burn_in * n_chains) ->
    filtered_samples

  labs_dict <- create_labs_dict(X_names)

  gg <- ggplot2::ggplot(filtered_samples,
                  ggplot2::aes_string(x = var1,
                                      y = var2)) +
    ggplot2::labs(x = labs_dict[[var1]], y = labs_dict[[var2]])
  #scale_x_continuous(expand = c(0, 0)) +
  #lims(x = c(var1_lb, var1_ub), y = c(var2_lb, var2_ub)) -> gg

  if(!any(is.na(xlim))) {
    gg <- gg + ggplot2::lims(x = xlim)
  }
  if(!any(is.na(ylim))) {
    gg <- gg + ggplot2::lims(y = ylim)
  }
  if(prior_bounds_as_lim) {
    gg <- gg + ggplot2::lims(x = c(var1_lb, var1_ub), y = c(var2_lb, var2_ub))
  }

  gg + ggplot2::stat_density_2d(ggplot2::aes(fill = stat(density)),
                                geom = 'raster',
                       contour = FALSE, adjust = 2) +
    ggplot2::scale_fill_distiller(palette = "PuRd", trans = "reverse") +
    ggplot2::geom_point(size = 0.1, alpha = 0, col = "white") +
    #scale_fill_viridis_c() +
    #geom_bin2d() +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size = text_size),
                   legend.position= "bottom") +
    ggplot2::guides(fill = ggplot2::guide_colourbar(label.position = "top")) -> gg

  if(!any(is.na(colourbar_breaks))) {
    gg <- gg + ggplot2::scale_fill_distiller(palette = "PuRd", trans = "reverse",
                                    breaks=colourbar_breaks)
  }

  gg + ggplot2::theme(legend.position = "none") -> gg2
  ggExtra::ggMarginal(gg, type="histogram", size = text_size, fill = "firebrick") -> ggjoint
  # ggMarginal(gg2, type="histogram", size = text_size, fill = "firebrick") -> ggjoint2
  return(ggjoint)
}
