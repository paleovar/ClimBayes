#' Helper function to extract vector of observation data
#'
#' Called internally by `ebm_fit`.
#'
#' @param obs Data frame or vector, as the input for `ebm_fit`.
#' @param start_year First year to be considered.
#' @param end_year Last year to be considered (inclusive).
#' @return Vector of observations from `start_year` to `end_year`.
#' @noRd
obs_vec_from_input <- function(obs, start_year, end_year) {
  if(is.data.frame(obs)) {
    if(!("temperature" %in% colnames(obs))) {
      stop("In ebm_fit: Data frame for 'obs' must contain a column 'temperature'.")
    }
    if(is.null(start_year) | is.null(end_year)) {
      message("In ebm_fit: No years  for 'obs' supplied. Use entire time series.")
      obs_vec <- obs$temperature
    } else {
      if(!("year" %in% colnames(obs))) {
        stop("In ebm_fit: Data frame for 'obs' must contain a column 'year'.")
      }
      obs_vec <- dplyr::filter(obs,
                               year >= start_year, year <= end_year)$temperature
    }
  } else if(is.numeric(obs) & is.vector(obs)) {
    obs_vec <- obs
  } else {
    stop("obs must be a suitable data frame or a numeric vector.")
  }
  return(obs_vec)
}

#' Helper function to extract vector of forcing data
#'
#' Called internally by `ebm_fit`.
#'
#' @param forc Data frame or vector, as the input for `ebm_fit`.
#' @param start_year First year to be considered.
#' @param end_year Last year to be considered (inclusive).
#' @return Vector of forcing values from `start_year` to `end_year`.
#' @noRd
forc_vec_from_input <- function(forc, start_year, end_year) {

  if(is.data.frame(forc)) {
    if(!("forcing" %in% colnames(forc))) {
      stop("Data frame for 'forc' must contain a column 'forcing'.")
    }
    if(is.null(start_year) | is.null(end_year)) {
      message("No years for 'forc' supplied. Use entire time series.")
      forc_vec <- forc$forcing
    } else {
      if(!("year" %in% colnames(forc))) {
        stop("Data frame for 'forc' must contain a column 'year'.")
      }
      forc_vec <- dplyr::filter(forc,
                                year >= start_year, year <= end_year)$forcing
    }
  } else if(is.numeric(forc) & is.vector(forc)) {
    forc_vec <- forc
  } else {
    stop("forc must be a suitable data frame or a numeric vector.")
  }
  return(forc_vec)
}
