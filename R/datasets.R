# documentation of datasets following https://r-pkgs.org/data.html

#' Temperature observations from Hadcrut5
#'
#' A dataset containing the global annual mean surface temperature.
#'
#' @format A data frame with 172 rows and 2 variables:
#' \describe{
#'   \item{year}{from 1850-2022}
#'   \item{temperature}{temperature relative to 1961-1990}
#' }
#' @source \url{https://www.metoffice.gov.uk/hadobs/hadcrut5/}
"hadcrut"

#' Forcing data from Schmidt et. al.
#'
#' A dataset containing the global radiative forcing anomalies
#'
#' @format A data frame with 1151 rows and 2 variables:
#' \describe{
#'   \item{year}{from 850-2000}
#'   \item{forcing}{radiative forcing in W/m^2 relative to 850}
#' }
#' @source \url{https://doi.org/10.5194/gmd-5-185-2012}
"schmidt"
