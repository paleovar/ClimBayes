# --------------- Helper for detrending -----------
#' @title Linear detrending
#' @param y input vector
#' taken from Ellerhoff and Rehfeld (2022) https://github.com/paleovar/StateDependency
#' @return detrended output vector
#' @noRd
detrend <- function(y){
  fit <- lm(y ~ zoo::index(y))
  return(y - zoo::index(y)*fit$coefficients[2] - fit$coefficients[1])
}
