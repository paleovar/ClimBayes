% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prior.R
\name{prior}
\alias{prior}
\title{Prior density}
\usage{
prior(X, params)
}
\arguments{
\item{X}{Vector of parameter values.}

\item{params}{List of parameters.
Relevant parameters:
\itemize{
\item \code{prior_type}
\item lower and upper bounds for prior intervals of parameters
(e.g. \code{lambda1_lb} and \code{lambda1_ub} define bounds for parameter \code{lambda1})
\item if \code{type == "beta"}:  \code{prior_beta_shape1} and \code{prior_beta_shape2}
are the shape parameters for the beta distribution (either of length 1 to specify
one shape for all parameters or of length equal to \code{length(X)},
such that each parameter has a differently shaped prior)
}}
}
\value{
Value of the prior density.
}
\description{
Computes the prior density for the parameter values in \code{X},
assuming priors for individual parameters are independent of each other.
}
