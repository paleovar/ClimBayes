% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neg_log_post.R
\name{neg_log_post}
\alias{neg_log_post}
\title{Negative logarithm of posterior density (up to normalization constant)}
\usage{
neg_log_post(X, params, sol = NULL)
}
\arguments{
\item{X}{vector of parameter values.}

\item{params}{List of parameters;
relevant entries: those relevant for \code{log_likelihood}
and those for \code{prior}.}

\item{sol}{(optional) Vector of EBM solution for this value of X.
This can be used to speed up the code and avoid repeated calculations.}
}
\value{
Value of the negative log of posterior density, up to normalization constant.
}
\description{
The posterior is proportional to likelihood * prior by Bayes theorem,
the normalization by the marginal is not included.
}
