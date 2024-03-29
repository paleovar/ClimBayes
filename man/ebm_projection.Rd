% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ebm_projections.R
\name{ebm_projection}
\alias{ebm_projection}
\title{Generate temperature projections from EBM fit and additional forcing series.}
\usage{
ebm_projection(
  res_list,
  forc,
  start_year = NULL,
  end_year = NULL,
  cred_int = FALSE
)
}
\arguments{
\item{res_list}{Object of class \code{ebm_fit_result}}

\item{forc}{Forcing for which the projections should be computed. Vector of forcing values or data frame with columns \code{year} and \code{forcing}.}

\item{start_year}{First of forcing considered (only relevant if \code{forc} is a data frame).}

\item{end_year}{Last year of forcing considered (inclusive; only relevant if \code{forc} is a data frame).}

\item{cred_int}{Boolean -
\itemize{
\item If TRUE, credible intervals are computed also (much slower than without!)
\item If FALSE, only the posterior means of the parameters are extracted and EBM projection is computed with this.
}}
}
\value{
Object of class \code{ebm_proj_result}, inherits from \code{ebm_fit_result}.
In addition to the entries of \code{ebm_fit_result} (see documentation there), it contains an entry:
\itemize{
\item \code{projection}
\itemize{
\item \code{input}: list with entry \code{forc_vals}
\item \code{result}: list with \verb{median, mean, lower_quant, upper_quant}
}
}
}
\description{
Takes result of \code{ebm_fit} and uses the posterior parameters to project the EBM
further into the future.
}
