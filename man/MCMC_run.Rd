% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MCMC_run.R
\name{MCMC_run}
\alias{MCMC_run}
\title{Bayesian inference via MCMC approach}
\usage{
MCMC_run(params, diagnostics = TRUE)
}
\arguments{
\item{params}{Full parameter list.}

\item{diagnostics}{Boolean - if TRUE, criteria for performance diagnostics are
calculated and returned.}
}
\value{
Object of class \code{mcmc_result}, a nested list with the following entries:
\itemize{
\item \code{input_params}: Input parameters (copy of \code{params})
\item \code{posteriors}: List that contains posterior means, medians and variance for each estimated parameter
\item \code{samples}: List with entries
\itemize{
\item \code{parameters}
(matrix of samples,  one row for each sample, one column for each variable)
\item \code{model_fit} (matrix of EBM solutions (for all samples (potentially large!),
one row for each sample, one column for each point in time).
Note the ordering: first the burn-in samples from all chains, then the rest.
}
\item \code{diagnostics}: (only if diagnostics is TRUE) list with diagnostic criteria
\item \code{results_first_it}: (only if the noise is set in an iterative scheme)
some results of the first iteration
}
}
\description{
Computes one or several Markov Chains with a Metropolis Hastings algorithm
}
