% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcmc_helpers.R
\name{adaptive}
\alias{adaptive}
\title{Variance adaptation}
\usage{
adaptive(attempts, acceptance, sd.p, target = 0.234)
}
\arguments{
\item{attempts}{integer indicating number of attempts}

\item{acceptance}{integer giving the number of moves accepted by the algorithm}

\item{sd.p}{standard deviation of proposal}

\item{target}{target acceptance rate}
}
\value{
a list with components \code{sd}, \code{acc}, \code{att}
}
\description{
Adapt standard deviation of a proposal for a Markov chain Monte Carlo algorithm, based on the acceptance rate.
The function is targeting an acceptance probability of \code{target} for univariate Metropolis--Hastings proposals, which is
optimal for certain Gaussian regimes. If the number of attempts is large enough and the acceptance rate is not close
to the target, the standard deviation of the proposal will be increased or reduced and the number acceptance and attempts
reinitialized. If no change is made, the components \code{acc} will be the same as acceptance and similarly for attempts.
}
