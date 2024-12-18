% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{sample_mvnorm}
\alias{sample_mvnorm}
\title{Sample from Multivariate Normal Distribution}
\usage{
sample_mvnorm(n, cov_matrix, mean = NULL)
}
\arguments{
\item{n}{Integer number of samples.}

\item{cov_matrix}{Numeric covariance matrix.}

\item{mean}{Numeric vector of means.}
}
\value{
Numeric matrix of samples.
}
\description{
Generates samples from a multivariate normal distribution.
}
