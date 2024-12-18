% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{generate_response_poisson}
\alias{generate_response_poisson}
\title{Generate Poisson Response}
\usage{
generate_response_poisson(eta, link = "log")
}
\arguments{
\item{eta}{Numeric vector of linear predictors.}

\item{link}{Character string specifying the link function.}
}
\value{
Numeric vector of response values.
}
\description{
Generates the response variable for a Poisson model with a specified link function.
}
