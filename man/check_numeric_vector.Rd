% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validation.R
\name{check_numeric_vector}
\alias{check_numeric_vector}
\title{Check if a vector is numeric}
\usage{
check_numeric_vector(x, arg = rlang::caller_arg(x), call = rlang::caller_env())
}
\arguments{
\item{x}{A vector.}

\item{arg}{The argument name.}

\item{call}{The calling environment.}
}
\value{
Throws an error if the vector is not numeric.
}
\description{
Check if a vector is numeric
}
