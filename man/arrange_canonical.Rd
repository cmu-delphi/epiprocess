% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-epi_df.R
\name{arrange_canonical}
\alias{arrange_canonical}
\title{Arrange an epi_df into a standard order}
\usage{
arrange_canonical(x, ...)
}
\arguments{
\item{x}{an \code{epi_df}. Other objects will produce a warning and return as is.}

\item{...}{not used}
}
\description{
Moves \code{\link[=key_colnames]{key_colnames()}} to the left, then arranges rows based on that
ordering. This function is mainly for use in tests and so that
other function output will be in predictable order, where necessary.
}
\keyword{internal}
