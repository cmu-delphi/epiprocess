% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/key_colnames.R
\name{key_colnames}
\alias{key_colnames}
\alias{key_colnames.default}
\alias{key_colnames.data.frame}
\alias{key_colnames.epi_df}
\alias{key_colnames.epi_archive}
\title{Grab any keys associated to an epi_df}
\usage{
key_colnames(x, ...)

\method{key_colnames}{default}(x, ...)

\method{key_colnames}{data.frame}(x, other_keys = character(0L), exclude = character(0L), ...)

\method{key_colnames}{epi_df}(x, exclude = character(0L), ...)

\method{key_colnames}{epi_archive}(x, exclude = character(0L), ...)
}
\arguments{
\item{x}{a data.frame, tibble, or epi_df}

\item{...}{additional arguments passed on to methods}

\item{other_keys}{an optional character vector of other keys to include}

\item{exclude}{an optional character vector of keys to exclude}
}
\value{
If an \code{epi_df}, this returns all "keys". Otherwise \code{NULL}.
}
\description{
Grab any keys associated to an epi_df
}
\keyword{internal}
