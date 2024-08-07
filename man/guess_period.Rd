% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{guess_period}
\alias{guess_period}
\title{Use max valid period as guess for \code{period} of \code{time_values}}
\usage{
guess_period(
  time_values,
  time_values_arg = rlang::caller_arg(time_values),
  ...
)
}
\arguments{
\item{time_values}{Vector containing time-interval-like or time-point-like
data, with at least two distinct values.}

\item{time_values_arg}{Optional, string; name to give \code{time_values} in error
messages. Defaults to quoting the expression the caller fed into the
\code{time_values} argument.}

\item{...}{Should be empty, there to satisfy the S3 generic.}
}
\value{
length-1 vector; \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}} class will
either be the same class as \code{\link[base:diff]{base::diff()}} on such time values, an
integer, or a double, such that all \code{time_values} can be exactly obtained
by adding \code{k * result} for an integer k, and such that there is no smaller
\code{result} that can achieve this.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
