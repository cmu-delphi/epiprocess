% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-epi_archive.R
\name{epix_as_of}
\alias{epix_as_of}
\title{Generate a snapshot from an \code{epi_archive} object}
\usage{
epix_as_of(x, max_version, min_time_value = -Inf, all_versions = FALSE)
}
\arguments{
\item{x}{An \code{epi_archive} object}

\item{max_version}{Time value specifying the max version to permit in the
snapshot. That is, the snapshot will comprise the unique rows of the
current archive data that represent the most up-to-date signal values, as
of the specified \code{max_version} (and whose time values are at least
\code{min_time_value}.)}

\item{min_time_value}{Time value specifying the min time value to permit in
the snapshot. Default is \code{-Inf}, which effectively means that there is no
minimum considered.}

\item{all_versions}{If \code{all_versions = TRUE}, then the output will be in
\code{epi_archive} format, and contain rows in the specified \code{time_value} range
having \code{version <= max_version}. The resulting object will cover a
potentially narrower \code{version} and \code{time_value} range than \code{x}, depending
on user-provided arguments. Otherwise, there will be one row in the output
for the \code{max_version} of each \code{time_value}. Default is \code{FALSE}.}
}
\value{
An \code{epi_df} object.
}
\description{
Generates a snapshot in \code{epi_df} format from an \code{epi_archive} object, as of a
given version. See the \href{https://cmu-delphi.github.io/epiprocess/articles/archive.html}{archive vignette} for
examples.
}
\details{
This is simply a wrapper around the \code{as_of()} method of the
\code{epi_archive} class, so if \code{x} is an \code{epi_archive} object, then:

\if{html}{\out{<div class="sourceCode">}}\preformatted{epix_as_of(x, max_version = v)
}\if{html}{\out{</div>}}

is equivalent to:

\if{html}{\out{<div class="sourceCode">}}\preformatted{x$as_of(max_version = v)
}\if{html}{\out{</div>}}

Mutation and aliasing: \code{epix_as_of} and \verb{$as_of} will not mutate the input
archives, but may in some edge cases alias parts of the inputs, so copy the
outputs if needed before using mutating operations like \code{data.table}'s \verb{:=}
operator. Currently, the only situation where there is potentially aliasing
is of the \code{DT} in edge cases with \code{all_versions = TRUE}, but this may change
in the future.
}
\examples{
# warning message of data latency shown
epix_as_of(
  x = archive_cases_dv_subset,
  max_version = max(archive_cases_dv_subset$DT$version)
)


range(archive_cases_dv_subset$DT$version) # 2020-06-02 -- 2021-12-01

epix_as_of(
  x = archive_cases_dv_subset,
  max_version = as.Date("2020-06-12")
)

# When fetching a snapshot as of the latest version with update data in the
# archive, a warning is issued by default, as this update data might not yet
# be finalized (for example, if data versions are labeled with dates, these
# versions might be overwritten throughout the corresponding days with
# additional data or "hotfixes" of erroroneous data; when we build an archive
# based on database queries, the latest available update might still be
# subject to change, but previous versions should be finalized). We can
# muffle such warnings with the following pattern:
withCallingHandlers(
  {
    epix_as_of(
      x = archive_cases_dv_subset,
      max_version = max(archive_cases_dv_subset$DT$version)
    )
  },
  epiprocess__snapshot_as_of_clobberable_version = function(wrn) invokeRestart("muffleWarning")
)
# Since R 4.0, there is a `globalCallingHandlers` function that can be used
# to globally toggle these warnings.

}
