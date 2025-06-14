% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/revision_analysis.R
\name{revision_analysis}
\alias{revision_analysis}
\alias{print.revision_analysis}
\alias{revision_summary}
\title{A function to describe revision behavior for an archive.}
\usage{
revision_analysis(
  epi_arch,
  ...,
  drop_nas = TRUE,
  min_waiting_period = as.difftime(60, units = "days"),
  within_latest = 0.2,
  compactify = TRUE,
  compactify_abs_tol = 0,
  return_only_tibble = FALSE
)

\method{print}{revision_analysis}(
  x,
  quick_revision = as.difftime(3, units = "days"),
  few_revisions = 3,
  abs_spread_threshold = NULL,
  rel_spread_threshold = 0.1,
  ...
)

revision_summary(
  epi_arch,
  ...,
  drop_nas = TRUE,
  min_waiting_period = as.difftime(60, units = "days"),
  within_latest = 0.2,
  compactify = TRUE,
  compactify_abs_tol = 0,
  return_only_tibble = FALSE
)
}
\arguments{
\item{epi_arch}{an epi_archive to be analyzed}

\item{...}{<\code{\link[dplyr:dplyr_tidy_select]{tidyselect}}>, used to choose the column to
summarize. If empty and there is only one value/measurement column (i.e.,
not in \code{\link{key_colnames}}) in the archive, it will automatically select it.
If supplied, \code{...} must select exactly one column.}

\item{drop_nas}{bool, drop any \code{NA} values from the archive? After dropping
\code{NA}'s compactify is run again if \code{compactify} is \code{TRUE} to make
sure there are no duplicate values from occasions when the signal is
revised to \code{NA}, and then back to its immediately-preceding value.}

\item{min_waiting_period}{\code{difftime}, integer or \code{NULL}. Sets a cutoff: any
time_values that have not had at least \code{min_waiting_period} to stabilize as
of the \code{versions_end} are removed. \code{min_waiting_period} should characterize
the typical time during which most significant revisions occur. The default
of 60 days corresponds to a typical near-final value for case counts as
reported in the context of insurance. To avoid this filtering, either set
to \code{NULL} or 0. A \code{difftime} will be rounded up to the appropriate \code{time_type} if
necessary (that is 5 days will be rounded to 1 week if the data is weekly).}

\item{within_latest}{double between 0 and 1. Determines the threshold
used for the \code{lag_to}}

\item{compactify}{bool. If \code{TRUE}, we will compactify after the signal
requested in \code{...} has been selected on its own and the \code{drop_nas} step.
This helps, for example, to give similar results when called on
\link[=epix_merge]{merged} and single-signal archives, since merged archives
record an update when any of the other signals change, not just the
requested signal. The default is \code{TRUE}.}

\item{compactify_abs_tol}{length-1 double, used if \code{compactify} is \code{TRUE}, it
determines the threshold for when two doubles are considered identical.}

\item{return_only_tibble}{boolean to return only the simple \code{tibble} of
computational results rather than the complete S3 object.}

\item{x}{a \code{revision_analysis} object}

\item{quick_revision}{Difftime or integer (integer is treated as days).
The amount of time between the final revision and the
actual time_value to consider the revision quickly resolved. Default of 3
days. This will be rounded up to the appropriate \code{time_type} if
necessary (that is 5 days will be rounded to 1 week if the data is weekly).}

\item{few_revisions}{Integer. The upper bound on the
number of revisions to consider "few". Default is 3.}

\item{abs_spread_threshold}{Scalar numeric. The
maximum spread used to characterize revisions which don't actually change
very much. Default is 5\% of the maximum value in the dataset, but this is
the most unit dependent of values, and likely needs to be chosen
appropriate for the scale of the dataset.}

\item{rel_spread_threshold}{Scalar between 0 and 1. The relative spread fraction used to characterize revisions which
don't actually change very much. Default is .1, or 10\% of the final value}
}
\value{
An S3 object with class \code{revision_behavior}. This function is typically
called for the purposes of inspecting the printed output. The
results of the computations are available in
\code{revision_analysis(...)$revision_behavior}. If you only want to access
the internal computations, use \code{return_only_tibble = TRUE}.
}
\description{
\code{revision_summary} removes all missing values (if requested), and then
computes some basic statistics about the revision behavior of an archive,
returning a tibble summarizing the revisions per time_value+epi_key
features. If \code{print_inform} is true, it prints a concise summary. The
columns returned are:
\enumerate{
\item \code{n_revisions}: the total number of revisions for that entry
\item \code{min_lag}: the minimum time to any value (if \code{drop_nas=FALSE}, this
includes \code{NA}'s)
\item \code{max_lag}: the amount of time until the final (new) version (same caveat
for \code{drop_nas=FALSE}, though it is far less likely to matter)
\item \code{min_value}: the minimum value across revisions
\item \code{max_value}: the maximum value across revisions
\item \code{median_value}: the median value across revisions
\item \code{spread}: the difference between the smallest and largest values (this
always excludes \code{NA} values)
\item \code{rel_spread}: \code{spread} divided by the largest value (so it will
always be less than 1). Note that this need not be the final value. It will
be \code{NA} whenever \code{spread} is 0.
\item \code{lag_near_latest}: the time taken for the revisions to settle to within
\code{within_latest} (default 20\%) of the final value and stay there. For
example, consider the series (0, 20, 99, 150, 102, 100); then
\code{lag_near_latest} is 5, since even though 99 is within 20\%, it is outside
the window afterwards at 150.
}
}
\details{
Applies to \code{epi_archive}s with \code{time_type}s of \code{"day"}, \code{"week"},
and \code{"yearmonth"}. It can also work with a \code{time_type} of \code{"integer"} if
the possible \code{time_values} are all consecutive integers; you will need to
manually specify the \code{min_waiting_period} and \code{quick_revision}, though.
Using a \code{time_type} of \code{"integer"} with week numbers like 202501 will
produce incorrect results for some calculations, since week numbering
contains jumps at year boundaries.
}
\examples{
revision_example <- revision_analysis(archive_cases_dv_subset, percent_cli)
revision_example$revision_behavior \%>\% arrange(desc(spread))

}
