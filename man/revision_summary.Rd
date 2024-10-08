% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/revision_analysis.R
\name{revision_summary}
\alias{revision_summary}
\title{A function to describe revision behavior for an archive}
\usage{
revision_summary(
  epi_arch,
  ...,
  drop_nas = TRUE,
  print_inform = TRUE,
  min_waiting_period = as.difftime(60, units = "days"),
  within_latest = 0.2,
  quick_revision = as.difftime(3, units = "days"),
  few_revisions = 3,
  abs_spread_threshold = NULL,
  rel_spread_threshold = 0.1,
  compactify_tol = .Machine$double.eps^0.5,
  should_compactify = TRUE
)
}
\arguments{
\item{epi_arch}{an epi_archive to be analyzed}

\item{...}{<\code{\link[=dplyr_tidy_select]{tidyselect}}>, used to choose the column to
summarize. If empty, it chooses the first. Currently only implemented for
one column at a time.}

\item{drop_nas}{bool, drop any \code{NA} values from the archive? After dropping
\code{NA}'s compactify is run again to make sure there are no duplicate values
from occasions when the signal is revised to \code{NA}, and then back to its
immediately-preceding value.}

\item{print_inform}{bool, determines whether to print summary information, or
only return the full summary tibble}

\item{min_waiting_period}{\code{difftime}, integer or \code{NULL}. Sets a cutoff: any
time_values not earlier than \code{min_waiting_period} before \code{versions_end} are
removed. \code{min_waiting_period} should characterize the typical time during
which revisions occur.  The default of 60 days corresponds to a typical
final value for case counts as reported in the context of insurance. To
avoid this filtering, either set to \code{NULL} or 0.}

\item{within_latest}{double between 0 and 1. Determines the threshold
used for the \code{time_to}}

\item{quick_revision}{difftime or integer (integer is treated as days), for
the printed summary, the amount of time between the final revision and the
actual time_value to consider the revision quickly resolved. Default of 3
days}

\item{few_revisions}{integer, for the printed summary, the upper bound on the
number of revisions to consider "few". Default is 3.}

\item{abs_spread_threshold}{numeric, for the printed summary, the maximum
spread used to characterize revisions which don't actually change very
much. Default is 5\% of the maximum value in the dataset, but this is the
most unit dependent of values, and likely needs to be chosen appropriate
for the scale of the dataset.}

\item{rel_spread_threshold}{float between 0 and 1, for the printed summary,
the relative spread fraction used to characterize revisions which don't
actually change very much. Default is .1, or 10\% of the final value}

\item{compactify_tol}{float, used if \code{drop_nas=TRUE}, it determines the
threshold for when two floats are considered identical.}

\item{should_compactify}{bool. Compactify if \code{TRUE}.}
}
\description{
\code{revision_summary} removes all missing values (if requested), and then
computes some basic statistics about the revision behavior of an archive,
returning a tibble summarizing the revisions per time_value+epi_key features. If \code{print_inform} is true, it
prints a concise summary. The columns returned are:
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
\item \code{time_near_latest}: This gives the lag when the value is within
\code{within_latest} (default 20\%) of the value at the latest time. For example,
consider the series (0,20, 99, 150, 102, 100); then \code{time_near_latest} is
the 5th index, since even though 99 is within 20\%, it is outside the window
afterwards at 150.
}
}
\examples{

revision_example <- revision_summary(archive_cases_dv_subset, percent_cli)

revision_example \%>\% arrange(desc(spread))
}
