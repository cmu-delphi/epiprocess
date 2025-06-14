% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slide.R
\name{epi_slide_opt}
\alias{epi_slide_opt}
\alias{epi_slide_mean}
\alias{epi_slide_sum}
\title{Calculate rolling or running means, sums, etc., or custom calculations}
\usage{
epi_slide_opt(
  .x,
  .col_names,
  .f,
  ...,
  .window_size = NULL,
  .align = c("right", "center", "left"),
  .prefix = NULL,
  .suffix = NULL,
  .new_col_names = NULL,
  .ref_time_values = NULL,
  .all_rows = FALSE
)

epi_slide_mean(
  .x,
  .col_names,
  ...,
  .window_size = NULL,
  .align = c("right", "center", "left"),
  .prefix = NULL,
  .suffix = NULL,
  .new_col_names = NULL,
  .ref_time_values = NULL,
  .all_rows = FALSE
)

epi_slide_sum(
  .x,
  .col_names,
  ...,
  .window_size = NULL,
  .align = c("right", "center", "left"),
  .prefix = NULL,
  .suffix = NULL,
  .new_col_names = NULL,
  .ref_time_values = NULL,
  .all_rows = FALSE
)
}
\arguments{
\item{.x}{An \code{epi_df} object. If ungrouped, we temporarily group by \code{geo_value}
and any columns in \code{other_keys}. If grouped, we make sure the grouping is by
\code{geo_value} and \code{other_keys}.}

\item{.col_names}{<\code{\link[dplyr:dplyr_tidy_select]{tidy-select}}> An unquoted column
name (e.g., \code{cases}), multiple column names (e.g., \code{c(cases, deaths)}),
\link[tidyselect:language]{other tidy-select expression}, or a vector of
characters (e.g. \code{c("cases", "deaths")}). Variable names can be used as if
they were positions in the data frame, so expressions like \code{x:y} can be
used to select a range of variables.

The tidy-selection renaming interface is not supported, and cannot be used
to provide output column names; if you want to customize the output column
names, use \code{\link[dplyr:rename]{dplyr::rename}} after the slide.}

\item{.f}{Function; together with \code{...} specifies the computation to slide.
\code{.f} must be one of \code{data.table}'s rolling functions
(\code{frollmean}, \code{frollsum}, \code{frollapply}. See \link[data.table:froll]{data.table::roll}) or one
of \code{slider}'s specialized sliding functions (\code{slide_mean}, \code{slide_sum},
etc. See \link[slider:summary-slide]{slider::summary-slide}).

The optimized \code{data.table} and \code{slider} functions can't be directly passed
as the computation function in \code{epi_slide} without careful handling to make
sure each computation group is made up of the \code{.window_size} dates rather
than \code{.window_size} points. \code{epi_slide_opt} (and wrapper functions
\code{epi_slide_mean} and \code{epi_slide_sum}) take care of window completion
automatically to prevent associated errors.}

\item{...}{Additional arguments to pass to the slide computation \code{.f}, for
example, \code{algo} or \code{na.rm} in data.table functions. You don't need to
specify \code{.x}, \code{.window_size}, or \code{.align} (or \code{before}/\code{after} for slider
functions).}

\item{.window_size}{The size of the sliding window. The accepted values
depend on the type of the \code{time_value} column in \code{.x}:
\itemize{
\item if time type is \code{Date} and the cadence is daily, then \code{.window_size} can
be an integer (which will be interpreted in units of days) or a difftime
with units "days"
\item if time type is \code{Date} and the cadence is weekly, then \code{.window_size} must
be a \code{difftime} with units "weeks"
\item if time type is a \code{yearmonth} or an integer, then \code{.window_size} must be an
integer
}}

\item{.align}{The alignment of the sliding window.
\itemize{
\item If "right" (default), then the window has its end at the reference time.
This is likely the most common use case, e.g. \code{.window_size=7} and
\code{.align="right"} slides over the past week of data.
\item If "left", then the window has its start at the reference time.
\item If "center", then the window is centered at the reference time. If the
window size is odd, then the window will have floor(window_size/2) points
before and after the reference time; if the window size is even, then the
window will be asymmetric and have one more value before the reference time
than after.
}}

\item{.prefix}{Optional \code{\link[glue:glue]{glue::glue}} format string; name the slide result
column(s) by attaching this prefix to the corresponding input column(s).
Some shorthand is supported for basing the output names on \code{.window_size}
or other arguments; see "Prefix and suffix shorthand" below.}

\item{.suffix}{Optional \code{\link[glue:glue]{glue::glue}} format string; like \code{.prefix}. The
default naming behavior is equivalent to \code{.suffix = "_{.n}{.time_unit_abbr}{.align_abbr}{.f_abbr}"}. Can be used in combination
with \code{.prefix}.}

\item{.new_col_names}{Optional character vector with length matching the
number of input columns from \code{.col_names}; name the slide result column(s)
with these names. Cannot be used in combination with \code{.prefix} and/or
\code{.suffix}.}

\item{.ref_time_values}{The time values at which to compute the slides
values. By default, this is all the unique time values in \code{.x}.}

\item{.all_rows}{If \code{.all_rows = FALSE}, the default, then the output
\code{epi_df} will have only the rows that had a \code{time_value} in
\code{.ref_time_values}. Otherwise, all the rows from \code{.x} are included by with
a missing value marker (typically NA, but more technically the result of
\code{vctrs::vec_cast}-ing \code{NA} to the type of the slide computation output).}
}
\value{
An \code{epi_df} object with one or more new slide computation columns
added. It will be ungrouped if \code{.x} was ungrouped, and have the same groups
as \code{.x} if \code{.x} was grouped.
}
\description{
These methods take each subpopulation (i.e., a single
\code{geo_value} and combination of any \code{other_keys} you set up for age groups,
etc.) and perform a \code{.window_size}-width time window rolling/sliding
computation, or alternatively, a running/cumulative computation (with
\code{.window_size = Inf}) on the requested columns. Explicit \code{NA} measurements
are temporarily added to fill in any time gaps, and, for rolling
computations, to pad the time series to ensure that the first & last
computations use exactly \code{.window_size} values.

\code{epi_slide_opt} allows you to use any \link[data.table:froll]{data.table::froll} or
\link[slider:summary-slide]{slider::summary-slide} function. If none of those specialized functions fit
your usecase, you can use \code{data.table::frollapply} together with a non-rolling
function (e.g., \code{median}). See \code{\link{epi_slide}} if you need to work with
multiple columns at once or output a custom type.

\code{epi_slide_mean} is a wrapper around \code{epi_slide_opt} with \code{.f = data.table::frollmean}.

\code{epi_slide_sum} is a wrapper around \code{epi_slide_opt} with \code{.f = data.table::frollsum}.
}
\section{Prefix and suffix shorthand}{


\code{\link[glue:glue]{glue::glue}} format strings specially interpret content within curly
braces. E.g., \code{glue::glue("ABC{2 + 2}")} evaluates to \code{"ABC4"}. For \code{.prefix}
and \code{.suffix}, we provide \code{glue} with some additional variable bindings:
\itemize{
\item \code{{.n}} will be the number of time steps in the computation
corresponding to the \code{.window_size}.
\item \code{{.time_unit_abbr}} will be a lower-case letter corresponding to the
\code{time_type} of \code{.x}
\item \code{{.align_abbr}} will be \code{""} if \code{.align} is the default of \code{"right"};
otherwise, it will be the first letter of \code{.align}
\item \code{{.f_abbr}} will be a character vector containing a short abbreviation
for \code{.f} factoring in the input column type(s) for \code{.col_names}
}
}

\examples{
library(dplyr)

# Add a column (`cases_7dsum`) containing a 7-day trailing sum on `cases`:
cases_deaths_subset \%>\%
  select(geo_value, time_value, cases) \%>\%
  epi_slide_sum(cases, .window_size = 7)

# Add a column (`cases_rate_7dav`) containing a 7-day trailing average on `case_rate`:
covid_case_death_rates_extended \%>\%
  epi_slide_mean(case_rate, .window_size = 7)

# Use a less common specialized slide function:
cases_deaths_subset \%>\%
  epi_slide_opt(cases, slider::slide_min, .window_size = 7)

# Specify output column names and/or a naming scheme:
cases_deaths_subset \%>\%
  select(geo_value, time_value, cases) \%>\%
  group_by(geo_value) \%>\%
  epi_slide_sum(cases, .window_size = 7, .new_col_names = "case_sum") \%>\%
  ungroup()
cases_deaths_subset \%>\%
  select(geo_value, time_value, cases) \%>\%
  group_by(geo_value) \%>\%
  epi_slide_sum(cases, .window_size = 7, .prefix = "sum_") \%>\%
  ungroup()

# Additional settings can be sent to the {data.table} and {slider} functions
# via `...`. This example passes some arguments to `frollmean` settings for
# speed, accuracy, and to allow partially-missing windows:
covid_case_death_rates_extended \%>\%
  epi_slide_mean(
    case_rate,
    .window_size = 7,
    na.rm = TRUE, algo = "exact", hasNA = TRUE
  )

# If the more specialized possibilities for `.f` don't cover your needs, you
# can use `epi_slide_opt` with `.f = data.table::frollapply` to apply a
# custom function at the cost of more computation time. See also `epi_slide`
# if you need something even more general.
cases_deaths_subset \%>\%
  select(geo_value, time_value, case_rate_7d_av, death_rate_7d_av) \%>\%
  epi_slide_opt(c(case_rate_7d_av, death_rate_7d_av),
    data.table::frollapply,
    FUN = median, .window_size = 28,
    .suffix = "_{.n}{.time_unit_abbr}_median"
  ) \%>\%
  print(n = 40)
}
\seealso{
\code{\link{epi_slide}} for the more general slide function
}
