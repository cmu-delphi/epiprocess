% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-epi_df.R
\name{complete.epi_df}
\alias{complete.epi_df}
\title{Complete epi_df}
\usage{
\method{complete}{epi_df}(data, ..., fill = list(), explicit = TRUE)
}
\arguments{
\item{data}{an \code{epi_df}}

\item{...}{see \code{\link[tidyr:complete]{tidyr::complete}}}

\item{fill}{see \code{\link[tidyr:complete]{tidyr::complete}}}

\item{explicit}{see \code{\link[tidyr:complete]{tidyr::complete}}}
}
\description{
A ‘tidyr::complete()’ analogue for ‘epi_df’ objects. This function
can be used, for example, to add rows for missing combinations
of ‘geo_value’ and ‘time_value’, filling other columns with \code{NA}s.
See the examples for usage details.
}
\examples{
start_date <- as.Date("2020-01-01")
daily_edf <- tibble::tribble(
  ~geo_value, ~time_value, ~value,
  1, start_date + 1, 1,
  1, start_date + 3, 3,
  2, start_date + 2, 2,
  2, start_date + 3, 3,
) \%>\%
  as_epi_df(as_of = start_date + 3)
# Complete without grouping puts all the geo_values on the same min and max
# time_value index
daily_edf \%>\%
  complete(geo_value, time_value = full_seq(time_value, period = 1))
# Complete with grouping puts all the geo_values on individual min and max
# time_value indices
daily_edf \%>\%
  group_by(geo_value) \%>\%
  complete(time_value = full_seq(time_value, period = 1))
# Complete has explicit=TRUE by default, but if it's FALSE, then complete
# only fills the implicit gaps, not those that are explicitly NA
daily_edf <- tibble::tribble(
  ~geo_value, ~time_value, ~value,
  1, start_date + 1, 1,
  1, start_date + 2, NA,
  1, start_date + 3, 3,
  2, start_date + 2, 2,
  2, start_date + 3, 3,
) \%>\%
  as_epi_df(as_of = start_date + 3)
daily_edf \%>\%
  complete(
    geo_value,
    time_value = full_seq(time_value, period = 1),
    fill = list(value = 0),
    explicit = FALSE
  )
# Complete works for weekly data and can take a fill value
# No grouping
weekly_edf <- tibble::tribble(
  ~geo_value, ~time_value, ~value,
  1, start_date + 1, 1,
  1, start_date + 15, 3,
  2, start_date + 8, 2,
  2, start_date + 15, 3,
) \%>\%
  as_epi_df(as_of = start_date + 3)
weekly_edf \%>\%
  complete(
    geo_value,
    time_value = full_seq(time_value, period = 7),
    fill = list(value = 0)
  )
# With grouping
weekly_edf \%>\%
  group_by(geo_value) \%>\%
  complete(
    time_value = full_seq(time_value, period = 7),
    fill = list(value = 0)
  )
}
