# as_epi_archive default compactification (no longer messages/warns)

    Code
      res <- dumb_ex %>% as_epi_archive()

# Version dates as fake UTC midnight datetimes are converted.

    Datetime (POSIXct) `version`s are not yet supported.
    > Consider coarsening the versions into dates, keeping only the last version of each measurement within a day.

