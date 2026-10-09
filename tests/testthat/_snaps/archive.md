# as_epi_archive default compactification (no longer messages/warns)

    Code
      res <- dumb_ex %>% as_epi_archive()

# POSIXct versions are converted to Dates

    Code
      et_result <- late_et_display_et %>% as_epi_archive()
    Message <epiprocess__as_epi_archive__datetime_version>
      POSIXct `version`s are not yet supported; converting to Dates in x$version's display time zone, "America/New_York".
      i Only keeping the last version of each measurement if there are multiple within a day.

---

    Code
      withr::with_envvar(list(TZ = "America/New_York"), {
        utc_result1 <- late_et_display_et %>% mutate(version = as.POSIXct(version,
          tz = "UTC")) %>% as_epi_archive()
      })
    Message <epiprocess__as_epi_archive__datetime_version>
      POSIXct `version`s are not yet supported; converting to Dates in x$version's display time zone, "UTC".
      i Only keeping the last version of each measurement if there are multiple within a day.

---

    Code
      withr::with_envvar(list(TZ = "UTC"), {
        utc_result2 <- late_et_display_et %>% mutate(version = as.POSIXct(version,
          tz = "")) %>% as_epi_archive()
      })
    Message <epiprocess__as_epi_archive__datetime_version>
      POSIXct `version`s are not yet supported; converting to Dates in R session time zone, "UTC".
      i Only keeping the last version of each measurement if there are multiple within a day.

---

    Code
      withr::with_envvar(list(TZ = "UTC"), {
        utc_result3 <- late_et_display_et %>% mutate(version = as.POSIXct(version,
          tz = NULL)) %>% as_epi_archive()
      })
    Message <epiprocess__as_epi_archive__datetime_version>
      POSIXct `version`s are not yet supported; converting to Dates in R session time zone, "UTC".
      i Only keeping the last version of each measurement if there are multiple within a day.

---

    Assertion on 'x' failed: There cannot be more than one row with the same combination of geo_value, time_value, and version.  Problematic rows:
    # A tibble: 2 x 4
      geo_value time_value version             value
          <dbl> <date>     <dttm>              <int>
    1         2 2020-01-01 2020-01-08 08:30:00     2
    2         2 2020-01-01 2020-01-08 08:30:00     3
    .

