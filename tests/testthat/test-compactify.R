dt <- archive_cases_dv_subset$DT
dt <- filter(dt, geo_value == "ca") %>%
  filter(version <= "2020-06-15") %>%
  select(-case_rate_7d_av)

test_that("Input for compactify must be NULL or a boolean", {
  expect_error(as_epi_archive(dt, compactify = "no"))
})

dt$percent_cli <- c(1:80)
dt$case_rate <- c(1:80)

row_replace <- function(dt, row, x, y) {
  # (This way of "replacing" elements appears to use copy-on-write even though
  # we are working with a data.table.)
  dt[row, 4] <- x
  dt[row, 5] <- y
  dt
}

# Note that compactify is working on version-wise LOCF (last version of each
# observation carried forward)

# Rows 1 should not be eliminated even if NA
dt <- row_replace(dt, 1, NA, NA) # Not LOCF

# NOTE! We are assuming that there are no NA's in geo_value, time_value,
# and version. Even though compactify may erroneously remove the first row
# if it has all NA's, we are not testing this behaviour for now as this dataset
# has problems beyond the scope of this test

# Rows 11 and 12 correspond to different time_values
dt <- row_replace(dt, 12, 11, 11) # Not LOCF

# Rows 20 and 21 only differ in version
dt <- row_replace(dt, 21, 20, 20) # LOCF

# Rows 21 and 22 only differ in version
dt <- row_replace(dt, 22, 20, 20) # LOCF

# Row 39 comprises the first NA's
dt <- row_replace(dt, 39, NA, NA) # Not LOCF

# Row 40 has two NA's, just like its lag, row 39
dt <- row_replace(dt, 40, NA, NA) # LOCF

# Row 62's values already exist in row 15, but row 15 is not a preceding row
dt <- row_replace(dt, 62, 15, 15) # Not LOCF

# Row 73 only has one value carried over
dt <- row_replace(dt, 74, 73, 74) # Not LOCF

dt_true <- as_tibble(as_epi_archive(dt, compactify = TRUE)$DT)
dt_false <- as_tibble(as_epi_archive(dt, compactify = FALSE)$DT)
dt_message <- suppressMessages(as_tibble(as_epi_archive(dt, compactify = "message")$DT))

test_that('Warning for LOCF with compactify as "message"', {
  expect_message(as_epi_archive(dt, compactify = "message"))
})

test_that("No warning when there is no LOCF", {
  expect_no_message(as_epi_archive(dt[1:5], compactify = "message"))
})

test_that("LOCF values are ignored with compactify=FALSE", {
  expect_identical(nrow(dt), nrow(dt_false))
})

test_that("LOCF values are taken out with compactify=TRUE", {
  dt_test <- as_tibble(as_epi_archive(dt[-c(21, 22, 40), ], compactify = FALSE)$DT)

  expect_identical(dt_true, dt_message)
  expect_identical(dt_message, dt_test)
})

test_that("as_of produces the same results with compactify=TRUE as with compactify=FALSE", {
  ea_true <- as_epi_archive(dt, compactify = TRUE)
  ea_false <- as_epi_archive(dt, compactify = FALSE)

  # Row 22, an LOCF row corresponding to the latest version, is omitted in
  # ea_true
  latest_version <- max(ea_false$DT$version)
  as_of_true <- epix_as_of(ea_true, latest_version)
  as_of_false <- epix_as_of(ea_false, latest_version)

  expect_identical(as_of_true, as_of_false)
})

test_that("compactify does not alter the default clobberable and observed version bounds", {
  d <- as.Date("2000-01-01")
  x <- tibble::tibble(
    geo_value = "ak",
    time_value = d,
    version = d + 1:5,
    value = 42L
  )
  ea_true <- as_epi_archive(x, compactify = TRUE)
  ea_false <- as_epi_archive(x, compactify = FALSE)
  # We say that we base the bounds on the user's `x` arg. We might mess up or
  # change our minds and base things on the `DT` field (or a temporary `DT`
  # variable, post-compactify) instead. Check that this test would trigger
  # in that case:
  expect_true(max(ea_true$DT$version) != max(ea_false$DT$version))
  # The actual test:
  expect_identical(ea_true$clobberable_versions_start, ea_false$clobberable_versions_start)
  expect_identical(ea_true$versions_end, ea_false$versions_end)
})

test_that("compactify works on distributions", {
  forecasts <- tibble(
    ahead = 2L,
    geo_value = "ak",
    target_end_date = as.Date("2020-01-19"),
    forecast_date = as.Date("2020-01-17") + 1:8,
    actual = 25,
    .pred_distn = c(
      epipredict::dist_quantiles(c(1, 5, 9), c(0.1, 0.5, 0.9)),
      epipredict::dist_quantiles(c(1, NA, 9), c(0.1, 0.5, 0.9)), # single NA in quantiles
      epipredict::dist_quantiles(c(NA, NA, NA), c(0.1, 0.5, 0.9)), # all NAs in quantiles
      distributional::dist_missing(1), # the actual `NA` for distributions
      epipredict::dist_quantiles(c(1, 5, 9), c(0.1, 0.5, 0.9)), # and back
      epipredict::dist_quantiles(c(3, 5, 9), c(0.1, 0.5, 0.9)), # change quantile
      epipredict::dist_quantiles(c(3, 5, 9), c(0.2, 0.5, 0.8)), # change level
      epipredict::dist_quantiles(c(3, 5, 9), c(0.2, 0.5, 0.8)) # LOCF
    )
  )
  expect_equal(
    forecasts %>%
      as_epi_archive(
        other_keys = "ahead", time_value = target_end_date, version = forecast_date,
        compactify = TRUE
      ) %>%
      .$DT %>%
      as.data.frame() %>%
      as_tibble(),
    forecasts[-8, ] %>%
      rename(time_value = target_end_date, version = forecast_date)
  )
})
