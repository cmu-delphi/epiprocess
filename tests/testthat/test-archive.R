test_that("first input must be a data.frame", {
  expect_error(as_epi_archive(c(1, 2, 3), compactify = FALSE),
    regexp = "Must be of type 'data.frame'."
  )
})

archive_data <- archive_cases_dv_subset$DT

test_that("data.frame must contain geo_value, time_value and version columns", {
  expect_error(as_epi_archive(select(archive_data, -geo_value), compactify = FALSE),
    regexp = "There is no geo_value column or similar name"
  )
  expect_error(as_epi_archive(select(archive_data, -time_value), compactify = FALSE),
    regexp = "There is no time_value column or similar name"
  )
  expect_error(as_epi_archive(select(archive_data, -version), compactify = FALSE),
    regexp = "There is no version column or similar name"
  )
})

test_that("as_epi_archive custom name mapping works correctly", {
  # custom name works correctly
  expect_equal(
    as_epi_archive(rename(archive_data, weirdName = version),
      version = weirdName, compactify = TRUE
    ),
    as_epi_archive(archive_data, compactify = TRUE)
  )
  expect_equal(
    as_epi_archive(rename(archive_data, weirdName = geo_value),
      geo_value = weirdName, compactify = TRUE
    ),
    as_epi_archive(archive_data, compactify = TRUE)
  )
  expect_equal(
    as_epi_archive(rename(archive_data, weirdName = time_value),
      time_value = weirdName, compactify = TRUE
    ),
    as_epi_archive(archive_data, compactify = TRUE)
  )

  expect_error(
    as_epi_archive(
      rename(archive_data, weirdName = version),
      version = weirdName,
      version = time_value
    ), "Names must be unique"
  )
})

test_that("other_keys can only contain names of the data.frame columns", {
  expect_error(as_epi_archive(archive_data, other_keys = "xyz", compactify = FALSE),
    regexp = "`other_keys` must be contained in the column names of `x`."
  )
  expect_error(as_epi_archive(archive_data, other_keys = "percent_cli", compactify = FALSE), NA)
})

test_that("other_keys cannot contain names geo_value, time_value or version", {
  expect_error(as_epi_archive(archive_data, other_keys = "geo_value", compactify = FALSE),
    regexp = "`other_keys` cannot contain \"geo_value\", \"time_value\", or \"version\"."
  )
  expect_error(as_epi_archive(archive_data, other_keys = "time_value", compactify = FALSE),
    regexp = "`other_keys` cannot contain \"geo_value\", \"time_value\", or \"version\"."
  )
  expect_error(as_epi_archive(archive_data, other_keys = "version", compactify = FALSE),
    regexp = "`other_keys` cannot contain \"geo_value\", \"time_value\", or \"version\"."
  )
})

test_that("Warning thrown when other_metadata contains overlapping names with geo_type field", {
  expect_warning(as_epi_archive(archive_data, additional_metadata = list(geo_type = 1), compactify = FALSE),
    regexp = "`additional_metadata` names overlap with existing metadata fields"
  )
  expect_warning(as_epi_archive(archive_data, additional_metadata = list(time_type = 1), compactify = FALSE),
    regexp = "`additional_metadata` names overlap with existing metadata fields"
  )
})

test_that("epi_archives are correctly instantiated with a variety of data types", {
  d <- as.Date("2020-01-01")
  # Data frame
  df <- data.frame(
    geo_value = "ca",
    time_value = d,
    version = d + 0:19,
    value = 1:20
  )

  ea1 <- as_epi_archive(df, compactify = FALSE)
  expect_equal(key(ea1$DT), c("geo_value", "time_value", "version"))
  expect_equal(ea1$additional_metadata, list())

  ea2 <- as_epi_archive(df, other_keys = "value", additional_metadata = list(value = df$value), compactify = FALSE)
  expect_equal(key(ea2$DT), c("geo_value", "time_value", "value", "version"))
  expect_equal(ea2$additional_metadata, list(value = df$value))

  # Tibble
  tib <- tibble::tibble(df, code = "x")

  ea3 <- as_epi_archive(tib, compactify = FALSE)
  expect_equal(key(ea3$DT), c("geo_value", "time_value", "version"))
  expect_equal(ea3$additional_metadata, list())

  ea4 <- as_epi_archive(tib, other_keys = "code", additional_metadata = list(value = df$value), compactify = FALSE)
  expect_equal(key(ea4$DT), c("geo_value", "time_value", "code", "version"))
  expect_equal(ea4$additional_metadata, list(value = df$value))

  # Keyed data.table
  kdt <- data.table::data.table(
    geo_value = "ca",
    time_value = d,
    version = d + 0:19,
    value = 1:20,
    code = "CA",
    key = "code"
  )

  ea5 <- as_epi_archive(kdt, compactify = FALSE)
  # Key from data.table isn't absorbed when as_epi_archive is used
  expect_equal(key(ea5$DT), c("geo_value", "time_value", "version"))
  expect_equal(ea5$additional_metadata, list())

  ea6 <- as_epi_archive(kdt, other_keys = "value", additional_metadata = list(value = df$value), compactify = FALSE)
  # Mismatched keys, but the one from as_epi_archive overrides
  expect_equal(key(ea6$DT), c("geo_value", "time_value", "value", "version"))
  expect_equal(ea6$additional_metadata, list(value = df$value))

  # Unkeyed data.table
  udt <- data.table::data.table(
    geo_value = "ca",
    time_value = d,
    version = d + 0:19,
    value = 1:20,
    code = "CA"
  )

  ea7 <- as_epi_archive(udt, compactify = FALSE)
  expect_equal(key(ea7$DT), c("geo_value", "time_value", "version"))
  expect_equal(ea7$additional_metadata, list())

  ea8 <- as_epi_archive(udt, other_keys = "code", additional_metadata = list(value = df$value), compactify = FALSE)
  expect_equal(key(ea8$DT), c("geo_value", "time_value", "code", "version"))
  expect_equal(ea8$additional_metadata, list(value = df$value))

  # epi_df
  edf1 <- jhu_csse_daily_subset %>%
    select(geo_value, time_value, cases) %>%
    mutate(version = max(time_value), code = "USA")

  ea9 <- as_epi_archive(edf1, compactify = FALSE)
  expect_equal(key(ea9$DT), c("geo_value", "time_value", "version"))
  expect_equal(ea9$additional_metadata, list())

  ea10 <- as_epi_archive(edf1, other_keys = "code", additional_metadata = list(value = df$value), compactify = FALSE)
  expect_equal(key(ea10$DT), c("geo_value", "time_value", "code", "version"))
  expect_equal(ea10$additional_metadata, list(value = df$value))

  # Keyed epi_df
  edf2 <- data.frame(
    geo_value = "al",
    time_value = rep(d + 0:9, 2),
    version = c(
      rep(as.Date("2020-01-25"), 10),
      rep(as.Date("2020-01-26"), 10)
    ),
    cases = 1:20,
    misc = "USA"
  ) %>%
    as_epi_df(additional_metadata = list(other_keys = "misc"))

  ea11 <- as_epi_archive(edf2, compactify = FALSE)
  expect_equal(key(ea11$DT), c("geo_value", "time_value", "version"))
  expect_equal(ea11$additional_metadata, list())

  ea12 <- as_epi_archive(edf2, other_keys = "misc", additional_metadata = list(value = df$misc), compactify = FALSE)
  expect_equal(key(ea12$DT), c("geo_value", "time_value", "misc", "version"))
  expect_equal(ea12$additional_metadata, list(value = df$misc))
})

test_that("`epi_archive` rejects nonunique keys", {
  toy_update_tbl <- tibble::tribble(
    ~geo_value, ~age_group, ~time_value, ~version, ~value,
    "us", "adult", "2000-01-01", "2000-01-02", 121,
    "us", "adult", "2000-01-01", "2000-01-03", 125, # (revision)
    "us", "adult", "2000-01-02", "2000-01-03", 130,
    "us", "pediatric", "2000-01-01", "2000-01-02", 5
  ) %>%
    mutate(
      age_group = ordered(age_group, c("pediatric", "adult")),
      time_value = as.Date(time_value),
      version = as.Date(version)
    )
  expect_error(
    as_epi_archive(toy_update_tbl),
    class = "epiprocess__epi_archive_requires_unique_key"
  )
  expect_error(
    regexp = NA,
    as_epi_archive(toy_update_tbl, other_keys = "age_group"),
  )
})

test_that("`epi_archive` rejects dataframes where time_value and version columns don't share type", {
  tbl1 <- tibble::tribble(
    ~geo_value, ~age_group, ~time_value, ~version, ~value,
    "us", "adult", as.Date("2000-01-01"), as.Date("2000-01-02"), 121,
  ) %>%
    mutate(
      age_group = ordered(age_group, c("pediatric", "adult")),
    )
  expect_no_error(as_epi_archive(tbl1))
  tbl2 <- tibble::tribble(
    ~geo_value, ~age_group, ~time_value, ~version, ~value,
    "us", "adult", as.Date("2000-01-01"), 2022, 121,
  ) %>%
    mutate(
      age_group = ordered(age_group, c("pediatric", "adult")),
    )
  expect_error(as_epi_archive(tbl2), class = "epiprocess__time_value_version_mismatch")
  tbl3 <- tibble::tribble(
    ~geo_value, ~age_group, ~time_value, ~version, ~value,
    "us", "adult", as.Date("2000-01-01"), as.POSIXct("2000-01-01"), 121,
  ) %>%
    mutate(
      age_group = ordered(age_group, c("pediatric", "adult")),
    )
  expect_error(as_epi_archive(tbl3), class = "epiprocess__time_value_version_mismatch")
})
