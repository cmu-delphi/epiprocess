test_that("linelist_to_archive works with basic inputs", {
  linelist <- tibble(
    event_id = 1:3,
    geo_value = c("ca", "ca", "ca"),
    time_value = as.Date(c("2022-01-01", "2022-01-01", "2022-01-02")),
    version_recorded = as.Date(c("2022-01-02", "2022-01-02", "2022-01-03")),
    version_deleted = as.Date(c("2022-01-04", NA, NA))
  )

  # Basic call
  ea <- linelist_to_archive(
    linelist,
    geo_value = geo_value,
    time_value = time_value,
    version_recorded = version_recorded,
    version_deleted = version_deleted
  )

  expect_s3_class(ea, "epi_archive")
  expect_equal(ea$DT$n, c(2, 1, 1))

  # Check content at different versions
  df_v2 <- epix_as_of(ea, as.Date("2022-01-02"))
  expect_equal(nrow(df_v2), 1)
  expect_equal(df_v2$n, 2)
  expect_equal(df_v2$time_value, as.Date("2022-01-01"))

  df_v3 <- epix_as_of(ea, as.Date("2022-01-03"))
  expect_equal(nrow(df_v3), 2)
  expect_equal(sum(df_v3$n), 3)

  df_v4 <- epix_as_of(ea, as.Date("2022-01-04"))
  expect_equal(nrow(df_v4), 2)
  expect_equal(df_v4$n[df_v4$time_value == "2022-01-01"], 1)
})

test_that("linelist_to_archive handles tidy selection", {
  linelist <- tibble::tibble(
    state = "ca",
    date = as.Date("2022-01-01"),
    recorded = as.Date("2022-01-02")
  )

  ea <- linelist_to_archive(
    linelist,
    geo_value = state,
    time_value = date,
    version_recorded = recorded
  )

  expect_s3_class(ea, "epi_archive")
  expect_equal(ea$DT$geo_value, "ca")
})

test_that("linelist_to_archive handles other_keys", {
  linelist <- tibble::tibble(
    geo_value = "ca",
    time_value = as.Date("2022-01-01"),
    version_recorded = as.Date("2022-01-02"),
    age_group = "adult"
  )

  ea <- linelist_to_archive(
    linelist,
    geo_value = geo_value,
    time_value = time_value,
    version_recorded = version_recorded,
    other_keys = age_group
  )

  expect_true("age_group" %in% names(ea$DT))
  expect_equal(key(ea$DT), c("geo_value", "time_value", "age_group", "version"))
})

test_that("linelist_to_archive errors on missing required cols", {
  linelist <- tibble::tibble(
    geo_value = "ca",
    time_value = as.Date("2022-01-01")
  )
  # Missing version_recorded
  expect_error(linelist_to_archive(linelist))
})
test_that("linelist_to_archive validates id uniqueness", {
  # duplicate creation for id
  linelist_bad <- tibble::tibble(
    id = c("A", "A"),
    geo_value = "ca",
    time_value = as.Date("2022-01-01"),
    recorded = as.Date("2022-01-02")
  )

  expect_error(
    linelist_to_archive(linelist_bad,
      geo_value = geo_value,
      time_value = time_value,
      version_recorded = recorded, id = id
    ),
    "must have at most one entry"
  )
})

test_that("linelist_to_archive refuses to handle split rows with id", {
  # One row creates, one row deletes
  linelist_split <- tibble::tibble(
    case_id = c("A", "A"),
    geo_value = "ca",
    time_value = as.Date("2022-01-01"),
    recorded = as.Date(c("2022-01-02", NA)),
    deleted = as.Date(c(NA, "2022-01-05"))
  )

  expect_error(
    ea <- linelist_to_archive(
      linelist_split,
      geo_value = geo_value,
      time_value = time_value,
      version_recorded = recorded,
      version_deleted = deleted,
      id = case_id
    ),
    class = "epiprocess__linelist_to_archive__ver_rec_had_nas"
  )
})

test_that("linelist_to_archive enforces deleted >= recorded with id", {
  linelist_bad <- tibble::tibble(
    id = "A",
    geo_value = "ca",
    time_value = as.Date("2022-01-01"),
    recorded = as.Date("2022-01-05"),
    deleted = as.Date("2022-01-04") # Deleted BEFORE recorded
  )

  expect_error(
    linelist_to_archive(linelist_bad,
      geo_value = geo_value, time_value = time_value,
      version_recorded = recorded, version_deleted = deleted, id = id
    ),
    "must be >= "
  )
})
test_that("linelist_to_archive refuses to handle unordered split rows with id", {
  linelist_unordered <- tibble::tibble(
    case_id = c("A", "A"),
    geo_value = "ca",
    time_value = as.Date("2022-01-01"),
    recorded = as.Date(c(NA, "2022-01-02")),
    deleted = as.Date(c("2022-01-05", NA))
  )

  expect_error(
    ea <- linelist_to_archive(
      linelist_unordered,
      geo_value = geo_value,
      time_value = time_value,
      version_recorded = recorded,
      version_deleted = deleted,
      id = case_id
    ),
    class = "epiprocess__linelist_to_archive__ver_rec_had_nas"
  )
})
test_that("linelist_to_archive uses smart defaults", {
  # All defaults
  # geo_value -> state, time_value -> date, version_recorded -> issue
  linelist_default <- tibble::tibble(
    state = "ca",
    date = as.Date("2022-01-01"),
    issue = as.Date("2022-01-02"),
    value = 1
  )

  # Should work without args
  ea <- linelist_to_archive(linelist_default)
  expect_s3_class(ea, "epi_archive")
  expect_equal(ea$DT$geo_value, "ca")

  # Partial defaults
  # geo_value -> geo_value, time_value -> date, version -> my_ver
  linelist_mixed <- tibble::tibble(
    geo_value = "ny",
    date = as.Date("2022-01-01"),
    my_ver = as.Date("2022-01-02")
  )

  ea2 <- linelist_to_archive(linelist_mixed, version_recorded = my_ver)
  expect_s3_class(ea2, "epi_archive")
  expect_equal(ea2$DT$geo_value, "ny")
})

test_that("linelist_to_archive errors when defaults not found", {
  linelist_bad <- tibble::tibble(
    loc = "ca", # not in default list
    day = as.Date("2022-01-01"), # not in default list
    ver = as.Date("2022-01-02")
  )

  expect_error(linelist_to_archive(linelist_bad), class = "epiprocess__resolve_col__autoselection_failed")
})

test_that("linelist_to_archive errors when too many defaults not found", {
  linelist_bad2 <- tibble::tibble(
    geo_value = "ca", # not in default list
    date = as.Date("2022-01-01"),
    time_value = date,
    ver = as.Date("2022-01-02")
  )

  expect_error(linelist_to_archive(linelist_bad2), class = "epiprocess__resolve_col__selected_multiple")
})

test_that("linelist_to_archive supports chart-style linelists", {
  # chart-style linelist
  x <- tibble::tibble(
    geo_value = "ma",
    time_value = as.Date("2020-01-01"),
    issue_date = as.Date(c("2020-01-02", "2020-01-02", "2020-01-03")),
    is_deletion = c(FALSE, FALSE, TRUE),
    id = c(1, 2, 1)
  )

  # Should fail if both version_deleted and is_deletion provided (mutually exclusive)
  expect_error(
    linelist_to_archive(x,
      geo_value = geo_value, time_value = time_value,
      version_recorded = issue_date, version_deleted = issue_date,
      is_deletion = is_deletion, id = id
    ),
    "mutually exclusive"
  )

  # Should work with is_deletion
  ea <- linelist_to_archive(
    x,
    geo_value = geo_value, time_value = time_value,
    version_recorded = issue_date,
    is_deletion = is_deletion, id = id, value = "count"
  )

  # Verify history using snapshots

  snap1 <- epix_as_of(ea, as.Date("2020-01-02"))
  expect_equal(nrow(snap1), 1)
  expect_equal(snap1$count, 2)

  snap2 <- epix_as_of(ea, as.Date("2020-01-03"))
  expect_equal(nrow(snap2), 1)
  expect_equal(snap2$count, 1)
})

test_that("linelist_to_archive chart-style validation works", {
  x <- tibble::tibble(
    geo_value = "ma",
    time_value = as.Date("2020-01-01"),
    issue_date = as.Date("2020-01-02"),
    is_deletion = c(NA),
    id = 1
  )

  expect_error(
    linelist_to_archive(
      x,
      geo_value = geo_value, time_value = time_value,
      version_recorded = issue_date,
      is_deletion = is_deletion, id = id
    ),
    "must not contain NAs"
  )
})

test_that("linelist_to_archive matches complete() for complex data", {
  # simulate individual cases with IDs.

  set.seed(42)
  n_cases <- 200

  # Scenario: constant flow of cases over 50 days
  days <- seq(as.Date("2023-01-01"), as.Date("2023-03-01"), by = "1 day")

  # Generate cases
  cases <- tibble(
    id = 1:n_cases,
    geo_value = "CA",
    # random day in the range
    time_value = sample(days, n_cases, replace = TRUE),
    # most reported quickly, some late
    lag_rec = rgeom(n_cases, 0.2),
    # cases later deleted
    is_deletion = runif(n_cases) < 0.02,
    lag_del = rgeom(n_cases, 0.1)
  ) %>%
    mutate(
      version_recorded = time_value + lag_rec,
      # Deleted some time after recording
      version_deleted = if_else(is_deletion, version_recorded + lag_del + 1, as.Date(NA))
    )

  linelist <- cases %>%
    select(id, geo_value, time_value, version_recorded, version_deleted) %>%
    arrange(version_recorded)

  # Run Archive
  ea <- linelist_to_archive(
    linelist,
    geo_value = geo_value,
    time_value = time_value,
    version_recorded = version_recorded,
    version_deleted = version_deleted,
    id = id,
    value = "cases"
  )


  # Compute Reference
  max_ver <- max(linelist$version_recorded, linelist$version_deleted, na.rm = TRUE)

  active_df <- linelist %>%
    filter(version_recorded <= max_ver) %>%
    filter(is.na(version_deleted) | version_deleted > max_ver)

  all_locs <- c("CA")
  all_times <- days

  reference_df <- active_df %>%
    group_by(geo_value, time_value) %>%
    summarise(cases = as.double(dplyr::n()), .groups = "drop") %>%
    complete(
      geo_value = all_locs,
      time_value = all_times,
      fill = list(cases = 0)
    ) %>%
    arrange(geo_value, time_value)

  # The archive for the snapshot at max_ver
  archive_df <- epix_as_of(ea, max_ver) %>%
    select(geo_value, time_value, cases) %>%
    arrange(geo_value, time_value)

  observed_times <- unique(sort(linelist$time_value))

  reference_df_corrected <- active_df %>%
    group_by(geo_value, time_value) %>%
    summarise(cases = as.double(dplyr::n()), .groups = "drop") %>%
    complete(
      geo_value = all_locs,
      time_value = observed_times,
      fill = list(cases = 0)
    ) %>%
    arrange(geo_value, time_value)

  archive_df_filtered <- archive_df %>%
    filter(time_value %in% observed_times)

  expect_equal(nrow(archive_df_filtered), nrow(reference_df_corrected))

  expect_true(all.equal(
    as.data.frame(archive_df_filtered),
    as.data.frame(reference_df_corrected),
    check.attributes = FALSE
  ))
})
