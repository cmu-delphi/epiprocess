ea <- archive_cases_dv_subset
ea2_data <- tibble::tribble(
  ~geo_value, ~time_value, ~version, ~cases,
  "ca", "2020-06-01", "2020-06-01", 1,
  "ca", "2020-06-01", "2020-06-02", 2,
  #
  "ca", "2020-06-02", "2020-06-02", 0,
  "ca", "2020-06-02", "2020-06-03", 1,
  "ca", "2020-06-02", "2020-06-04", 2,
  #
  "ca", "2020-06-03", "2020-06-03", 1,
  #
  "ca", "2020-06-04", "2020-06-04", 4,
) %>%
  dplyr::mutate(dplyr::across(c(time_value, version), as.Date))

test_that("Errors are thrown due to bad epix_as_of inputs", {
  # max_version cannot be of string class rather than date class
  expect_error(ea %>% epix_as_of("2020-01-01"))
  # max_version cannot be later than latest version
  expect_error(ea %>% epix_as_of(as.Date("2025-01-01")))
  # max_version cannot be a vector
  expect_error(ea %>% epix_as_of(c(as.Date("2020-01-01"), as.Date("2020-01-02"))))
})

test_that("Warning against max_version being clobberable", {
  # none by default
  expect_warning(regexp = NA, ea %>% epix_as_of(max(ea$DT$version)))
  expect_warning(regexp = NA, ea %>% epix_as_of(min(ea$DT$version)))
  # but with `clobberable_versions_start` non-`NA`, yes
  ea_with_clobberable <- ea
  ea_with_clobberable$clobberable_versions_start <- max(ea_with_clobberable$DT$version)
  expect_warning(ea_with_clobberable %>% epix_as_of(max(ea$DT$version)))
  expect_warning(regexp = NA, ea_with_clobberable %>% epix_as_of(min(ea$DT$version)))
})

test_that("epix_as_of properly grabs the data and doesn't mutate key", {
  d <- as.Date("2020-06-01")

  ea2 <- ea2_data %>%
    as_epi_archive()

  old_key <- data.table::key(ea2$DT)

  edf_as_of <- ea2 %>%
    epix_as_of(as.Date("2020-06-03"))

  edf_expected <- as_epi_df(tibble(
    geo_value = "ca",
    time_value = d + 0:2,
    cases = c(2, 1, 1)
  ), as_of = as.Date("2020-06-03"))

  expect_equal(edf_as_of, edf_expected, ignore_attr = c(".internal.selfref", "sorted"))
  expect_equal(data.table::key(ea2$DT), old_key)
})

test_that("Errors are thrown due to bad epix_truncate_versions_after inputs", {
  # x must be an archive
  expect_error(epix_truncate_versions_after(data.frame(), as.Date("2020-01-01")))
  # max_version cannot be of string class rather than date class
  expect_error(epix_truncate_versions_after(ea, "2020-01-01"))
  # max_version cannot be a vector
  expect_error(epix_truncate_versions_after(ea, c(as.Date("2020-01-01"), as.Date("2020-01-02"))))
  # max_version cannot be missing
  expect_error(epix_truncate_versions_after(ea, as.Date(NA)))
  # max_version cannot be after latest version in archive
  expect_error(epix_truncate_versions_after(ea, as.Date("2025-01-01")))
})

test_that("epix_truncate_version_after properly grabs the data and doesn't mutate key", {
  ea2 <- ea2_data %>%
    as_epi_archive()

  old_key <- data.table::key(ea2$DT)

  ea_as_of <- ea2 %>%
    epix_truncate_versions_after(max_version = as.Date("2020-06-02"))

  ea_expected <- ea2_data[1:3, ] %>%
    as_epi_archive()

  expect_equal(ea_as_of, ea_expected, ignore_attr = c(".internal.selfref", "sorted"))
  expect_equal(data.table::key(ea2$DT), old_key)
})

test_that("epix_truncate_version_after doesn't filter if max_verion at latest version", {
  ea2 <- ea2_data %>%
    as_epi_archive()

  ea_expected <- ea2

  ea_as_of <- ea2 %>%
    epix_truncate_versions_after(max_version = as.Date("2020-06-04"))
  expect_equal(ea_as_of, ea_expected, ignore_attr = c(".internal.selfref", "sorted"))
})

test_that("epix_truncate_version_after returns the same grouping type as input epi_archive", {
  ea2 <- ea2_data %>%
    as_epi_archive()

  ea_as_of <- ea2 %>%
    epix_truncate_versions_after(max_version = as.Date("2020-06-04"))
  expect_class(ea_as_of, "epi_archive")

  ea2_grouped <- ea2 %>% group_by(geo_value)

  ea_as_of <- ea2_grouped %>%
    epix_truncate_versions_after(max_version = as.Date("2020-06-04"))
  expect_true(is_grouped_epi_archive(ea_as_of))
})


test_that("epix_truncate_version_after returns the same groups as input grouped_epi_archive", {
  ea2 <- ea2_data %>%
    as_epi_archive()
  ea2 <- ea2 %>% group_by(geo_value)

  ea_expected <- ea2

  ea_as_of <- ea2 %>%
    epix_truncate_versions_after(max_version = as.Date("2020-06-04"))
  expect_equal(ea_as_of %>% groups(), ea_expected %>% groups())
})
