test_that("Grouping, regrouping, and ungrouping archives works as intended", {
  # From an example:
  suppressPackageStartupMessages(library(dplyr))
  toy_archive <-
    tribble(
      ~geo_value, ~age_group, ~time_value, ~version, ~value,
      "us", "adult", "2000-01-01", "2000-01-02", 121,
      "us", "pediatric", "2000-01-02", "2000-01-03", 5, # (addition)
      "us", "adult", "2000-01-01", "2000-01-03", 125, # (revision)
      "us", "adult", "2000-01-02", "2000-01-03", 130 # (addition)
    ) %>%
    mutate(
      age_group = ordered(age_group, c("pediatric", "adult")),
      time_value = as.Date(time_value),
      version = as.Date(version)
    ) %>%
    as_epi_archive(other_keys = "age_group")

  # Test equivalency claims in example:
  by_both_keys <- toy_archive %>% group_by(geo_value, age_group)
  expect_identical(
    by_both_keys,
    toy_archive %>% group_by(geo_value) %>% group_by(age_group, .add = TRUE)
  )
  grouping_cols <- c("geo_value", "age_group")
  expect_identical(
    by_both_keys,
    toy_archive %>% group_by(across(all_of(grouping_cols)))
  )

  expect_identical(
    toy_archive %>% group_by(geo_value),
    toy_archive %>% group_by(geo_value, age_group) %>% ungroup(age_group)
  )

  # Test `.drop` behavior:
  expect_error(toy_archive %>% group_by(.drop = "bogus"),
    regexp = "Must be of type 'logical', not 'character'"
  )
  expect_no_warning(toy_archive %>% group_by(.drop = FALSE))
  expect_warning(toy_archive %>% group_by(geo_value, .drop = FALSE),
    class = "epiprocess__group_by_epi_archive__drop_FALSE_no_factors"
  )
  expect_warning(
    grouped_factor_then_nonfactor <-
      toy_archive %>% group_by(age_group, geo_value, .drop = FALSE),
    class = "epiprocess__group_by_epi_archive__drop_FALSE_nonfactor_after_factor"
  )
  expect_identical(
    grouped_factor_then_nonfactor %>%
      epix_slide(.before = 10, s = sum(value)),
    tibble::tribble(
      ~age_group, ~geo_value, ~version, ~s,
      "pediatric", NA_character_, "2000-01-02", 0,
      "adult", "us", "2000-01-02", 121,
      "pediatric", "us", "2000-01-03", 5,
      "adult", "us", "2000-01-03", 255
    ) %>%
      mutate(
        age_group = ordered(age_group, c("pediatric", "adult")),
        version = as.Date(version)
      ) %>%
      group_by(age_group, geo_value, .drop = FALSE)
  )
  expect_identical(
    toy_archive %>%
      group_by(geo_value, age_group, .drop = FALSE) %>%
      epix_slide(.before = 10, s = sum(value)),
    tibble::tribble(
      ~geo_value, ~age_group, ~version, ~s,
      "us", "pediatric", "2000-01-02", 0,
      "us", "adult", "2000-01-02", 121,
      "us", "pediatric", "2000-01-03", 5,
      "us", "adult", "2000-01-03", 255
    ) %>%
      mutate(
        age_group = ordered(age_group, c("pediatric", "adult")),
        version = as.Date(version)
      ) %>%
      # as_epi_df(as_of = as.Date("2000-01-03"),
      #           other_keys = "age_group") %>%
      # # put back in expected order; see issue #166:
      # select(geo_value, age_group, time_value, s) %>%
      group_by(geo_value, age_group, .drop = FALSE)
  )
})
