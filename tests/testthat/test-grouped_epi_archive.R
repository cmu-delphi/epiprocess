
test_that("Grouping, regrouping, and ungrouping archives works as intended", {
  # From an example:
  library(dplyr)
  toy_archive =
    tribble(
      ~geo_value,  ~age_group,  ~time_value,     ~version, ~value,
            "us",     "adult", "2000-01-01", "2000-01-02",    121,
            "us", "pediatric", "2000-01-02", "2000-01-03",      5, # (addition)
            "us",     "adult", "2000-01-01", "2000-01-03",    125, # (revision)
            "us",     "adult", "2000-01-02", "2000-01-03",    130  # (addition)
    ) %>%
    mutate(age_group = ordered(age_group, c("pediatric", "adult")),
           time_value = as.Date(time_value),
           version = as.Date(version)) %>%
    as_epi_archive(other_keys = "age_group")

  # Ensure that we're using testthat edition 3's idea of "identical", which is
  # not as strict as `identical`:
  testthat::local_edition(3)

  # Test equivalency claims in example:
  by_both_keys = toy_archive %>% group_by(geo_value, age_group)
  expect_identical(
    by_both_keys,
    toy_archive %>% group_by(geo_value) %>% group_by(age_group, .add=TRUE)
  )
  grouping_cols = c("geo_value", "age_group")
  expect_identical(
    by_both_keys,
    toy_archive %>% group_by(across(all_of(grouping_cols)))
  )

  expect_identical(
    toy_archive %>% group_by(geo_value),
    toy_archive %>% group_by(geo_value, age_group) %>% ungroup(age_group)
  )

  expect_identical(
    toy_archive %>%
      group_by(geo_value, age_group, .drop=FALSE) %>%
      epix_slide(f = ~ sum(.x$value), before = 20) %>%
      ungroup(),
    toy_archive %>%
      group_by(geo_value, age_group, .drop=TRUE) %>%
      epix_slide(f = ~ sum(.x$value), before = 20) %>%
      ungroup()
  )
})
