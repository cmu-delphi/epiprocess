test_that("epix_slide group_by= deprecation works", {
  expect_error(
    archive_cases_dv_subset %>%
      epix_slide(function(...) {}, before = 2L, group_by = c()),
    class = "epiprocess__epix_slide_group_by_parameter_deprecated"
  )
  expect_error(
    archive_cases_dv_subset %>%
      slide(function(...) {}, before = 2L, group_by = c()),
    class = "epiprocess__epix_slide_group_by_parameter_deprecated"
  )
  expect_error(
    archive_cases_dv_subset %>%
      group_by(geo_value) %>%
      epix_slide(function(...) {}, before = 2L, group_by = c()),
    class = "epiprocess__epix_slide_group_by_parameter_deprecated"
  )
  expect_error(
    archive_cases_dv_subset %>%
      group_by(geo_value) %>%
      slide(function(...) {}, before = 2L, group_by = c()),
    class = "epiprocess__epix_slide_group_by_parameter_deprecated"
  )
  #
  expect_error(
    archive_cases_dv_subset %>%
      epix_slide(function(...) {}, before = 2L, all_rows = TRUE),
    class = "epiprocess__epix_slide_all_rows_parameter_deprecated"
  )
  expect_error(
    archive_cases_dv_subset %>%
      slide(function(...) {}, before = 2L, all_rows = TRUE),
    class = "epiprocess__epix_slide_all_rows_parameter_deprecated"
  )
  expect_error(
    archive_cases_dv_subset %>%
      group_by(geo_value) %>%
      epix_slide(function(...) {}, before = 2L, all_rows = TRUE),
    class = "epiprocess__epix_slide_all_rows_parameter_deprecated"
  )
  expect_error(
    archive_cases_dv_subset %>%
      group_by(geo_value) %>%
      slide(function(...) {}, before = 2L, all_rows = TRUE),
    class = "epiprocess__epix_slide_all_rows_parameter_deprecated"
  )
})
