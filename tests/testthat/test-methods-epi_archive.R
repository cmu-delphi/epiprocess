ea <- archive_cases_dv_subset

test_that("Warning against max_version being same as edf's max version",{
  expect_warning(ea$as_of(max_version = max(ea$DT$version)))
  expect_warning(ea$as_of(max_version = min(ea$DT$version)),NA)
})

