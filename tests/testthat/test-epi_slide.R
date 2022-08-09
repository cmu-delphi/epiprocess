## Create an epi. df and a function to test epi_slide with

edf = dplyr::bind_rows(
  dplyr::tibble(geo_value = "ak", time_value = as.Date("2020-01-01") + 1:200, value=1:200),
  dplyr::tibble(geo_value = "al", time_value=as.Date("2020-01-01") + 1:5, value=-(1:5))
) %>%
  as_epi_df() %>%
  group_by(geo_value)

f = function(x, ...) dplyr::tibble(value=mean(x$value), count=length(x$value))

## --- These cases generate the error: ---
test_that("`after` must be defined as a non-zero integer if `before` is missing", {
  expect_error(epi_slide(edf, f, after = 0L, ref_time_values=as.Date("2020-01-01")),
               "`before` cannot be missing when `after` is set to 0.")
})

test_that("Warn user against having a blank `before`",{
  expect_warning(epi_slide(edf, f, after = 1L, ref_time_values=as.Date("2020-01-01")+1L),
                 regexp="`before` is missing, but `after` is nonzero. `before` has been set to\n0.")
})

test_that("Both `before` and `after` must be nonnegative integers",{
  expect_error(epi_slide(edf, f, before = -1L, ref_time_values=as.Date("2020-01-01")+2L),
               "`before` and `after` must be at least 0.")
  expect_error(epi_slide(edf, f, before = 2L, after = -1L, ref_time_values=as.Date("2020-01-01")+2L),
               "`before` and `after` must be at least 0.")
  expect_error(epi_slide(edf, f, before = 0.5, ref_time_values=as.Date("2020-01-01")+2L),
               "`before` and `after` must be integers.")
  expect_error(epi_slide(edf, f, before = 1, after = 0.5, ref_time_values=as.Date("2020-01-01")+2L),
               "`before` and `after` must be integers.")
  # The before and after values can be numerics that are integerish
  expect_error(epi_slide(edf, f, before = 1, after = 1, ref_time_values=as.Date("2020-01-01")+2L),NA)
})

test_that("`ref_time_values` + `align` that result in no slide data, generate the error", {
  expect_error(epi_slide(edf, f, before=2L, ref_time_values=as.Date("2020-01-01")), 
               "starting and/or stopping times for sliding are out of bounds") # before the first, no data in the slide windows
  expect_error(epi_slide(edf, f, before=2L, ref_time_values=as.Date("2020-01-01")+207L), 
               "starting and/or stopping times for sliding are out of bounds") # beyond the last, no data in window
})

test_that("`ref_time_values` + `align` that have some slide data, but generate the error due to ref. time being out of time range", {
  expect_error(epi_slide(edf, f, before=0L, after=2L, ref_time_values=as.Date("2020-01-01")), 
               "starting and/or stopping times for sliding are out of bounds") # before the first, but we'd expect there to be data in the window
  expect_error(epi_slide(edf, f, before=2L, ref_time_values=as.Date("2020-01-01")+201L), 
               "starting and/or stopping times for sliding are out of bounds") # beyond the last, but still with data in window
})

## --- These cases doesn't generate the error: ---
test_that("these doesn't produce an error; the error appears only if the ref time values are out of the range for every group", {
  expect_identical(epi_slide(edf, f, before=2L, ref_time_values=as.Date("2020-01-01")+200L) %>% 
                     dplyr::select("geo_value","slide_value_value"), 
                   dplyr::tibble(geo_value = "ak", slide_value_value = 199)) # out of range for one group
  expect_identical(epi_slide(edf, f, before=2L, ref_time_values=as.Date("2020-01-04")) %>% 
                     dplyr::select("geo_value","slide_value_value"), 
                   dplyr::tibble(geo_value = c("ak", "al"), slide_value_value = c(2, -2))) # not out of range for either group
})