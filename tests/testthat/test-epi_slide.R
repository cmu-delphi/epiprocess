## Create an epi. df and a function to test epi_slide with

edf = dplyr::bind_rows(
  dplyr::tibble(geo_value = "ak", time_value = as.Date("2020-01-01") + 1:200, value=1:200),
  dplyr::tibble(geo_value = "al", time_value=as.Date("2020-01-01") + 1:5, value=-(1:5))
) %>%
  as_epi_df()

f = function(x, ...) dplyr::tibble(value=mean(x$value), count=length(x$value))

## --- These cases generate the error: ---
test_that("`after` must be defined as a non-zero integer if `before` is missing", {
  expect_error(edf %>% group_by(geo_value) %>% epi_slide(f, after = 0L, ref_time_values=as.Date("2020-01-01")),
               "`before` cannot be missing when `after` is set to 0.")
})

test_that({
  expect_warning(edf %>% group_by(geo_value) %>% epi_slide(f, after = 1L, ref_time_values=as.Date("2020-01-01")+1L),
                 "`before` missing but `after` nonzero; `before` has been set to 0.")
})

test_that("`ref_time_values` + `align` that result in no slide data, generate the error", {
  expect_error(edf %>% group_by(geo_value) %>% epi_slide(f, before=2L, ref_time_values=as.Date("2020-01-01")), 
               "starting and/or stopping times for sliding are out of bounds") # before the first, no data in the slide windows
  expect_error(edf %>% group_by(geo_value) %>% epi_slide(f, before=2L, ref_time_values=as.Date("2020-01-01")+207L), 
               "starting and/or stopping times for sliding are out of bounds") # beyond the last, no data in window
})

test_that("`ref_time_values` + `align` that have some slide data, but generate the error due to ref. time being out of time range", {
  expect_error(edf %>% group_by(geo_value) %>% epi_slide(f, after=2L, ref_time_values=as.Date("2020-01-01")), 
               "starting and/or stopping times for sliding are out of bounds") # before the first, but we'd expect there to be data in the window
  expect_error(edf %>% group_by(geo_value) %>% epi_slide(f, before=2L, ref_time_values=as.Date("2020-01-01")+201L), 
               "starting and/or stopping times for sliding are out of bounds") # beyond the last, but still with data in window
})

## --- These cases doesn't generate the error: ---
test_that("these doesn't produce an error; the error appears only if the ref time values are out of the range for every group", {
  expect_identical(edf %>% group_by(geo_value) %>% epi_slide(f, before=2L, ref_time_values=as.Date("2020-01-01")+200L) %>% 
                     dplyr::select("geo_value","slide_value_value"), 
                   dplyr::tibble(geo_value = "ak", slide_value_value = 199)) # out of range for one group
  expect_identical(edf %>% group_by(geo_value) %>% epi_slide(f, before=2L, ref_time_values=as.Date("2020-01-04")) %>% 
                     dplyr::select("geo_value","slide_value_value"), 
                   dplyr::tibble(geo_value = c("ak", "al"), slide_value_value = c(2, -2))) # not out of range for either group
})