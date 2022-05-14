## --- These cases generate the error: ---
test_that("`ref_time_values` + `align` that result in no slide data, generate the error", {
  expect_error(edf %>% group_by(geo_value) %>% epi_slide(f, n=3L, ref_time_values=as.Date("2020-01-01")), 
               "starting and/or stopping times for sliding are out of bounds") # before the first, no data in the slide windows
  expect_error(edf %>% group_by(geo_value) %>% epi_slide(f, n=3L, ref_time_values=as.Date("2020-01-01")+207L), 
               "starting and/or stopping times for sliding are out of bounds") # beyond the last, no data in window
})

test_that("`ref_time_values` + `align` that have some slide data, but generate the error due to ref. time being out of time range", {
  expect_error(edf %>% group_by(geo_value) %>% epi_slide(f, n=3L, ref_time_values=as.Date("2020-01-01"), align="left"), 
               "starting and/or stopping times for sliding are out of bounds") # before the first, but we'd expect there to be data in the window
  expect_error(edf %>% group_by(geo_value) %>% epi_slide(f, n=3L, ref_time_values=as.Date("2020-01-01")+201L), "starting and/or stopping times for sliding are out of bounds") # beyond the last, but still with data in window
})

## --- These cases doesn't generate the error: ---
test_that("these doesn't produce an error; the error appears only if the ref time values are out of the range for every group", {
  expect_identical(edf %>% group_by(geo_value) %>% epi_slide(f, n=3L, ref_time_values=as.Date("2020-01-01")+200L) %>% dplyr::select("geo_value","slide_value_value"), 
                   dplyr::tibble(geo_value = "ak", slide_value_value = 199) %>% group_by(geo_value)) # out of range for one group
  expect_identical(edf %>% group_by(geo_value) %>% epi_slide(f, n=3L, ref_time_values=as.Date("2020-01-04")) %>% dplyr::select("geo_value","slide_value_value"), 
                   dplyr::tibble(geo_value = c("ak", "al"), slide_value_value = c(2, -2)) %>% group_by(geo_value)) # not out of range for either group
  
  
})