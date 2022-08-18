library(dplyr)
library(rlang)

test_that("epix_slide only works on an epi_archive",{
  expect_error(epix_slide(data.frame(x=1)))
})

test_that("epix_slide works as intended",{
  x <- tibble::tribble(~version, ~time_value,
                       5, c(1:2,4),
                       6, c(1:2,4:5),
                       7, 2:6) %>%
    tidyr::unnest(time_value)
  
  xx <- bind_cols(geo_value = rep("x",12),
                  arrange(x,time_value,version),
                  binary = 2^(1:12)) %>%
    as_epi_archive()
  
  time_values <- 2:5
  
  xx1 <- epix_slide(x = xx,
                    f = ~ sum(.xx$binary),
                    before = 3,
                    group_by = geo_value,
                    ref_time_values = versions,
                    new_col_name = "sum_binary")
  
  xx2 <- tibble(geo_value = rep("x",5),
                time_value = as.Date("2020-06-01") + 1:5,
                sum_binary = c(3))
  
  expect_identical(xx1,xx2) # *
  
  xx3 <- xx$slide(f = ~ sum(.xx$binary),
                  before = 3,
                  group_by = "geo_value",
                  ref_time_values = time_values,
                  new_col_name = 'sum_binary')
  
  expect_identical(xx1,xx3) # This and * Imply xx2 and xx3 are identical
})
