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
  
  xx1 <- epix_slide(x = xx,
                    f = ~ sum(.x$binary),
                    before = 3,
                    group_by = geo_value,
                    new_col_name = "sum_binary")
  
  xx2 <- tibble(geo_value = rep("x",2),
                time_value = c(5,6),
                sum_binary = c(2^7,
                               2^10+2^8)) %>%
    as_epi_df(as_of = 1)
  
  expect_identical(xx1,xx2) # *
  
  xx3 <- xx$slide(f = ~ sum(.x$binary),
                  before = 3,
                  group_by = "geo_value",
                  new_col_name = 'sum_binary')
  
  expect_identical(xx1,xx3) # This and * Imply xx2 and xx3 are identical
})
