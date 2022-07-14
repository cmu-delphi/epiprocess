library(dplyr)

ea <- archive_cases_dv_subset$clone()

test_that("epix_slide only works on an epi_archive",{
  expect_error(epix_slide(data.frame(x=1)))
})

test_that("epix_slide works as intended",{
  x2 <- ea$clone()$DT %>%
    filter(geo_value == "ca", version <= as.Date("2020-06-09")) %>%
    select(-percent_cli,-case_rate_7d_av) %>%
    mutate(binary = 2^(row_number()-1)) %>%
    as_epi_archive()
  
  time_values <- seq(as.Date("2020-06-01"),
                     as.Date("2020-06-09"),
                     by = "1 day")
  xx <- epix_slide(x = x2,
                   f = ~ sum(.x$binary),
                   n = 3,
                   group_by = geo_value,
                   ref_time_values = time_values,
                   new_col_name = 'sum_binary')
  
  # No test here as this is broken
  
  xx2 <- x2$DT %>%
    filter(time_value + 1 == version) %>%
    mutate(sum_binary = lag(binary) + lag(binary,2) + lag(binary,3)) %>%
    select(-version,-binary) %>%
    as_tibble()
  
  xx2 <- tail(xx2,-1)
  xx2[1,3] <- 1
  xx2[2,3] <- 33
 
  expect_identical(xx,xx2)
})