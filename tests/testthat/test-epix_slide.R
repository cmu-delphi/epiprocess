library(dplyr)

ea <- archive_cases_dv_subset$clone()

test_that("epix_slide only works on an epi_archive",{
  expect_error(epix_slide(data.frame(x=1)))
})

test_that("epix_slide works as intended",{
  x2 <- ea$clone()$DT %>%
    filter(geo_value == "ca", version <= as.Date("2020-06-09")) %>%
    select(-percent_cli,-case_rate_7d_av) %>%
    mutate(binary = 2^(row_number())) %>%
    as_epi_archive()
  
  time_values <- seq(as.Date("2020-06-01"),
                     as.Date("2020-06-09"),
                     by = "1 day")
  
  xx1 <- epix_slide(x = x2,
                   f = ~ sum(.x$binary),
                   max_version_gap = 5,
                   group_by = geo_value,
                   ref_time_values = time_values,
                   new_col_name = 'sum_binary')
  
  xx2 <- tibble(geo_value = rep("ca",7),
               version = as.Date("2020-06-01") + 1:7,
               sum_binary = c(2^1,
                              2^6+2^1,
                              2^11+2^6+2^1,
                              2^16+2^11+2^6+2^1,
                              2^19+2^16+2^12+2^7,
                              2^21+2^19+2^16+2^13,
                              2^22+2^21+2^19+2^17))
  
  expect_identical(xx1,xx2)
})
