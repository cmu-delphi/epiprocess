library(dplyr)

x <- jhu_csse_daily_subset %>%
  select(-case_rate_7d_av,-death_rate_7d_av, -cases_7d_av) %>%
  group_by(geo_value) %>% 
  mutate(cases_gr = growth_rate(x = time_value,  y = cases))

test_that("Test error throwing",{
  expect_error(growth_rate(x=1:3,y=1:4),
               "`x` and `y` must have the same length.")
  expect_error(growth_rate(x=1:20,y=1:20,x0=21),
               "`x0` must be a subset of `x`.")
  expect_error(growth_rate(x=1:20,y=1:20,x0=c(1,3)),NA)
})