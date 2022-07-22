library(dplyr)

x <- jhu_csse_daily_subset %>%
  select(-case_rate_7d_av,-death_rate_7d_av, -cases_7d_av) %>%
  group_by(geo_value) %>% 
  mutate(cases_gr = growth_rate(x = time_value,  y = cases))

test_that("`x` and `y` must be the same lengths",{
  expect_error(growth_rate(x=1:3,y=1:4))
})

test_that("`x0` must be a subset of `x`",{
  expect_error(growth_rate(x=1:20,y=1:20,x0=21))
  expect_error(growth_rate(x=1:20,y=1:20,x0=c(1,3)),NA)
})