test_that("check that dataset contains geo_value",{
  expect_error(
    as_epi_df(data.frame(x=1))
  )
})

test_that("check that dataset contains time_value",{
  expect_error(
    as_epi_df(data.frame(geo_value=1))
  )
})

test_that("tsibble isn't preserved",{
  df <- data.frame(geo_value = "ca",time_value = as.Date("2020-01-01"),cases=1)
  my_tsibble <- as_tsibble(df,index=time_value)
  my_epi_df <- as_epi_df(my_tsibble)
  expect_false(inherits(my_epi_df,"tbl_ts"))
})

test_that("is_epi_df works as it should",{
  expect_false(is_epi_df(data.frame(x=1)))
})
