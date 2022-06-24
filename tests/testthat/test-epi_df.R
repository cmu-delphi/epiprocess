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


