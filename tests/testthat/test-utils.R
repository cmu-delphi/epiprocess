test_that("guess_geo_type tests for different types of geo_value's",{
  
  # California, New York
  states <- c("ca","ny")
  
  # Canada, USA, United Kingdom
  nations <- c("ca","us","uk")
  
  # These are just five-number names that may not necessarily be existent
  # counties
  counties <- c("12345","67890")
  
  # HHS regions
  hhs <- c(1:3)
  
  # HRR regions
  hrr <- c(100,200)
  
  # Health regions in British Columbia
  bc <- c("Vancouver Coastal","Interior","Fraser",
              "Northern","Vancouver Island")
  
  expect_equal(guess_geo_type(states),"state")
  expect_equal(guess_geo_type(nations),"nation")
  expect_equal(guess_geo_type(counties),"county")
  expect_equal(guess_geo_type(hhs),"hhs")
  expect_equal(guess_geo_type(hrr),"hrr")
  expect_equal(guess_geo_type(bc),"custom") #THIS TEST IS FAILING!!!
  
})