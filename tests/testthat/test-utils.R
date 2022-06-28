test_that("break_string works properly",{
  expect_equal(break_str("A dog is here", 6),"A dog\nis\nhere")
})

test_that("Abort and Warn work",{
  expect_error(Abort("abort"))
  expect_warning(Warn("warn"))
})

test_that("in_range works",{
  expect_equal(in_range(1,c(2,4)),2)
  expect_equal(in_range(3,c(2,4)),3)
  expect_equal(in_range(5,c(2,4)),4)
})

test_that("new summarizing functions work",{
  x <- c(3,4,5,9,NA)
  expect_equal(Min(x),3)
  expect_equal(Max(x),9)
  expect_equal(Sum(x),21)
  expect_equal(Mean(x),5.25)
  expect_equal(Median(x),4.5)
})

test_that("Other capital letter functions work",{
  x <- c(1,2,3,4,5)
  expect_equal(Start(x),1)
  expect_equal(End(x),5)
  expect_equal(MiddleL(x),2) # Questionable for odd length vectors
  expect_equal(MiddleR(x),3) # Questionable for odd length vectors
  expect_equal(ExtendL(x),c(1,1,2,3,4,5))
  expect_equal(ExtendR(x),c(1,2,3,4,5,5))
})

test_that("guess_geo_type tests for different types of geo_value's",{
  # California, New York
  states <- c("ca","ny")
  
  # Canada, USA, United Kingdom
  nations <- c("ca","us","uk")
  
  # Note: These are just five-number names that may not necessarily be existent
  # counties
  counties <- c("12345","67890")
  
  # HHS regions
  hhs <- c(1:3)
  
  # HRR regions
  hrr <- c(100,200)
  
  # Health regions in British Columbia
  bc <- c("Vancouver Coastal","Interior","Fraser",
              "Northern","Vancouver Island")
  
  # Long numbers as strings that should also be custom
  long_nums <- c("123456789","111222333")
  
  expect_equal(guess_geo_type(states),"state")
  expect_equal(guess_geo_type(nations),"nation")
  expect_equal(guess_geo_type(counties),"county")
  expect_equal(guess_geo_type(hhs),"hhs")
  expect_equal(guess_geo_type(hrr),"hrr")
  # expect_equal(guess_geo_type(bc),"custom") # EDIT LINE 42
  # expect_equal(guess_geo_type(long_nums),"custom") # EDIT LINE 45
})

test_that("guess_time_type works for different types",{
  days <- as.Date("2022-01-01") + 0:6
  weeks <- as.Date("2022-01-01") + 7 * 0:6

  yearweeks <- tsibble::yearweek(10)
  yearmonths <- tsibble::yearmonth(10)
  yearquarters <- tsibble::yearquarter(10)
  
  years <- c(1999,2000)
  
  # YYYY-MM-DD is the accepted format
  not_ymd1 <- "January 1, 2022"
  not_ymd2 <- "1 January 2022"
  not_ymd3 <- "1 Jan 2022"
  
  not_a_date <- "asdf"
  
  expect_equal(guess_time_type(days),"day")
  # expect_equal(guess_time_type(weeks),"week") # EDIT LINE 82

  expect_equal(guess_time_type(yearweeks),"yearweek")  
  expect_equal(guess_time_type(yearmonths),"yearmonth")
  expect_equal(guess_time_type(yearquarters),"yearquarter")
  
  expect_equal(guess_time_type(years),"year")
  
  expect_equal(guess_time_type(not_ymd1),"custom")
  expect_equal(guess_time_type(not_ymd2),"custom")
  expect_equal(guess_time_type(not_ymd3),"custom")
  expect_equal(guess_time_type(not_a_date),"custom")
})

test_that("enlist works",{
  my_list <- enlist(x=1,y=2,z=3)
  expect_equal(my_list$x,1)
  expect_true(inherits(my_list,"list"))
})