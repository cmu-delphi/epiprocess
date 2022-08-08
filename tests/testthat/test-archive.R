library(dplyr)

test_that("first input must be a data.frame",{
  expect_error(as_epi_archive(c(1,2,3)),
               regexp="`x` must be a data frame.")
})

dt <- archive_cases_dv_subset$DT

test_that("data.frame must contain geo_value, time_value and version columns",{
  expect_error(as_epi_archive(select(dt,-geo_value)),
               regexp="`x` must contain a `geo_value` column.")
  expect_error(as_epi_archive(select(dt,-time_value)),
               regexp="`x` must contain a `time_value` column.")
  expect_error(as_epi_archive(select(dt,-version)),
               regexp="`x` must contain a `version` column.")
})

test_that("other_keys can only contain names of the data.frame columns",{
  expect_error(as_epi_archive(dt,other_keys = "xyz"),
               regexp="`other_keys` must be contained in the column names of `x`.")
  expect_error(as_epi_archive(dt,other_keys = "percent_cli"),NA)
})

test_that("other_keys cannot contain names geo_value, time_value or version",{
  expect_error(as_epi_archive(dt,other_keys = "geo_value"),
               regexp="`other_keys` cannot contain \"geo_value\", \"time_value\", or \"version\".")
  expect_error(as_epi_archive(dt,other_keys = "time_value"),
               regexp="`other_keys` cannot contain \"geo_value\", \"time_value\", or \"version\".")
  expect_error(as_epi_archive(dt,other_keys = "version"),
               regexp="`other_keys` cannot contain \"geo_value\", \"time_value\", or \"version\".")
})

test_that("Warning thrown when other_metadata contains overlapping names with geo_type or time_type fields",{
  expect_warning(as_epi_archive(dt,additional_metadata = list(geo_type = 1)),
                 regexp="`additional_metadata` names overlap with existing metadata fields\n\"geo_type\", \"time_type\".")
  expect_warning(as_epi_archive(dt,additional_metadata = list(time_type = 1)),
                 regexp="`additional_metadata` names overlap with existing metadata fields\n\"geo_type\", \"time_type\".")
})

test_that("epi_archives are correctly instantiated with a variety of data types",{
  # Data frame
  df <- data.frame(geo_value="ca",
                   time_value=as.Date("2020-01-01"),
                   version = as.Date("2020-01-01") + 0:19,
                   value=1:20)
  
  ea1 <- as_epi_archive(df)
  expect_equal(key(ea1$DT),c("geo_value","time_value","version"))
  expect_equal(ea1$additional_metadata,list())
  
  ea2 <- as_epi_archive(df,other_keys="value",additional_metadata=list(value=df$value))
  expect_equal(key(ea2$DT),c("geo_value","time_value","value","version"))
  expect_equal(ea2$additional_metadata,list(value=df$value))
  
  # Tibble
  tib <- tibble::tibble(df, code="x")
  
  ea3 <- as_epi_archive(tib)
  expect_equal(key(ea3$DT),c("geo_value","time_value","version"))
  expect_equal(ea3$additional_metadata,list())
  
  ea4 <- as_epi_archive(tib,other_keys="code",additional_metadata=list(value=df$value))
  expect_equal(key(ea4$DT),c("geo_value","time_value","code","version"))
  expect_equal(ea4$additional_metadata,list(value=df$value))
  
  # Keyed data.table
  kdt <- data.table::data.table(geo_value="ca",
                                time_value=as.Date("2020-01-01"),
                                version = as.Date("2020-01-01") + 0:19,
                                value = 1:20,
                                code = "CA",
                                key = "code")
  
  ea5 <- as_epi_archive(kdt)
  expect_equal(key(ea5$DT),c("geo_value","time_value","version")) # Key from data.table isn't absorbed
  expect_equal(ea5$additional_metadata,list())
  
  ea6 <- as_epi_archive(kdt,other_keys="code",additional_metadata=list(value=df$value))
  expect_equal(key(ea6$DT),c("geo_value","time_value","code","version"))
  expect_equal(ea6$additional_metadata,list(value=df$value))
  
  # Unkeyed data.table
  udt <- data.table::data.table(geo_value="ca",
                                time_value=as.Date("2020-01-01"),
                                version = as.Date("2020-01-01") + 0:19,
                                value=1:20,
                                code = "CA")
  
  ea7 <- as_epi_archive(udt)
  expect_equal(key(ea7$DT),c("geo_value","time_value","version"))
  expect_equal(ea7$additional_metadata,list())
  
  ea8 <- as_epi_archive(udt,other_keys="code",additional_metadata=list(value=df$value))
  expect_equal(key(ea8$DT),c("geo_value","time_value","code","version"))
  expect_equal(ea8$additional_metadata,list(value=df$value))
  
  #epi_df
  edf <- jhu_csse_daily_subset %>%
    select(geo_value,time_value,cases) %>%
    mutate(version = max(time_value), code = "USA")
  
  ea9 <- as_epi_archive(edf)
  expect_equal(key(ea9$DT),c("geo_value","time_value","version"))
  expect_equal(ea9$additional_metadata,list())
  
  ea10 <- as_epi_archive(edf,other_keys="code",additional_metadata=list(value=df$value))
  expect_equal(key(ea10$DT),c("geo_value","time_value","code","version"))
  expect_equal(ea10$additional_metadata,list(value=df$value))
})