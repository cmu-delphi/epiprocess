test_that("new_epi_df works as intended", {
  # Empty call fails
  expect_error(new_epi_df(), "argument \"geo_type\" is missing")
  # Empty tibble works, but requires metadata
  a <- new_epi_df(tibble(), geo_type = "custom", time_type = "custom", as_of = as.POSIXct("2020-01-01"))
  expect_true(is_epi_df(a))
  expect_identical(attributes(a)$metadata$geo_type, "custom")
  expect_identical(attributes(a)$metadata$time_type, "custom")
  expect_true(lubridate::is.POSIXt(attributes(a)$metadata$as_of))

  # Simple non-empty tibble with geo_value and time_value cols
  tib <- tibble::tibble(
    x = 1:10, y = 1:10,
    time_value = rep(seq(as.Date("2020-01-01"), by = 1, length.out = 5), times = 2),
    geo_value = rep(c("ca", "hi"), each = 5)
  )

  epi_tib <- new_epi_df(tib, geo_type = "state", time_type = "day", as_of = as.POSIXct("2020-01-01"))
  expect_true(is_epi_df(epi_tib))
  expect_length(epi_tib, 4L)
  expect_identical(attributes(epi_tib)$metadata$geo_type, "state")
  expect_identical(attributes(epi_tib)$metadata$time_type, "day")
  expect_true(lubridate::is.POSIXt(attributes(epi_tib)$metadata$as_of))
})

test_that("as_epi_df errors when additional_metadata is not a list", {
  skip("additional_metadata is no longer an argument, not tested")
  # This is the 3rd example from as_epi_df
  ex_input <- jhu_csse_county_level_subset %>%
    dplyr::filter(time_value > "2021-12-01", state_name == "Massachusetts") %>%
    dplyr::slice_tail(n = 6) %>%
    tsibble::as_tsibble() %>%
    dplyr::mutate(
      state = rep("MA", 6),
      pol = rep(c("blue", "swing", "swing"), each = 2)
    )

  expect_error(
    as_epi_df(ex_input, additional_metadata = c(other_keys = "state", "pol")),
    "Must be of type 'list', not 'character'."
  )
})

test_that("as_epi_df errors for non-character other_keys", {
  ex_input <- jhu_csse_county_level_subset %>%
    dplyr::filter(time_value > "2021-12-01", state_name == "Massachusetts") %>%
    dplyr::slice_tail(n = 6) %>%
    tsibble::as_tsibble() %>%
    dplyr::mutate(
      state = rep("MA", 6),
      pol = rep(c("blue", "swing", "swing"), each = 2)
    )

  expect_error(
    as_epi_df(ex_input, other_keys = list()),
    "Must be of type 'character'"
  )
  expect_silent(as_epi_df(ex_input, other_keys = c("state", "pol")))
})

test_that("as_epi_df works for nonstandard input", {
  tib <- tibble::tibble(
    x = 1:10, y = 1:10,
    date = rep(seq(as.Date("2020-01-01"), by = 1, length.out = 5), times = 2),
    geo_value = rep(c("ca", "hi"), each = 5)
  )
  expect_message(expect_no_error(tib_epi_df <- tib %>% as_epi_df()),
    class = "epiprocess__guess_column_inferring_inform"
  )
  expect_no_error(tib_epi_df <- tib %>% as_epi_df(time_value = date, geo_value = geo_value))
  expect_error(
    expect_message(
      tib %>%
        rename(awefa = geo_value) %>%
        as_epi_df(),
      class = "epiprocess__guess_column_inferring_inform"
    ),
    class = "epiprocess__guess_column__multiple_substitution_error"
  )
  expect_no_error(expect_message(
    tib %>% rename(awefa = geo_value) %>% as_epi_df(geo_value = awefa),
    class = "epiprocess__guess_column_inferring_inform"
  ))

  tib <- tib %>% rename(target_date = date)
  expect_message(expect_no_error(tib_epi_df <- tib %>% as_epi_df()),
    class = "epiprocess__guess_column_inferring_inform"
  )

  tib <- tib %>% mutate(Time = 20 + target_date)
  expect_error(tib_epi_df <- tib %>% as_epi_df(),
    class = "epiprocess__guess_column__multiple_substitution_error"
  )
})

# select fixes
tib <- tibble::tibble(
  x = 1:10, y = 1:10,
  time_value = rep(seq(as.Date("2020-01-01"), by = 1, length.out = 5), times = 2),
  geo_value = rep(c("ca", "hi"), each = 5)
)
epi_tib <- as_epi_df(tib)
test_that("grouped epi_df maintains type for select", {
  grouped_epi <- epi_tib %>% group_by(geo_value)
  selected_df <- grouped_epi %>% select(-y)
  expect_true(inherits(selected_df, "epi_df"))
  # make sure that the attributes are right
  epi_attr <- attributes(selected_df)
  expect_identical(epi_attr$names, c("geo_value", "time_value", "x"))
  expect_identical(epi_attr$row.names, seq(1, 10))
  expect_identical(epi_attr$groups, attributes(grouped_epi)$groups)
  expect_identical(epi_attr$metadata, attributes(epi_tib)$metadata)
  expect_identical(selected_df, epi_tib %>% select(-y) %>% group_by(geo_value))
})

test_that("grouped epi_df drops type when dropping keys", {
  grouped_epi <- epi_tib %>% group_by(geo_value)
  selected_df <- grouped_epi %>% select(geo_value)
  expect_true(!inherits(selected_df, "epi_df"))
})

test_that("grouped epi_df handles extra keys correctly", {
  tib <- tibble::tibble(
    x = 1:10, y = 1:10,
    time_value = rep(seq(as.Date("2020-01-01"), by = 1, length.out = 5), times = 2),
    geo_value = rep(c("ca", "hi"), each = 5),
    extra_key = rep(seq(as.Date("2020-01-01"), by = 1, length.out = 5), times = 2)
  )
  epi_tib <- as_epi_df(tib, other_keys = "extra_key")
  grouped_epi <- epi_tib %>% group_by(geo_value)
  selected_df <- grouped_epi %>% select(-extra_key)
  expect_true(inherits(selected_df, "epi_df"))
  # make sure that the attributes are right
  old_attr <- attributes(epi_tib)
  epi_attr <- attributes(selected_df)
  expect_identical(epi_attr$names, c("geo_value", "time_value", "x", "y"))
  expect_identical(epi_attr$row.names, seq(1, 10))
  expect_identical(epi_attr$groups, attributes(grouped_epi)$groups)
  expect_identical(epi_attr$metadata, list(
    geo_type = "state", time_type = "day",
    as_of = old_attr$metadata$as_of,
    other_keys = character(0)
  ))
})
