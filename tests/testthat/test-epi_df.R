test_that("new_epi_df works as intended", {
  # Empty tibble
  wmsg <- capture_warnings(a <- new_epi_df())
  expect_match(
    wmsg[1],
    "Unknown or uninitialised column: `geo_value`."
  )
  expect_match(
    wmsg[2],
    "Unknown or uninitialised column: `time_value`."
  )
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

  epi_tib <- new_epi_df(tib)
  expect_true(is_epi_df(epi_tib))
  expect_length(epi_tib, 4L)
  expect_identical(attributes(epi_tib)$metadata$geo_type, "state")
  expect_identical(attributes(epi_tib)$metadata$time_type, "day")
  expect_true(lubridate::is.POSIXt(attributes(epi_tib)$metadata$as_of))
})

test_that("as_epi_df errors when additional_metadata is not a list", {
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
    "`additional_metadata` must be a list type."
  )
})

# select fixes

tib <- tibble::tibble(
  x = 1:10, y = 1:10,
  time_value = rep(seq(as.Date("2020-01-01"),
    by = 1, length.out = 5
  ), times = 2),
  geo_value = rep(c("ca", "hi"), each = 5)
)
epi_tib <- epiprocess::new_epi_df(tib)
test_that("grouped epi_df maintains type for select", {
  grouped_epi <- epi_tib %>% group_by(geo_value)
  selected_df <- grouped_epi %>% select(-y)
  expect_true("epi_df" %in% class(selected_df))
  # make sure that the attributes are right
  epi_attr <- attributes(selected_df)
  expect_identical(epi_attr$names, c("geo_value", "time_value", "x"))
  expect_identical(epi_attr$row.names, seq(1, 10))
  expect_identical(epi_attr$groups, attributes(grouped_epi)$groups)
  expect_identical(epi_attr$metadata, attributes(epi_tib)$metadata)
})

test_that("grouped epi_df drops type when dropping keys", {
  grouped_epi <- epi_tib %>% group_by(geo_value)
  selected_df <- grouped_epi %>% select(geo_value)
  expect_true(!("epi_df" %in% class(selected_df)))
})

test_that("grouped epi_df handles extra keys correctly", {
  tib <- tibble::tibble(
    x = 1:10, y = 1:10,
    time_value = rep(seq(as.Date("2020-01-01"),
      by = 1, length.out = 5
    ), times = 2),
    geo_value = rep(c("ca", "hi"), each = 5),
    extra_key = rep(seq(as.Date("2020-01-01"),
      by = 1, length.out = 5
    ), times = 2)
  )
  epi_tib <- epiprocess::new_epi_df(tib,
    additional_metadata = list(other_keys = "extra_key")
  )
  attributes(epi_tib)
  grouped_epi <- epi_tib %>% group_by(geo_value)
  selected_df <- grouped_epi %>% select(-extra_key)
  selected_df
  expect_true("epi_df" %in% class(selected_df))
  # make sure that the attributes are right
  old_attr <- attributes(epi_tib)
  epi_attr <- attributes(selected_df)
  expect_identical(epi_attr$names, c("geo_value", "time_value", "x", "y"))
  expect_identical(epi_attr$row.names, seq(1, 10))
  expect_identical(epi_attr$groups, attributes(grouped_epi)$groups)
  expect_identical(epi_attr$metadata, list(
    geo_type = "state", time_type =
      "day",
    as_of = old_attr$metadata$as_of,
    other_keys = character(0)
  ))
})
