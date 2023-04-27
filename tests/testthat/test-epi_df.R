test_that("new_epi_df works as intended", {
  # Empty tibble
  wmsg = capture_warnings(a <- new_epi_df())
  expect_match(wmsg[1],
                 "Unknown or uninitialised column: `geo_value`.")
  expect_match(wmsg[2],
                 "Unknown or uninitialised column: `time_value`.")
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
  
  epi_tib = new_epi_df(tib)
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
      state = rep("MA",6),
      pol = rep(c("blue", "swing", "swing"), each = 2))
  
  expect_error(
    as_epi_df(ex_input, additional_metadata = c(other_keys = "state", "pol")), 
    "`additional_metadata` must be a list type.")
})

# test_that("`reclass` `after_classes` works as intended", {
#   edf = jhu_csse_county_level_subset
#   tbl = tibble::as_tibble(edf)
#   grouped_tbl = tbl %>% group_by(geo_value)
#   metadata = attr(jhu_csse_county_level_subset, "metadata")
#   # Simple default behavior:
#   expect_identical(
#     class(reclass(tbl, metadata)),
#     c("epi_df", "tbl_df", "tbl", "data.frame")
#   )
#   # Behavior when we already have "epi_df" class:
#   expect_identical(
#     class(reclass(edf, metadata)),
#     c("epi_df", "tbl_df", "tbl", "data.frame")
#   )
#   some_other_ordering = c("tbl_df", "epi_df", "tbl", "data.frame")
#   expect_identical(
#     class(reclass(`class<-`(edf, some_other_ordering), metadata)),
#     some_other_ordering
#   )
#   # Controlling ordering moving from default to passing nondefault
#   # `before_classes`:
#   expect_identical(
#     class(reclass(grouped_tbl, metadata)),
#     c("epi_df", "grouped_df", "tbl_df", "tbl", "data.frame")
#   )
#   expect_identical(
#     class(reclass(grouped_tbl, metadata, "grouped_df")),
#     c("grouped_df", "epi_df", "tbl_df", "tbl", "data.frame")
#   )
#   expect_identical(
#     class(reclass(tbl, metadata, "grouped_df")),
#     c("epi_df", "tbl_df", "tbl", "data.frame")
#   )
# })
