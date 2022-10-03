library(dplyr)

test_that("epix_slide only works on an epi_archive",{
  expect_error(epix_slide(data.frame(x=1)))
})

x <- tibble::tribble(~version, ~time_value, ~binary,
                     4,       c(1:3),       2^(1:3),
                     5,       c(1:2,4),     2^(4:6),
                     6,       c(1:2,4:5),   2^(7:10),
                     7,       2:6,          2^(11:15)) %>%
  tidyr::unnest(c(time_value,binary))

xx <- bind_cols(geo_value = rep("x",15), x) %>%
  as_epi_archive()

test_that("epix_slide works as intended",{
  xx1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(f = ~ sum(.x$binary),
               before = 2,
               new_col_name = "sum_binary")
  
  xx2 <- tibble(geo_value = rep("x",3),
                # 7 should also be there below; this is a bug on issue #153
                time_value = c(4,5,6),
                sum_binary = c(2^3+2^2,
                               2^6+2^3,
                               2^10+2^9)) %>%
    as_epi_df(as_of = 1) # Also a bug (issue #213)
  
  expect_identical(xx1,xx2) # *
  
  xx3 <- (
    xx
    $group_by(dplyr::across(dplyr::all_of("geo_value")))
    $slide(f = ~ sum(.x$binary),
           before = 2,
           new_col_name = 'sum_binary')
  )
  
  expect_identical(xx1,xx3) # This and * Imply xx2 and xx3 are identical
})

test_that("epix_slide `before` validation works", {
  expect_error(xx$slide(f = ~ sum(.x$binary)),
               "`before` is required")
  expect_error(xx$slide(f = ~ sum(.x$binary), before=NA),
               "`before`.*NA")
  expect_error(xx$slide(f = ~ sum(.x$binary), before=-1),
               "`before`.*negative")
  expect_error(xx$slide(f = ~ sum(.x$binary), before=1.5),
               regexp="before",
               class="vctrs_error_incompatible_type")
  # We might want to allow this at some point (issue #219):
  expect_error(xx$slide(f = ~ sum(.x$binary), before=Inf),
               regexp="before",
               class="vctrs_error_incompatible_type")
  # (wrapper shouldn't introduce a value:)
  expect_error(epix_slide(xx, f = ~ sum(.x$binary)), "`before` is required")
  # These `before` values should be accepted:
  expect_error(xx$slide(f = ~ sum(.x$binary), before=0),
               NA)
  expect_error(xx$slide(f = ~ sum(.x$binary), before=2L),
               NA)
  expect_error(xx$slide(f = ~ sum(.x$binary), before=365000),
               NA)
})
