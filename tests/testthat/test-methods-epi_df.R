toy_epi_df <- tibble::tibble(
  x = 1:10,
  y = 1:10,
  time_value = rep(seq(
    as.Date("2020-01-01"),
    by = 1,
    length.out = 5
  ), times = 2),
  geo_value = rep(c("ca", "hi"), each = 5),
  indic_var1 = as.factor(rep(1:2, times = 5)),
  indic_var2 = as.factor(rep(letters[1:5], times = 2))
) %>% as_epi_df(
  other_keys = c("indic_var1", "indic_var2")
)

att_toy <- attr(toy_epi_df, "metadata")

test_that("Head and tail do not drop the epi_df class", {
  att_head <- attr(head(toy_epi_df), "metadata")
  att_tail <- attr(tail(toy_epi_df), "metadata")

  expect_true(is_epi_df(head(toy_epi_df)))
  expect_true(is_epi_df(tail(toy_epi_df)))
  expect_identical(att_head$geo_type, att_toy$geo_type)
  expect_identical(att_head$time_type, att_toy$time_type)
  expect_identical(att_head$as_of, att_toy$as_of)
  expect_identical(att_head$other_keys, att_toy$other_keys)
  expect_identical(att_tail$geo_type, att_toy$geo_type)
  expect_identical(att_tail$time_type, att_toy$time_type)
  expect_identical(att_tail$as_of, att_toy$as_of)
  expect_identical(att_tail$other_keys, att_toy$other_keys)
})


test_that("Subsetting drops & does not drop the epi_df class appropriately", {
  # Row subset - should be epi_df
  row_subset <- toy_epi_df[1:2, ]
  att_row_subset <- attr(row_subset, "metadata")

  expect_true(is_epi_df(row_subset))
  expect_equal(nrow(row_subset), 2L)
  expect_equal(ncol(row_subset), 6L)
  expect_identical(att_row_subset$geo_type, att_toy$geo_type)
  expect_identical(att_row_subset$time_type, att_toy$time_type)
  expect_identical(att_row_subset$as_of, att_toy$as_of)
  expect_identical(att_row_subset$other_keys, att_toy$other_keys)

  # Row and col single value - shouldn't be an epi_df
  row_col_subset1 <- toy_epi_df[1, 2]
  expect_false(is_epi_df(row_col_subset1))
  expect_true(tibble::is_tibble(row_col_subset1))
  expect_equal(nrow(row_col_subset1), 1L)
  expect_equal(ncol(row_col_subset1), 1L)

  # Col subset with no time_value - shouldn't be an epi_df
  col_subset1 <- toy_epi_df[, c(1, 3)]

  expect_false(is_epi_df(col_subset1))
  expect_true(tibble::is_tibble(col_subset1))
  expect_equal(nrow(col_subset1), 10L)
  expect_equal(ncol(col_subset1), 2L)

  # Col subset with no geo_value - shouldn't be an epi_df
  col_subset2 <- toy_epi_df[, 2:3]

  expect_false(is_epi_df(col_subset2))
  expect_true(tibble::is_tibble(col_subset2))
  expect_equal(nrow(col_subset2), 10L)
  expect_equal(ncol(col_subset2), 2L)

  # Row and col subset that contains geo_value and time_value - should be epi_df
  row_col_subset2 <- toy_epi_df[2:3, 1:3]
  att_row_col_subset2 <- attr(row_col_subset2, "metadata")

  expect_true(is_epi_df(row_col_subset2))
  expect_equal(nrow(row_col_subset2), 2L)
  expect_equal(ncol(row_col_subset2), 3L)
  expect_identical(att_row_col_subset2$geo_type, att_toy$geo_type)
  expect_identical(att_row_col_subset2$time_type, att_toy$time_type)
  expect_identical(att_row_col_subset2$as_of, att_toy$as_of)
  expect_identical(att_row_col_subset2$other_keys, att_toy$other_keys[1])
})

test_that("When duplicate cols in subset should abort", {
  expect_error(toy_epi_df[, c(2, 2:3, 4, 4, 4)],
    "Duplicated column names: time_value, indic_var2",
    fixed = TRUE
  )
  expect_error(toy_epi_df[1:4, c(1, 2:4, 1)],
    "Duplicated column name: geo_value",
    fixed = TRUE
  )
})

test_that("Correct metadata when subset includes some of other_keys", {
  # Only include other_var of indic_var1
  only_indic_var1 <- toy_epi_df[, c(1:3, 5:6)]
  att_only_indic_var1 <- attr(only_indic_var1, "metadata")

  expect_true(is_epi_df(only_indic_var1))
  expect_equal(nrow(only_indic_var1), 10L)
  expect_equal(ncol(only_indic_var1), 5L)
  expect_identical(att_only_indic_var1$geo_type, att_toy$geo_type)
  expect_identical(att_only_indic_var1$time_type, att_toy$time_type)
  expect_identical(att_only_indic_var1$as_of, att_toy$as_of)
  expect_identical(att_only_indic_var1$other_keys, att_toy$other_keys[-2])

  # Only include other_var of indic_var2
  only_indic_var2 <- toy_epi_df[, c(1:2, 4:6)]
  att_only_indic_var2 <- attr(only_indic_var2, "metadata")

  expect_true(is_epi_df(only_indic_var2))
  expect_equal(nrow(only_indic_var2), 10L)
  expect_equal(ncol(only_indic_var2), 5L)
  expect_identical(att_only_indic_var2$geo_type, att_toy$geo_type)
  expect_identical(att_only_indic_var2$time_type, att_toy$time_type)
  expect_identical(att_only_indic_var2$as_of, att_toy$as_of)
  expect_identical(att_only_indic_var2$other_keys, att_toy$other_keys[-1])

  # Including both original other_keys was already tested above
})

test_that("Metadata is dropped by `as_tibble`", {
  grouped_converted <- toy_epi_df %>%
    group_by(geo_value) %>%
    as_tibble()
  expect_true(
    !any(c("metadata") %in% names(attributes(grouped_converted)))
  )
})

test_that("Grouping are dropped by `as_tibble`", {
  grouped_converted <- toy_epi_df %>%
    group_by(geo_value) %>%
    as_tibble()
  expect_true(
    !any(c("metadata", "groups") %in% names(attributes(grouped_converted)))
  )
  expect_s3_class(grouped_converted, class(tibble()), exact = TRUE)
})

test_that("Renaming columns gives appropriate colnames and metadata", {
  edf <- tibble::tibble(geo_value = "ak", time_value = as.Date("2020-01-01"), age = 1, value = 1) %>%
    as_epi_df(other_keys = "age")
  # renaming using base R
  renamed_edf1 <- edf %>%
    `[`(c("geo_value", "time_value", "age", "value")) %>%
    `names<-`(c("geo_value", "time_value", "age_group", "value"))
  expect_identical(names(renamed_edf1), c("geo_value", "time_value", "age_group", "value"))
  expect_identical(attr(renamed_edf1, "metadata")$other_keys, c("age_group"))
  # renaming using select
  renamed_edf2 <- edf %>%
    as_epi_df(other_keys = "age") %>%
    select(geo_value, time_value, age_group = age, value)
  expect_identical(renamed_edf1, renamed_edf2)
})

test_that("Renaming columns while grouped gives appropriate colnames and metadata", {
  gedf <- tibble::tibble(geo_value = "ak", time_value = as.Date("2020-01-01"), age = 1, value = 1) %>%
    as_epi_df(other_keys = "age") %>%
    group_by(geo_value)
  # renaming using base R
  renamed_gedf1 <- gedf %>%
    `[`(c("geo_value", "time_value", "age", "value")) %>%
    `names<-`(c("geo_value", "time_value", "age_group", "value"))
  # tets type preservation
  expect_true(inherits(renamed_gedf1, "epi_df"))
  expect_true(inherits(renamed_gedf1, "grouped_df"))
  # the names are right
  expect_identical(names(renamed_gedf1), c("geo_value", "time_value", "age_group", "value"))
  expect_identical(attr(renamed_gedf1, "metadata")$other_keys, c("age_group"))
  # renaming using select
  renamed_gedf2 <- gedf %>%
    select(geo_value, time_value, age_group = age, value)
  expect_identical(renamed_gedf1, renamed_gedf2)
})

test_that("Additional `select` on `epi_df` tests", {
  edf <- tibble::tibble(geo_value = "ak", time_value = as.Date("2020-01-01"), age = 1, value = 1) %>%
    as_epi_df(other_keys = "age")

  # Dropping a non-geo_value epikey column doesn't decay, though maybe it
  # should, since you'd expect that to possibly result in multiple rows per
  # epikey (though not in this toy case), and while we don't require that, we
  # sort of expect it:
  edf_not_decayed <- edf %>%
    select(geo_value, time_value, value)
  expect_class(edf_not_decayed, "epi_df")
  expect_identical(attr(edf_not_decayed, "metadata")$other_keys, character(0L))

  # Dropping geo_value does decay:
  edf_decayed <- edf %>%
    select(age, time_value, value)
  expect_false(inherits(edf_decayed, "epi_df"))
  expect_identical(attr(edf_decayed, "metadata"), NULL)
})

test_that("complete.epi_df works", {
  start_date <- as.Date("2020-01-01")
  daily_edf <- tibble::tribble(
    ~geo_value, ~time_value, ~value,
    1, start_date + 1, 1,
    1, start_date + 3, 3,
    2, start_date + 2, 2,
    2, start_date + 3, 3,
  ) %>%
    as_epi_df(as_of = start_date + 3)
  # Complete without grouping puts all the geo_values on the same min and max
  # time_value index
  expect_identical(
    daily_edf %>%
      complete(geo_value, time_value = full_seq(time_value, period = 1)),
    tibble::tribble(
      ~geo_value, ~time_value, ~value,
      1, start_date + 1, 1,
      1, start_date + 2, NA,
      1, start_date + 3, 3,
      2, start_date + 1, NA,
      2, start_date + 2, 2,
      2, start_date + 3, 3,
    ) %>%
      as_epi_df(as_of = start_date + 3)
  )
  # Complete with grouping puts all the geo_values on individual min and max
  # time_value indices
  expect_identical(
    daily_edf %>%
      group_by(geo_value) %>%
      complete(time_value = full_seq(time_value, period = 1)),
    tibble::tribble(
      ~geo_value, ~time_value, ~value,
      1, start_date + 1, 1,
      1, start_date + 2, NA,
      1, start_date + 3, 3,
      2, start_date + 2, 2,
      2, start_date + 3, 3,
    ) %>%
      as_epi_df(as_of = start_date + 3) %>%
      group_by(geo_value)
  )
  # Complete has explicit=TRUE by default, but if it's FALSE, then complete only fills the implicit gaps
  # not those that are explicitly NA
  daily_edf <- tibble::tribble(
    ~geo_value, ~time_value, ~value,
    1, start_date + 1, 1,
    1, start_date + 2, NA,
    1, start_date + 3, 3,
    2, start_date + 2, 2,
    2, start_date + 3, 3,
  ) %>%
    as_epi_df(as_of = start_date + 3)
  expect_identical(
    daily_edf %>%
      complete(geo_value, time_value = full_seq(time_value, period = 1), fill = list(value = 0), explicit = FALSE),
    tibble::tribble(
      ~geo_value, ~time_value, ~value,
      1, start_date + 1, 1,
      1, start_date + 2, NA,
      1, start_date + 3, 3,
      2, start_date + 1, 0,
      2, start_date + 2, 2,
      2, start_date + 3, 3,
    ) %>%
      as_epi_df(as_of = start_date + 3)
  )
  # Complete works for weekly data and can take a fill value
  # No grouping
  weekly_edf <- tibble::tribble(
    ~geo_value, ~time_value, ~value,
    1, start_date + 1, 1,
    1, start_date + 15, 3,
    2, start_date + 8, 2,
    2, start_date + 15, 3,
  ) %>%
    as_epi_df(as_of = start_date + 3)
  expect_identical(
    weekly_edf %>%
      complete(geo_value,
        time_value = full_seq(time_value, period = 7),
        fill = list(value = 0)
      ),
    tibble::tribble(
      ~geo_value, ~time_value, ~value,
      1, start_date + 1, 1,
      1, start_date + 8, 0,
      1, start_date + 15, 3,
      2, start_date + 1, 0,
      2, start_date + 8, 2,
      2, start_date + 15, 3,
    ) %>%
      as_epi_df(as_of = start_date + 3)
  )
  # With grouping
  expect_identical(
    weekly_edf %>%
      group_by(geo_value) %>%
      complete(
        time_value = full_seq(time_value, period = 7),
        fill = list(value = 0)
      ),
    tibble::tribble(
      ~geo_value, ~time_value, ~value,
      1, start_date + 1, 1,
      1, start_date + 8, 0,
      1, start_date + 15, 3,
      2, start_date + 8, 2,
      2, start_date + 15, 3,
    ) %>%
      as_epi_df(as_of = start_date + 3) %>%
      group_by(geo_value)
  )
})

test_that("aggregate_epi_df works", {
  out <- toy_epi_df %>% aggregate_epi_df(value_col = "x")
  expected_out <- toy_epi_df %>%
    group_by(time_value) %>%
    summarize(x = sum(x)) %>%
    mutate(geo_value = "total") %>%
    as_epi_df(as_of = attr(toy_epi_df, "metadata")$as_of)
  expect_equal(out, expected_out)

  out <- toy_epi_df %>% aggregate_epi_df(value_col = "y", group_cols = c("time_value", "geo_value", "indic_var1"))
  expected_out <- toy_epi_df %>%
    group_by(time_value, geo_value, indic_var1) %>%
    summarize(y = sum(y)) %>%
    ungroup() %>%
    as_epi_df(as_of = attr(toy_epi_df, "metadata")$as_of)
  expect_equal(out, expected_out)
})
