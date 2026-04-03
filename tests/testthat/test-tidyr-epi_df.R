# Common test data
toy_edf <- tibble(
  geo_value = c("ca", "ca"),
  time_value = as.Date(c("2020-01-01", "2020-01-02")),
  val = c(1, NA)
) %>% as_epi_df()

test_that("drop_na.epi_df works", {
  res <- drop_na(toy_edf, val)
  expect_s3_class(res, "epi_df")
  expect_equal(nrow(res), 1)
  expect_equal(res$val, 1)

  expect_s3_class(drop_na(toy_edf %>% mutate(val = NA), val), "epi_df")
  expect_equal(nrow(drop_na(toy_edf %>% mutate(val = NA), val)), 0)
})

test_that("pivot_wider.epi_df handles keys and metadata", {
  # Case 1: Standard pivot, grp is a key and should be updated
  edf_grp <- tibble(
    geo_value = c("ca", "ca"),
    time_value = as.Date(c("2020-01-01", "2020-01-01")),
    grp = c("A", "B"),
    val = c(1, 2)
  ) %>% as_epi_df(other_keys = "grp")

  res_grp <- pivot_wider(edf_grp, names_from = grp, values_from = val)
  expect_s3_class(res_grp, "epi_df")
  expect_true(all(c("A", "B") %in% names(res_grp)))
  expect_false("grp" %in% attr(res_grp, "metadata")$other_keys)

  # Case 2: Decay when essential keys (geo_value) are pivoted away
  res_decay <- pivot_wider(edf_grp, names_from = geo_value, values_from = val)
  expect_false(inherits(res_decay, "epi_df"))

  # Case 3: Standard pivot with age key
  edf_age <- tibble::tibble(
    geo_value = "ak", time_value = as.Date("2020-01-01"),
    age = c(1, 2), value = c(10, 20)
  ) %>%
    as_epi_df(other_keys = "age")
  
  res_age <- edf_age %>% tidyr::pivot_wider(names_from = age, values_from = value)
  expect_class(res_age, "epi_df")
  expect_equal(attr(res_age, "metadata")$other_keys, character(0L))

  # Case 4: Overlapping signal name (user example)
  edf_overlap <- tibble::tibble(
    geo_value = 1, time_value = as.Date("2020-01-01"),
    signal = c("cases", "signal"), value = c(5, 2)
  ) %>%
    as_epi_df(other_keys = "signal")
  
  res_overlap <- edf_overlap %>% tidyr::pivot_wider(names_from = signal, values_from = value)
  expect_class(res_overlap, "epi_df")
  expect_true(all(c("cases", "signal") %in% names(res_overlap)))
  expect_equal(attr(res_overlap, "metadata")$other_keys, character(0L))
})

test_that("pivot_longer.epi_df handles new keys", {
  x <- tibble(
    geo_value = "ca",
    time_value = as.Date("2020-01-01"),
    A = 1, B = 2
  ) %>% as_epi_df()

  # Default names_to="name"
  res <- pivot_longer(x, c(A, B))
  expect_s3_class(res, "epi_df")
  expect_true("name" %in% attr(res, "metadata")$other_keys)
  expect_silent(as_epi_df(res))

  # Explicit names_to
  expect_true("metric" %in% attr(pivot_longer(x, c(A, B), names_to = "metric"), "metadata")$other_keys)

  # .value should not be added to keys
  x_complex <- tibble(
    geo_value = "ca", time_value = as.Date("2020-01-01"),
    x_1 = 1, x_2 = 2, y_1 = 3, y_2 = 4
  ) %>% as_epi_df()
  res_complex <- pivot_longer(x_complex, cols = -c(geo_value, time_value),
                               names_to = c(".value", "num"), names_sep = "_")
  expect_true("num" %in% attr(res_complex, "metadata")$other_keys)
  expect_false(".value" %in% attr(res_complex, "metadata")$other_keys)

  # names_to should be added even if rows are already unique
  x_sparse <- tibble(
    geo_value = c("ca", "ny"), time_value = as.Date("2020-01-01"),
    val1 = c(1, NA), val2 = c(NA, 2)
  ) %>% as_epi_df()
  res_sparse <- pivot_longer(x_sparse, cols = c(val1, val2), values_drop_na = TRUE)
  expect_true("name" %in% attr(res_sparse, "metadata")$other_keys)
})
