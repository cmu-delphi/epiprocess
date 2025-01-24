test_that("`key_colnames` on non-`epi_df`-like tibbles works as expected", {
  k1k2_tbl <- tibble::tibble(k1 = 1, k2 = 1)

  expect_equal(
    key_colnames(k1k2_tbl, geo_keys = character(), time_keys = character(), other_keys = c("k1", "k2")),
    c("k1", "k2")
  )
  # `geo_keys`, `other_keys`, `time_keys` are mandatory:
  expect_error(key_colnames(k1k2_tbl, other_keys = c("k1", "k2"), time_keys = character()),
               regexp = '"geo_keys" is missing')
  expect_error(key_colnames(k1k2_tbl, geo_keys = character(), time_keys = character()),
               regexp = '"other_keys" is missing')
  expect_error(key_colnames(k1k2_tbl, geo_keys = character(), other_keys = c("k1", "k2")),
               regexp = '"time_keys" is missing')

  # Manually specifying keys that aren't there is an error:
  expect_error(
    key_colnames(k1k2_tbl, geo_keys = "bogus", other_keys = c("k1", "k2"), time_keys = character()),
    class = "epiprocess__key_colnames__keys_not_in_colnames"
  )
  expect_error(
    key_colnames(k1k2_tbl, other_keys = "bogus", geo_keys = character(), time_keys = character()),
    class = "epiprocess__key_colnames__keys_not_in_colnames"
  )
  expect_error(
    key_colnames(k1k2_tbl, time_keys = "bogus", geo_keys = character(), other_keys = c("k1", "k2")),
    class = "epiprocess__key_colnames__keys_not_in_colnames"
  )

  # We can specify non-`epi_df`-like geo keys:
  expect_equal(
    key_colnames(k1k2_tbl, geo_keys = c("k1", "k2"), other_keys = character(), time_keys = character()),
    c("k1", "k2")
  )
})

test_that("`key_colnames` on `epi_df`s and similar tibbles works as expected", {
  withr::local_options(list(lifecycle_verbosity = "warning")) # for extra_keys tests

  gat_tbl <- tibble::tibble(geo_value = 1, age_group = 1, time_value = 1)
  gat_edf <- as_epi_df(gat_tbl, other_keys = "age_group", as_of = 2)

  # For tbl: we must provide all key naming arguments:
  expect_equal(
    key_colnames(gat_tbl, geo_keys = "geo_value", other_keys = "age_group", time_keys = "time_value"),
    c("geo_value", "age_group", "time_value")
  )
  # given same inputs, compatible edfs give something compatible:
  expect_equal(
    key_colnames(gat_edf, geo_keys = "geo_value", other_keys = "age_group", time_keys = "time_value"),
    c("geo_value", "age_group", "time_value")
  )
  # though edfs don't have to specify the key settings:
  expect_equal(
    key_colnames(gat_edf),
    c("geo_value", "age_group", "time_value")
  )
  # and they will balk if we write something intended to work for both tbls and
  # edfs but mis-specify something:
  expect_error(
    key_colnames(gat_edf, geo_keys = character(0L)),
    class = "epiprocess__key_colnames__mismatched_geo_keys"
  )
  expect_error(
    key_colnames(gat_edf, other_keys = character(0L)),
    class = "epiprocess__key_colnames__mismatched_other_keys"
  )
  expect_error(
    key_colnames(gat_edf, time_keys = character(0L)),
    class = "epiprocess__key_colnames__mismatched_time_keys"
  )

  # edfs also won't let us specify nonstandard geotime keys:
  expect_error(
    key_colnames(gat_edf, geo_keys = "time_value"),
    class = "epiprocess__key_colnames__mismatched_geo_keys"
  )
  expect_error(
    key_colnames(gat_edf, time_keys = "geo_value"),
    class = "epiprocess__key_colnames__mismatched_time_keys"
  )

  # We can exclude keys:
  expect_equal(
    key_colnames(gat_tbl, geo_keys = "geo_value", other_keys = "age_group", time_keys = "time_value", exclude = c("time_value")),
    c("geo_value", "age_group")
  )
  expect_equal(
    key_colnames(gat_tbl, geo_keys = "geo_value", other_keys = "age_group", time_keys = "time_value", exclude = c("geo_value", "time_value")),
    c("age_group")
  )
  expect_equal(
    key_colnames(gat_edf, exclude = c("time_value")),
    c("geo_value", "age_group")
  )
  expect_equal(
    key_colnames(gat_edf, exclude = c("geo_value", "time_value")),
    c("age_group")
  )

  # Using `extra_keys =` is soft-deprecated and routes to `other_keys =`:
  expect_warning(
    gat_tbl_extra_keys_res <- key_colnames(gat_tbl, geo_keys = "geo_value", time_keys = "time_value", extra_keys = "age_group"),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(gat_tbl_extra_keys_res, c("geo_value", "age_group", "time_value"))

  expect_warning(
    gat_edf_extra_keys_exclude_res <-
      key_colnames(gat_edf, extra_keys = "age_group", exclude = c("geo_value", "time_value")),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(gat_edf_extra_keys_exclude_res, c("age_group"))
})

test_that("`key_colnames` on tsibbles works as expected", {
  k1k2i_tsbl <- tsibble::tsibble(k1 = 1, k2 = 1, i = 1, key = c(k1, k2), index = i)

  # Normal operation:
  expect_equal(key_colnames(k1k2i_tsbl), c("k1", "k2", "i"))

  # Currently there is just bare-bones support for tsibbles to not output
  # incompatible results based on `data.frame` inheritance:
  expect_error(
    key_colnames(k1k2i_tsbl, geo_keys = "k1"),
    class = "rlib_error_dots_nonempty"
  )
  expect_error(
    key_colnames(k1k2i_tsbl, time_keys = "k1"),
    class = "rlib_error_dots_nonempty"
  )
  expect_error(
    key_colnames(k1k2i_tsbl, other_keys = "k1"),
    class = "rlib_error_dots_nonempty"
  )

  # We guard against confusing cases:
  expect_error(
    key_colnames(k1k2i_tsbl %>% tsibble::index_by(fake_coarser_i = i)),
    class = "epiprocess__key_colnames__incomplete_reindexing_operation"
  )
})

test_that("`key_colnames` on `epi_archive`s works as expected", {
  gatv_ea <- tibble(geo_value = 1, age_group = 1, time_value = 1, version = 2) %>%
    as_epi_archive(other_keys = "age_group")

  # Basic operation:
  expect_equal(
    key_colnames(gatv_ea),
    c("geo_value", "age_group", "time_value", "version")
  )

  # Since we shouldn't have uncertainty about whether we might have an archive
  # or not, there's no need to provide compatibility with the key specification
  # args:
  expect_error(
    key_colnames(gatv_ea, geo_keys = "k1"),
    class = "rlib_error_dots_nonempty"
  )
  expect_error(
    key_colnames(gatv_ea, time_keys = "k1"),
    class = "rlib_error_dots_nonempty"
  )
  expect_error(
    key_colnames(gatv_ea, other_keys = "k1"),
    class = "rlib_error_dots_nonempty"
  )

  # Key exclusion works:
  expect_equal(
    key_colnames(gatv_ea, exclude = c("version", "time_value")),
    c("geo_value", "age_group")
  )
})
