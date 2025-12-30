test_that("relocate preserves epi_df class and metadata", {
    x <- tibble::tibble(
        geo_value = c("ca", "ca", "fl", "fl"),
        time_value = as.Date(c("2020-01-01", "2020-01-02", "2020-01-01", "2020-01-02")),
        value = c(1, 2, 3, 4),
        other_col = c("a", "b", "c", "d")
    ) %>% as_epi_df()

    # Relocate non-key column
    res <- x %>% dplyr::relocate(value, .before = geo_value)
    expect_s3_class(res, "epi_df")
    expect_equal(names(res), c("value", "geo_value", "time_value", "other_col"))
    expect_equal(attributes(res)$metadata, attributes(x)$metadata)

    # Relocate key column (geo_value)
    res2 <- x %>% dplyr::relocate(time_value, .before = geo_value)
    expect_s3_class(res2, "epi_df")
    expect_equal(names(res2), c("time_value", "geo_value", "value", "other_col"))
})

test_that("distinct preserves epi_df class if keys are kept", {
    # Valid epi_df (must be unique on keys)
    x <- tibble::tibble(
        geo_value = c("ca", "ca", "fl", "fl"),
        time_value = as.Date(c("2020-01-01", "2020-01-02", "2020-01-01", "2020-01-02")),
        value = c(1, 1, 3, 4),
        other_col = c("a", "a", "c", "d")
    ) %>% as_epi_df()

    # distinct() on all cols (no-op on already unique keys, usually)
    res <- x %>% dplyr::distinct()
    expect_s3_class(res, "epi_df")
    expect_equal(nrow(res), 4)
    expect_equal(attributes(res)$metadata, attributes(x)$metadata)

    # distinct() with .keep_all = TRUE (keys preserved)
    # This filters to unique combinations of geo_value (picking first), keeping time_value
    res2 <- x %>% dplyr::distinct(geo_value, .keep_all = TRUE)
    expect_s3_class(res2, "epi_df")
    expect_equal(nrow(res2), 2) # Only 2 geo_values
})

test_that("distinct decays to tibble if keys are dropped", {
    x <- tibble::tibble(
        geo_value = c("ca", "ca", "fl", "fl"),
        time_value = as.Date(c("2020-01-01", "2020-01-02", "2020-01-01", "2020-01-02")),
        value = c(1, 2, 3, 4)
    ) %>% as_epi_df()

    # distinct(geo_value) -> drops time_value and value
    res <- x %>% dplyr::distinct(geo_value)
    expect_false(inherits(res, "epi_df"))
    expect_s3_class(res, "tbl_df")
    expect_equal(names(res), "geo_value")
})

test_that("fill preserves epi_df class", {
    x <- tibble::tibble(
        geo_value = c("ca", "ca", "fl", "fl"),
        time_value = as.Date(c("2020-01-01", "2020-01-02", "2020-01-01", "2020-01-02")),
        value = c(1, NA, 3, NA),
        other_col = c("a", "b", "c", "d")
    ) %>% as_epi_df()

    # fill value
    res <- x %>% tidyr::fill(value, .direction = "down")
    expect_s3_class(res, "epi_df")
    expect_equal(res$value, c(1, 1, 3, 3))
    expect_equal(attributes(res)$metadata, attributes(x)$metadata)
})

test_that("fill handles grouped epi_df", {
    x <- tibble::tibble(
        geo_value = c("ca", "ca", "fl", "fl"),
        time_value = as.Date(c("2020-01-01", "2020-01-02", "2020-01-01", "2020-01-02")),
        value = c(1, NA, 3, NA)
    ) %>%
        as_epi_df() %>%
        dplyr::group_by(geo_value)

    res <- x %>% tidyr::fill(value, .direction = "down")
    expect_s3_class(res, "epi_df")
    expect_s3_class(res, "grouped_df")
    expect_equal(res$value, c(1, 1, 3, 3))
})
