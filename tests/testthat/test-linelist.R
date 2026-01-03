test_that("linelist_to_archive works with basic inputs", {
    library(tibble)
    library(dplyr)

    linelist <- tibble(
        event_id = 1:3,
        geo_value = c("ca", "ca", "ca"),
        time_value = as.Date(c("2022-01-01", "2022-01-01", "2022-01-02")),
        version_recorded = as.Date(c("2022-01-02", "2022-01-02", "2022-01-03")),
        version_deleted = as.Date(c("2022-01-04", NA, NA))
    )

    # Basic call
    ea <- linelist_to_archive(
        linelist,
        geo_value = geo_value,
        time_value = time_value,
        version_recorded = version_recorded,
        version_deleted = version_deleted
    )

    expect_s3_class(ea, "epi_archive")
    expect_equal(ea$DT$n, c(2, 1, 1))

    # Check content at different versions
    df_v2 <- epix_as_of(ea, as.Date("2022-01-02"))
    expect_equal(nrow(df_v2), 1)
    expect_equal(df_v2$n, 2)
    expect_equal(df_v2$time_value, as.Date("2022-01-01"))

    df_v3 <- epix_as_of(ea, as.Date("2022-01-03"))
    expect_equal(nrow(df_v3), 2)
    expect_equal(sum(df_v3$n), 3)

    df_v4 <- epix_as_of(ea, as.Date("2022-01-04"))
    expect_equal(nrow(df_v4), 2)
    expect_equal(df_v4$n[df_v4$time_value == "2022-01-01"], 1)
})

test_that("linelist_to_archive handles tidy selection", {
    linelist <- tibble::tibble(
        state = "ca",
        date = as.Date("2022-01-01"),
        recorded = as.Date("2022-01-02")
    )

    ea <- linelist_to_archive(
        linelist,
        geo_value = state,
        time_value = date,
        version_recorded = recorded
    )

    expect_s3_class(ea, "epi_archive")
    expect_equal(ea$DT$geo_value, "ca")
})

test_that("linelist_to_archive handles other_keys", {
    linelist <- tibble::tibble(
        geo_value = "ca",
        time_value = as.Date("2022-01-01"),
        version_recorded = as.Date("2022-01-02"),
        age_group = "adult"
    )

    ea <- linelist_to_archive(
        linelist,
        geo_value = geo_value,
        time_value = time_value,
        version_recorded = version_recorded,
        other_keys = age_group
    )

    expect_true("age_group" %in% names(ea$DT))
    expect_equal(key(ea$DT), c("geo_value", "time_value", "age_group", "version"))
})

test_that("linelist_to_archive errors on missing required cols", {
    linelist <- tibble::tibble(
        geo_value = "ca",
        time_value = as.Date("2022-01-01")
    )
    # Missing version_recorded
    expect_error(linelist_to_archive(linelist))
})
test_that("linelist_to_archive validates id uniqueness", {
    library(tibble)
    library(dplyr)

    # duplicate creation for id
    LL_bad <- tibble::tibble(
        id = c("A", "A"),
        geo_value = "ca",
        time_value = as.Date("2022-01-01"),
        recorded = as.Date("2022-01-02")
    )

    expect_error(
        linelist_to_archive(LL_bad,
            geo_value = geo_value,
            time_value = time_value,
            version_recorded = recorded, id = id
        ),
        "must have at most one .* entry"
    )
})

test_that("linelist_to_archive supports split rows with id", {
    # One row creates, one row deletes
    LL_split <- tibble::tibble(
        case_id = c("A", "A"),
        geo_value = "ca",
        time_value = as.Date("2022-01-01"),
        recorded = as.Date(c("2022-01-02", NA)),
        deleted = as.Date(c(NA, "2022-01-05"))
    )

    ea <- linelist_to_archive(
        LL_split,
        geo_value = geo_value,
        time_value = time_value,
        version_recorded = recorded,
        version_deleted = deleted,
        id = case_id
    )

    expect_s3_class(ea, "epi_archive")
    df <- epix_as_of(ea, as.Date("2022-01-04"))
    expect_equal(df$n, 1)

    df2 <- epix_as_of(ea, as.Date("2022-01-05"))
    expect_equal(df2$n, 0)
})

test_that("linelist_to_archive enforces deleted >= recorded with id", {
    LL_bad <- tibble::tibble(
        id = "A",
        geo_value = "ca",
        time_value = as.Date("2022-01-01"),
        recorded = as.Date("2022-01-05"),
        deleted = as.Date("2022-01-04") # Deleted BEFORE recorded
    )

    expect_error(
        linelist_to_archive(LL_bad,
            geo_value = geo_value, time_value = time_value,
            version_recorded = recorded, version_deleted = deleted, id = id
        ),
        "must be >= "
    )
})
test_that("linelist_to_archive handles unordered split rows with id", {
    LL_unordered <- tibble::tibble(
        case_id = c("A", "A"),
        geo_value = "ca",
        time_value = as.Date("2022-01-01"),
        recorded = as.Date(c(NA, "2022-01-02")),
        deleted = as.Date(c("2022-01-05", NA))
    )

    expect_no_error(
        ea <- linelist_to_archive(
            LL_unordered,
            geo_value = geo_value,
            time_value = time_value,
            version_recorded = recorded,
            version_deleted = deleted,
            id = case_id
        )
    )
})
test_that("linelist_to_archive uses smart defaults", {
    library(tibble)
    library(dplyr)

    # All defaults
    # geo_value -> state, time_value -> date, version_recorded -> issue
    LL_default <- tibble::tibble(
        state = "ca",
        date = as.Date("2022-01-01"),
        issue = as.Date("2022-01-02"),
        value = 1
    )

    # Should work without args
    ea <- linelist_to_archive(LL_default)
    expect_s3_class(ea, "epi_archive")
    expect_equal(ea$DT$geo_value, "ca")

    # Partial defaults
    # geo_value -> geo_value, time_value -> date, version -> my_ver
    LL_mixed <- tibble::tibble(
        geo_value = "ny",
        date = as.Date("2022-01-01"),
        my_ver = as.Date("2022-01-02")
    )

    ea2 <- linelist_to_archive(LL_mixed, version_recorded = my_ver)
    expect_s3_class(ea2, "epi_archive")
    expect_equal(ea2$DT$geo_value, "ny")
})

test_that("linelist_to_archive errors when defaults not found", {
    LL_bad <- tibble::tibble(
        loc = "ca", # not in default list
        day = as.Date("2022-01-01"), # not in default list
        ver = as.Date("2022-01-02")
    )

    expect_error(linelist_to_archive(LL_bad), "Could not select column for `geo_value`")
})


test_that("linelist_to_archive supports chart-style linelists", {
    # chart-style linelist
    x <- tibble::tibble(
        geo_value = "ma",
        time_value = as.Date("2020-01-01"),
        issue_date = as.Date(c("2020-01-02", "2020-01-02", "2020-01-03")),
        is_deleted = c(FALSE, FALSE, TRUE),
        id = c(1, 2, 1)
    )

    # Should fail if is_deleted not provided
    expect_error(
        linelist_to_archive(x,
            geo_value = geo_value, time_value = time_value,
            version_recorded = issue_date, version_deleted = issue_date,
            id = id
        ),
        "is_deleted.*must be provided"
    )

    # Should work with is_deleted
    ea <- linelist_to_archive(
        x,
        geo_value = geo_value, time_value = time_value,
        version_recorded = issue_date, version_deleted = issue_date,
        is_deleted = is_deleted, id = id, value = "count"
    )

    # Verify history using snapshots

    snap1 <- epix_as_of(ea, as.Date("2020-01-02"))
    expect_equal(nrow(snap1), 1)
    expect_equal(snap1$count, 2)

    snap2 <- epix_as_of(ea, as.Date("2020-01-03"))
    expect_equal(nrow(snap2), 1)
    expect_equal(snap2$count, 1)
})

test_that("linelist_to_archive chart-style validation works", {
    x <- tibble::tibble(
        geo_value = "ma",
        time_value = as.Date("2020-01-01"),
        issue_date = as.Date("2020-01-02"),
        is_deleted = c(NA),
        id = 1
    )

    expect_error(
        linelist_to_archive(
            x,
            geo_value = geo_value, time_value = time_value,
            version_recorded = issue_date, version_deleted = issue_date,
            is_deleted = is_deleted, id = id
        ),
        "must not contain NAs"
    )
})
