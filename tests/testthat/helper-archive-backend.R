# Backend-switching helpers for archive tests.
#
# Most tests should exercise public archive behavior, not the storage engine.
# Set EPIPROCESS_TEST_ARCHIVE_BACKEND=duck to make unqualified
# `as_epi_archive()` calls construct duck-backed archives while the test suite
# is being migrated away from `$DT` assumptions.

test_archive_backend <- function() {
  backend <- Sys.getenv("EPIPROCESS_TEST_ARCHIVE_BACKEND", unset = "dt")
  if (!backend %in% c("dt", "duck")) {
    stop(
      "Unknown EPIPROCESS_TEST_ARCHIVE_BACKEND: ", backend,
      ". Expected one of: dt, duck.",
      call. = FALSE
    )
  }
  backend
}

using_duck_backend <- function() {
  identical(test_archive_backend(), "duck")
}

skip_if_duck_backend <- function(reason = "data.table-backend-specific test") {
  if (using_duck_backend()) {
    testthat::skip(reason)
  }
}

skip_if_dt_backend <- function(reason = "duck-backend-specific test") {
  if (identical(test_archive_backend(), "dt")) {
    testthat::skip(reason)
  }
}

if (using_duck_backend()) {
  as_epi_archive <- function(...) {
    testthat::skip_if_not_installed("duckplyr")
    epiprocess::as_duckdb_epi_archive(...)
  }
}
