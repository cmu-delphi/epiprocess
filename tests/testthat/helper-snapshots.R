#' Helper functions for custom RDS snapshots.
#'
#' Based on:
#' https://github.com/ryantibs/quantgen/blob/59718548fd6b327b5f1870f9c86fb079cc500b70/
#'   quantgen/tests/testthat/test-characterization-quantile_ensemble.R#L64
#'
#' Helper function for `expect_equal_custom_rds_snapshot`. Don't call directly.
expect_save_custom_rds_snapshot <- function(object, file, nonce) {
  if (file.exists(file)) {
    saved <- readRDS(file)
    if (identical(saved[["nonce"]], nonce)) {
      testthat::fail(paste(
        "The `nonce` argument matched the saved nonce value. This may mean the",
        "snapshot was already overwritten but the `nonce` argument was not removed",
        "from `expect_equal_custom_rds_snapshot`. (A reused nonce would silently",
        "overwrite the snapshot on every run, defeating the purpose of the test.)",
        "TO FIX: (a) Remove the `nonce` argument if you do not intend to overwrite.",
        "(b) Use a *new* integer nonce to overwrite the snapshot again.",
        "(Do not use RNG calls to generate the nonce value.)"
      ))
      return()
    }
  }
  dirpath <- dirname(file)
  if (!dir.exists(dirpath)) {
    dir.create(dirpath, recursive = TRUE)
  }
  saveRDS(list(object = object, nonce = nonce), file)
  testthat::succeed()
}

#' Compare an object against a saved RDS snapshot, with optional overwrite.
expect_equal_custom_rds_snapshot <- function(
  object,
  snap_name,
  nonce = NA_integer_
) {
  file <- file.path("_custom_snaps", paste0(snap_name, ".RDS"))
  if (!is.na(nonce)) {
    expect_save_custom_rds_snapshot(object, file, nonce)
  } else if (!file.exists(file)) {
    warning("Custom snapshot RDS not found; creating from the current object.")
    expect_save_custom_rds_snapshot(object, file, nonce)
  } else {
    testthat::expect_equal(object, readRDS(file)[["object"]])
  }
}
