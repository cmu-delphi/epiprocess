#' Helper functions for custom RDS snapshots.
#'
#' Based on:
#' https://github.com/ryantibs/quantgen/blob/59718548fd6b327b5f1870f9c86fb079cc500b70/quantgen/tests/testthat/test-characterization-quantile_ensemble.R#L64
#'
#' Helper function for expect_equal_custom_rds_snapshot below. Don't call directly.
expect_save_custom_rds_snapshot <- function(object, file, nonce.to.overwrite.custom.snapshot.with.new.value) {
  if (file.exists(file)) {
    old.object.and.nonce <- readRDS(file)
    if (identical(old.object.and.nonce[["nonce"]], nonce.to.overwrite.custom.snapshot.with.new.value)) {
      testthat::fail("nonce.to.overwrite.custom.snapshot.with.new.value matched the saved old nonce value. This may indicate that someone overwrote the custom snapshot already, but forgot to remove the nonce.to.overwrite.custom.snapshot.with.new.value argument to expect_identical_custom_rds_snapshot. (With a TRUE/FALSE switch instead of a nonce, this would effectively disable this test, always overwriting the rds with a new value.) The snapshot has not been updated. TO FIX: (a) if you do not want to overwrite the snapshot, do not pass a nonce.to.overwrite.custom.snapshot.with.new.value argument to expect_identical_custom_rds_snapshot. (b) If you want to overwrite the custom snapshot, change the nonce argument to a different integer (do not use RNG calls within your test code to come up with the value).")
      return()
    }
  }
  dirpath <- dirname(file)
  if (!dir.exists(dirpath)) {
    dir.create(dirpath, recursive = TRUE)
  }
  object.and.nonce <- list(object = object, nonce = nonce.to.overwrite.custom.snapshot.with.new.value)
  saveRDS(object.and.nonce, file)
  testthat::succeed()
}

expect_equal_custom_rds_snapshot <- function(object, base.name.sans.ext, nonce.to.overwrite.custom.snapshot.with.new.value = NA_integer_) {
  file <- file.path("_custom_snaps", paste0(base.name.sans.ext, ".RDS"))
  if (!is.na(nonce.to.overwrite.custom.snapshot.with.new.value)) {
    expect_save_custom_rds_snapshot(object, file, nonce.to.overwrite.custom.snapshot.with.new.value)
  } else if (!file.exists(file)) {
    warning("Custom snapshot rds not found; initializing custom snapshot rds using object that was to be tested.")
    expect_save_custom_rds_snapshot(object, file, nonce.to.overwrite.custom.snapshot.with.new.value)
  } else {
    testthat::expect_equal(object, readRDS(file)[["object"]])
  }
}
