#' Test two vctrs vectors for equality with some tolerance in some cases
#'
#' @param vec1,vec2 vctrs vectors (includes data frames)
#' @param abs_tol tolerance; will be used for bare numeric `vec1`, `vec2`, or
#'   any such columns within `vec1`, `vec2` if they are data frames
#' @param na_equal should `NA`s be considered equal to each other? (In
#'   epiprocess, we usually want this to be `TRUE`, but that doesn't match the
#'   [`vctrs::vec_equal()`] default, so this is mandatory.)
#' @param .ptype as in [`vctrs::vec_equal()`]
#' @param inds1,inds2 optional (row) indices into vec1 and vec2; output should
#'   be consistent with `vec_slice`-ing to these indices beforehand, but can
#'   give faster computation if `vec1` and `vec2` are data frames.
#'
#' @return logical vector; no nonmissing entries if `na_equal = TRUE`. Behavior
#'   may differ from `vec_equal` with non-`NA` `NaN`s involved.
approx_equal <- function(vec1, vec2, abs_tol, na_equal, .ptype = NULL, inds1 = NULL, inds2 = NULL) {
  # Recycle inds if provided; vecs if not:
  common_size <- vec_size_common(
    if (is.null(inds1)) vec1 else inds1,
    if (is.null(inds2)) vec2 else inds2
  )
  if (is.null(inds1)) {
    vec1 <- vec_recycle(vec1, common_size)
  } else {
    inds1 <- vec_recycle(inds1, common_size)
  }
  if (is.null(inds2)) {
    vec2 <- vec_recycle(vec2, common_size)
  } else {
    inds2 <- vec_recycle(inds2, common_size)
  }
  vecs <- vec_cast_common(vec1, vec2, .to = .ptype)
  approx_equal0(vecs[[1]], vecs[[2]], abs_tol, na_equal, inds1, inds2)
}

approx_equal0 <- function(vec1, vec2, abs_tol, na_equal, inds1 = NULL, inds2 = NULL) {
  if (is_bare_numeric(vec1) && abs_tol != 0) {
    # perf: since we're working with bare numerics and logicals: we can use `[`
    # and `fifelse`. Matching vec_equal, we ignore names and other attributes.

    # FIXME matrices can make their way in here though...
    if (!is.null(inds1)) vec1 <- vec1[inds1]
    if (!is.null(inds2)) vec2 <- vec2[inds2]
    res <- fifelse(
      !is.na(vec1) & !is.na(vec2),
      abs(vec1 - vec2) <= abs_tol,
      if (na_equal) is.na(vec1) & is.na(vec2) else FALSE
      # XXX ^ inconsistent with vec_equal treatment: NA vs. NaN comparison
      # behavior with na_equal = TRUE is different; plus output with na_equal =
      # FALSE on two NAs is different
    )
    # `fifelse` inherits any unrecognized attributes; drop them instead:
    attributes(res) <- NULL
    return(res)
  } else if (is.data.frame(vec1) && abs_tol != 0) {
    # (we only need to manually recurse if we potentially have columns that would
    # be specially processed by the above)
    if (ncol(vec1) == 0) {
      rep(TRUE, nrow(vec1))
    } else {
      Reduce(`&`, lapply(seq_len(ncol(vec1)), function(col_i) {
        approx_equal0(vec1[[col_i]], vec2[[col_i]], abs_tol, na_equal, inds1, inds2)
      }))
    }
  } else {
    # XXX No special handling for any other types/situations. Makes sense for
    # unclassed atomic things; bare lists and certain vctrs classes might want
    # recursion / specialization, though.
    if (!is.null(inds1)) {
      vec1 <- vec_slice(vec1, inds1)
      vec2 <- vec_slice(vec2, inds2)
    }
    res <- vec_equal(vec1, vec2, na_equal = na_equal)
    return(res)
  }
}

#' Calculate compact patch to move from one snapshot/update to another
#'
#' @param earlier_snapshot tibble or `NULL`; `NULL` represents that there was no
#'   data before `later_tbl`.
#' @param later_tbl tibble; must have the same column names as
#'   `earlier_snapshot` if it is a tibble.
#' @param ukey_names character; column names that together, form a unique key
#'   for `earlier_snapshot` and for `later_tbl`. This is unchecked; see
#'   [`check_ukey_unique`] if you don't already have this guaranteed.
#' @param later_format "snapshot" or "update"; default is "snapshot". If
#'   "snapshot", `later_tbl` will be interpreted as a full snapshot of the data
#'   set including all ukeys, and any ukeys that are in `earlier_snapshot` but
#'   not in `later_tbl` are interpreted as deletions, which are currently
#'   (imprecisely) represented in the output patch as revisions of all
#'   non-`ukey_names` columns to NA values (using `{vctrs}`). If "update", then
#'   it's assumed that any deletions have already been represented this way in
#'   `later_tbl` and any ukeys not in `later_tbl` are simply unchanged; we are
#'   just ensuring that the update is fully compact for the given
#'   `compactify_abs_tol`.
#' @param compactify_abs_tol compactification tolerance; see `apply_compactify`
#' @return a tibble in compact "update" (diff) format
tbl_diff2 <- function(earlier_snapshot, later_tbl,
                      ukey_names,
                      later_format = c("snapshot", "update"),
                      compactify_abs_tol = 0) {
  # Most input validation + handle NULL earlier_snapshot. This is a small function so
  # use faster validation variants:
  if (!is_tibble(later_tbl)) {
    cli_abort("`later_tbl` must be a tibble")
  }
  if (is.null(earlier_snapshot)) {
    return(later_tbl)
  }
  if (!is_tibble(earlier_snapshot)) {
    cli_abort("`earlier_snapshot` must be a tibble or `NULL`")
  }
  if (!is.character(ukey_names) || !all(ukey_names %in% names(earlier_snapshot))) {
    cli_abort("`ukey_names` must be a subset of column names")
  }
  later_format <- arg_match0(later_format, c("snapshot", "update"))
  if (!(is.vector(compactify_abs_tol, mode = "numeric") && length(compactify_abs_tol) == 1L && compactify_abs_tol >= 0)) {
    # Give a specific message:
    assert_numeric(compactify_abs_tol, lower = 0, any.missing = FALSE, len = 1L)
    # Fallback e.g. for invalid classes not caught by assert_numeric:
    cli_abort("`compactify_abs_tol` must be a length-1 double/integer >= 0")
  }

  # Extract metadata:
  earlier_n <- nrow(earlier_snapshot)
  later_n <- nrow(later_tbl)
  tbl_names <- names(earlier_snapshot)
  val_names <- tbl_names[!tbl_names %in% ukey_names]

  # More input validation:
  if (!identical(tbl_names, names(later_tbl))) {
    # XXX is this check actually necessary?
    cli_abort(c("`earlier_snapshot` and `later_tbl` should have identical column
                 names and ordering.",
      "*" = "`earlier_snapshot` colnames: {format_chr_deparse(tbl_names)}",
      "*" = "`later_tbl` colnames: {format_chr_deparse(names(later_tbl))}"
    ))
  }

  combined_tbl <- vec_rbind(earlier_snapshot, later_tbl)
  combined_n <- nrow(combined_tbl)

  # We'll also need epikeytimes and value columns separately:
  combined_ukeys <- combined_tbl[ukey_names]
  combined_vals <- combined_tbl[val_names]

  # We have five types of rows in combined_tbl:
  # 1. From earlier_snapshot, no matching ukey in later_tbl (deletion; turn vals to
  #    NAs to match epi_archive format)
  # 2. From earlier_snapshot, with matching ukey in later_tbl (context; exclude from
  #    result)
  # 3. From later_tbl, with matching ukey in earlier_snapshot, with value "close" (change
  #    that we'll compactify away)
  # 4. From later_tbl, with matching ukey in earlier_snapshot, value not "close" (change
  #    that we'll record)
  # 5. From later_tbl, with no matching ukey in later_tbl (addition)

  # For "snapshot" later_format, we need to filter to 1., 4., and 5., and alter
  # values for 1.  For "update" later_format, we need to filter to 4. and 5.

  # (For compactify_abs_tol = 0, we could potentially streamline things by dropping
  # ukey+val duplicates (cases 2. and 3.).)

  # Row indices of first occurrence of each ukey; will be the same as
  # seq_len(combined_n) except for when that ukey has been re-reported in
  # `later_tbl`, in which case (3. or 4.) it will point back to the row index of
  # the same ukey in `earlier_snapshot`:
  combined_ukey_firsts <- vec_duplicate_id(combined_ukeys)

  # Which rows from combined are cases 3. or 4.?
  combined_ukey_is_repeat <- combined_ukey_firsts != seq_len(combined_n)
  # For each row in 3. or 4., row numbers of the ukey appearance in earlier:
  ukey_repeat_first_i <- combined_ukey_firsts[combined_ukey_is_repeat]

  # Which rows from combined are in case 3.?
  combined_compactify_away <- rep(FALSE, combined_n)
  combined_compactify_away[combined_ukey_is_repeat] <-
    approx_equal0(combined_vals,
      combined_vals,
      # TODO move inds closer to vals to not be as confusing?
      abs_tol = compactify_abs_tol,
      na_equal = TRUE,
      inds1 = combined_ukey_is_repeat,
      inds2 = ukey_repeat_first_i
    )

  # Which rows from combined are in cases 3., 4., or 5.?
  combined_from_later <- vec_rep_each(c(FALSE, TRUE), c(earlier_n, later_n))

  if (later_format == "update") {
    # Cases 4. and 5.:
    combined_tbl <- combined_tbl[combined_from_later & !combined_compactify_away, ]
  } else { # later_format == "snapshot"
    # Which rows from combined are in case 1.?
    combined_is_deletion <- vec_rep_each(c(TRUE, FALSE), c(earlier_n, later_n))
    combined_is_deletion[ukey_repeat_first_i] <- FALSE
    # Which rows from combined are in cases 1., 4., or 5.?
    combined_include <- combined_is_deletion | combined_from_later & !combined_compactify_away
    combined_tbl <- combined_tbl[combined_include, ]
    # Represent deletion in 1. with NA-ing of all value columns. (In some
    # previous approaches to epi_diff2, this seemed to be faster than using
    # vec_rbind(case_1_ukeys, cases_45_tbl) or bind_rows to fill with NAs, and more
    # general than data.table's rbind(case_1_ukeys, cases_45_tbl, fill = TRUE).)
    combined_tbl[combined_is_deletion[combined_include], val_names] <- NA
  }

  combined_tbl
}

epi_diff2 <- function(earlier_snapshot, later_edf,
                      later_format = c("snapshot", "update"),
                      compactify_abs_tol = 0) {
  ukey_names <- key_colnames(later_edf)
  dplyr_reconstruct(tbl_diff2(as_tibble(earlier_snapshot), as_tibble(later_edf), ukey_names, later_format, compactify_abs_tol), later_edf)
}

# XXX vs. tbl_patch_apply?



#' Apply an update (e.g., from `tbl_diff2`) to a snapshot
#'
#' @param snapshot tibble or `NULL`; entire data set as of some version, or
#'   `NULL` to treat `update` as the initial version of the data set.
#' @param update tibble; ukeys + initial values for added rows, ukeys + new
#'   values for changed rows. Deletions must be imprecisely represented as
#'   changing all values to NAs.
#' @param ukey_names character; names of columns that should form a unique key
#'   for `snapshot` and for `update`. Uniqueness is unchecked; if you don't have
#'   this guaranteed, see [`check_ukey_unique()`].
#' @return tibble; snapshot of the data set with the update applied.
tbl_patch <- function(snapshot, update, ukey_names) {
  # Most input validation. This is a small function so use faster validation
  # variants:
  if (!is_tibble(update)) {
    # XXX debating about whether to have a specialized class for updates/diffs.
    # Seems nice for type-based reasoning and might remove some args from
    # interfaces, but would require constructor/converter functions for that
    # type.
    cli_abort("`update` must be a tibble")
  }
  if (is.null(snapshot)) {
    return(update)
  }
  if (!is_tibble(snapshot)) {
    cli_abort("`snapshot` must be a tibble")
  }
  if (!is.character(ukey_names) || !all(ukey_names %in% names(snapshot))) {
    cli_abort("`ukey_names` must be a subset of column names")
  }

  result_tbl <- vec_rbind(update, snapshot)

  dup_ids <- vec_duplicate_id(result_tbl[ukey_names])
  not_overwritten <- dup_ids == vec_seq_along(result_tbl)
  result_tbl <- result_tbl[not_overwritten, ]

  ## result_tbl <- arrange_canonical(result_tbl)

  result_tbl
}

epi_patch <- function(snapshot, update) {
  ukey_names <- key_colnames(update)
  dplyr_reconstruct(tbl_patch(as_tibble(snapshot), as_tibble(update), ukey_names), update)
}
