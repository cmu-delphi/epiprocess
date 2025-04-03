#' Test two vctrs vectors for equality with some tolerance in some cases
#'
#' Generalizes [`vctrs::vec_equal`].
#'
#' @param vec1,vec2 vctrs vectors (includes data frames). Take care when using
#'   on named vectors or "keyed" data frames; [`vec_names()`] are largely
#'   ignored, and key columns are treated as normal value columns (when they
#'   should probably generate an error if they are not lined up correctly, or be
#'   tested for exact rather than approximate equality).
#' @param na_equal should `NA`s be considered equal to each other? (In
#'   epiprocess, we usually want this to be `TRUE`, but that doesn't match the
#'   [`vctrs::vec_equal()`] default, so this is mandatory.)
#' @param .ptype as in [`vctrs::vec_equal()`].
#' @param ... should be empty (it's here to force later arguments to be passed
#'   by name)
#' @param abs_tol absolute tolerance; will be used for bare numeric `vec1`,
#'   `vec2`, or any such columns within `vec1`, `vec2` if they are data frames.
#' @param inds1,inds2 optional (row) indices into vec1 and vec2 compatible with
#'   [`vctrs::vec_slice()`]; output should be consistent with `vec_slice`-ing to
#'   these indices beforehand, but can give faster computation if `vec1` and
#'   `vec2` are data frames. Currently, any speedup is only by making sure that
#'   `vec_slice` is used rather than `[` for data frames.
#'
#' @return logical vector, with length matching the result of recycling `vec1`
#'   (at `inds1` if provided) and `vec2` (at `inds2` if provided); entries
#'   should all be `TRUE` or `FALSE` if `na_equal = TRUE`.
#'
#' @examples
#'
#' library(dplyr)
#'
#' # On numeric vectors:
#' vec_approx_equal(
#'   c(1, 2, 3, NA),
#'   c(1, 2 + 1e-10, NA, NA),
#'   na_equal = TRUE,
#'   abs_tol = 1e-8
#' )
#'
#' # On tibbles:
#' tbl1 <- tibble(
#'   a = 1:5,
#'   b = list(1:5, 1:4, 1:3, 1:2, 1:1) %>% lapply(as.numeric),
#'   c = tibble(
#'     c1 = 1:5
#'   ),
#'   d = matrix(1:10, 5, 2)
#' )
#' tbl2 <- tbl1
#' tbl2$a[[2]] <- tbl1$a[[2]] + 1e-10
#' tbl2$b[[3]][[1]] <- tbl1$b[[3]][[1]] + 1e-10
#' tbl2$c$c1[[4]] <- tbl1$c$c1[[4]] + 1e-10
#' tbl2$d[[5, 2]] <- tbl1$d[[5, 2]] + 1e-10
#' vctrs::vec_equal(tbl1, tbl2, na_equal = TRUE)
#' vec_approx_equal(tbl1, tbl2, na_equal = TRUE, abs_tol = 1e-12)
#' vec_approx_equal(tbl1, tbl2, na_equal = TRUE, abs_tol = 1e-8)
#'
#'
#'
#'
#'
#' # Type comparison within lists is stricter, matching vctrs:
#' vctrs::vec_equal(list(1:2), list(as.numeric(1:2)))
#' vec_approx_equal(list(1:2), list(as.numeric(1:2)), FALSE, abs_tol = 0)
#'
#' @export
vec_approx_equal <- function(vec1, vec2, na_equal, .ptype = NULL, ..., abs_tol, inds1 = NULL, inds2 = NULL) {
  if (!obj_is_vector(vec1)) cli_abort("`vec1` must be recognized by vctrs as a vector")
  if (!obj_is_vector(vec2)) cli_abort("`vec2` must be recognized by vctrs as a vector")
  # Leave vec size checking to vctrs recycling ops.
  assert_logical(na_equal, any.missing = FALSE, len = 1L)
  # Leave .ptype checks to cast operation.
  check_dots_empty()
  assert_numeric(abs_tol, lower = 0, len = 1L)
  assert(
    check_null(inds1),
    check_numeric(inds1),
    check_logical(inds1),
    check_character(inds1)
  )
  assert(
    check_null(inds2),
    check_numeric(inds2),
    check_logical(inds2),
    check_character(inds2)
  )
  # Leave heavier index validation to the vctrs recycling & indexing ops.

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
  if (!identical(vec_ptype(vec1), vec_ptype(vec2)) || !is.null(.ptype)) {
    # perf: this is slow, so try to avoid it if it's not needed
    vecs <- vec_cast_common(vec1, vec2, .to = .ptype)
  } else {
    vecs <- list(vec1, vec2)
  }
  vec_approx_equal0(vecs[[1]], vecs[[2]], na_equal, abs_tol, inds1, inds2)
}

#' Helper for [`vec_approx_equal`] for vecs guaranteed to have the same ptype and size
#'
#' @keywords internal
vec_approx_equal0 <- function(vec1, vec2, na_equal, abs_tol, inds1 = NULL, inds2 = NULL) {
  if (is_bare_numeric(vec1) && abs_tol != 0) {
    # Matching vec_equal, we ignore names and other attributes.
    if (!is.null(inds1)) vec1 <- vec_slice(vec1, inds1)
    if (!is.null(inds2)) vec2 <- vec_slice(vec2, inds2)
    na_or_nan1 <- is.na(vec1)
    na_or_nan2 <- is.na(vec2)
    # Since above are bare logical vectors, we can use `fifelse`
    if (na_equal) {
      res <- fifelse(
        !na_or_nan1 & !na_or_nan2,
        abs(vec1 - vec2) <= abs_tol,
        na_or_nan1 & na_or_nan2 & (is.nan(vec1) == is.nan(vec2))
      )
    } else {
      # Like `==` and `vec_equal`, we consider NaN == {NA, NaN, anything else}
      # to be NA.
      res <- fifelse(
        !na_or_nan1 & !na_or_nan2,
        abs(vec1 - vec2) <= abs_tol,
        NA
      )
    }

    if (!is.null(dim(vec1))) {
      dim(res) <- dim(vec1)
      res <- rowSums(res) == ncol(res)
    }
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
        vec_approx_equal0(vec1[[col_i]], vec2[[col_i]], na_equal, abs_tol, inds1, inds2)
      }))
    }
  } else if (is_bare_list(vec1) && abs_tol != 0) {
    vapply(seq_along(vec1), function(i) {
      entry1 <- vec1[[i]]
      entry2 <- vec2[[i]]
      vec_size(entry1) == vec_size(entry2) &&
        # Trying to follow vec_equal: strict on ptypes aside from vec_namedness:
        identical(
          vec_set_names(vec_ptype(entry1), NULL),
          vec_set_names(vec_ptype(entry2), NULL)
        ) &&
        all(vec_approx_equal0(entry1, entry2, na_equal, abs_tol))
    }, logical(1L))
  } else {
    # No special handling for any other types/situations. We may want to allow
    # S3 extension of this method or of a new appropriate vec_proxy_* variant.
    # See Issue #640.
    if (!is.null(inds1)) {
      vec1 <- vec_slice(vec1, inds1)
    }
    if (!is.null(inds2)) {
      vec2 <- vec_slice(vec2, inds2)
    }
    # perf: vec1 and vec2 have already been cast to a common ptype; we can't
    # disable casts, but can say to cast (again...) to that ptype
    res <- vec_equal(vec1, vec2, na_equal = na_equal, vec_ptype(vec1))
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
#'
#' @keywords internal
tbl_diff2 <- function(earlier_snapshot, later_tbl,
                      ukey_names,
                      later_format = c("snapshot", "update"),
                      compactify_abs_tol = 0) {
  # Most input validation + handle NULL earlier_snapshot. This is a small function so
  # use faster validation variants:
  if (!is_tibble(later_tbl)) {
    cli_abort("`later_tbl` must be a tibble",
      class = "epiprocess__tbl_diff2__later_tbl_invalid"
    )
  }
  if (is.null(earlier_snapshot)) {
    return(later_tbl)
  }
  if (!is_tibble(earlier_snapshot)) {
    cli_abort("`earlier_snapshot` must be a tibble or `NULL`",
      class = "epiprocess__tbl_diff2__earlier_tbl_class_invalid"
    )
  }
  if (!is.character(ukey_names) || !all(ukey_names %in% names(earlier_snapshot))) {
    cli_abort("`ukey_names` must be a subset of column names",
      class = "epiprocess__tbl_diff2__ukey_names_class_invalid"
    )
  }
  later_format <- arg_match0(later_format, c("snapshot", "update"))
  if (!(is.vector(compactify_abs_tol, mode = "numeric") &&
    length(compactify_abs_tol) == 1L && # nolint:indentation_linter
    compactify_abs_tol >= 0)) {
    # Give a specific message:
    assert_numeric(compactify_abs_tol, lower = 0, any.missing = FALSE, len = 1L)
    # Fallback e.g. for invalid classes not caught by assert_numeric:
    cli_abort("`compactify_abs_tol` must be a length-1 double/integer >= 0",
      class = "epiprocess__tbl_diff2__compactify_abs_tol_invalid"
    )
  }

  # Extract metadata:
  earlier_n <- nrow(earlier_snapshot)
  later_n <- nrow(later_tbl)
  tbl_names <- names(earlier_snapshot)
  val_names <- tbl_names[!tbl_names %in% ukey_names]

  # More input validation:
  if (!identical(tbl_names, names(later_tbl))) {
    cli_abort(c(
      "`earlier_snapshot` and `later_tbl` should have identical column
       names and ordering.",
      "*" = "`earlier_snapshot` colnames: {format_chr_deparse(tbl_names)}",
      "*" = "`later_tbl` colnames: {format_chr_deparse(names(later_tbl))}"
    ), class = "epiprocess__tbl_diff2__tbl_names_differ")
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
  # seq_len(combined_n) (cases 1., 2., or 5.) except for when that ukey has been
  # re-reported in `later_tbl`, in which case (3. or 4.) it will point back to
  # the row index of the same ukey in `earlier_snapshot`:
  combined_ukey_firsts <- vec_duplicate_id(combined_ukeys)

  # Which rows from combined are cases 3. or 4.?
  combined_ukey_is_repeat <- combined_ukey_firsts != seq_len(combined_n)
  # For each row in 3. or 4., row numbers of the ukey appearance in earlier:
  ukey_repeat_first_i <- combined_ukey_firsts[combined_ukey_is_repeat]

  # Which rows from combined are in case 3.?
  combined_compactify_away <- rep(FALSE, combined_n)
  combined_compactify_away[combined_ukey_is_repeat] <-
    vec_approx_equal0(combined_vals,
      combined_vals,
      na_equal = TRUE,
      abs_tol = compactify_abs_tol,
      inds1 = combined_ukey_is_repeat,
      inds2 = ukey_repeat_first_i
    )

  # Which rows from combined are in cases 3., 4., or 5.?
  combined_from_later <- vec_rep_each(c(FALSE, TRUE), c(earlier_n, later_n))

  if (later_format == "update") {
    # Cases 4. and 5.:
    combined_tbl <- combined_tbl[combined_from_later & !combined_compactify_away, ]
  } else { # later_format is "snapshot"
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
#'
#' @keywords internal
tbl_patch <- function(snapshot, update, ukey_names) {
  # Most input validation. This is a small function so use faster validation
  # variants:
  if (!is_tibble(update)) {
    cli_abort("`update` must be a tibble",
      class = "epiprocess__tbl_patch__update_class_invalid"
    )
  }
  if (is.null(snapshot)) {
    return(update)
  }
  if (!is_tibble(snapshot)) {
    cli_abort("`snapshot` must be a tibble",
      class = "epiprocess__tbl_patch__snapshot_class_invalid"
    )
  }
  if (!is.character(ukey_names) || !all(ukey_names %in% names(snapshot))) {
    cli_abort("`ukey_names` must be a subset of column names",
      class = "epiprocess__tbl_patch__ukey_names_invalid"
    )
  }
  if (!identical(names(snapshot), names(update))) {
    cli_abort(c(
      "`snapshot` and `update` should have identical column
       names and ordering.",
      "*" = "`snapshot` colnames: {format_chr_deparse(tbl_names)}",
      "*" = "`update` colnames: {format_chr_deparse(names(update))}"
    ), class = "epiprocess__tbl_patch__tbl_names_invalid")
  }

  result_tbl <- vec_rbind(update, snapshot)

  dup_ids <- vec_duplicate_id(result_tbl[ukey_names])
  # check that the index hasn't be reset to something lower that it duplicates
  not_overwritten <- dup_ids == vec_seq_along(result_tbl)
  result_tbl <- result_tbl[not_overwritten, ]

  result_tbl
}
