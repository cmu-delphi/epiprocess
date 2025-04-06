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

#' Variation on [`dplyr::anti_join`] for speed + tolerance setting
#'
#' @param x tibble; `x[ukey_names]` must not have any duplicate rows
#' @param y tibble; `y[ukey_names]` must not have any duplicate rows
#' @param ukey_names chr; names of columns that form a unique key, for `x` and
#'   for `y`
#' @param val_names chr; names of columns which should be treated as
#'   value/measurement columns, and compared with a tolerance
#' @param abs_tol scalar non-negative numeric; absolute tolerance with which to
#'   compare value columns; see [`vec_approx_equal`]
#' @return rows from `x` that either (a) don't have a (0-tolerance) matching
#'   ukey in `y`, or (b) have a matching ukey in `y`, but don't have
#'   approximately equal value column values
#'
#' @keywords internal
tbl_fast_anti_join <- function(x, y, ukey_names, val_names, abs_tol = 0) {
  x_keyvals <- x[c(ukey_names, val_names)]
  y_keyvals <- y[c(ukey_names, val_names)]
  xy_keyvals <- vec_rbind(x, y)
  if (abs_tol == 0) {
    # perf: 0 tolerance is just like a normal `anti_join` by both ukey_names and
    # val_names together. We can do that more quickly than `anti_join` with
    # `vctrs` by checking for keyvals of `x` that are not duplicated in `y`.
    # (`vec_duplicate_detect` will mark those, unlike `duplicated`.)
    x_exclude <- vec_duplicate_detect(xy_keyvals)
    x_exclude <- vec_slice(x_exclude, seq_len(nrow(x)))
  } else {
    xy_ukeys <- xy_keyvals[ukey_names]
    # Locate ukeys in `y` that match ukeys in `x` and where in `x` they map back
    # to. It's faster to do this with `vec_duplicate_id` on `xy_ukeys` than to
    # perform a `inner_join`.
    xy_ukey_dup_ids <- vec_duplicate_id(xy_ukeys)
    xy_ukey_dup_inds2 <- which(xy_ukey_dup_ids != seq_along(xy_ukey_dup_ids))
    # ^ these should point to rows from y that had a ukey match in x
    xy_ukey_dup_inds1 <- xy_ukey_dup_ids[xy_ukey_dup_inds2]
    # ^ these should point to the respectively corresponding rows from x

    # Anything in `x` without a ukey match in `y` should be kept; start off with
    # `FALSE` for everything and just fill in `TRUE`/`FALSE` results for the
    # ukeys with matches in `y`:
    x_exclude <- rep(FALSE, nrow(x))
    xy_vals <- xy[val_names]
    x_exclude[xy_ukey_dup_inds1] <- vec_approx_equal(
      xy_vals,
      inds1 = xy_ukey_dup_inds2,
      xy_vals,
      inds2 = xy_ukey_dup_inds1,
      na_equal = TRUE, abs_tol = abs_tol
    )
  }
  vec_slice(x_orig, !x_exclude)
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
    cli_abort(
      "`later_tbl` must be a tibble",
      class = "epiprocess__tbl_diff2__later_tbl_invalid"
    )
  }
  if (is.null(earlier_snapshot)) {
    return(later_tbl)
  }
  if (!is_tibble(earlier_snapshot)) {
    cli_abort(
      "`earlier_snapshot` must be a tibble or `NULL`",
      class = "epiprocess__tbl_diff2__earlier_tbl_class_invalid"
    )
  }
  if (!is.character(ukey_names) || !all(ukey_names %in% names(earlier_snapshot))) {
    cli_abort(
      "`ukey_names` must be a subset of column names",
      class = "epiprocess__tbl_diff2__ukey_names_class_invalid"
    )
  }
  later_format <- arg_match0(later_format, c("snapshot", "update"))
  if (!(is.vector(compactify_abs_tol, mode = "numeric") &&
    length(compactify_abs_tol) == 1L && # nolint: indentation_linter
    compactify_abs_tol >= 0)) {
    # Give a specific message:
    assert_numeric(compactify_abs_tol, lower = 0, any.missing = FALSE, len = 1L)
    # Fallback e.g. for invalid classes not caught by assert_numeric:
    cli_abort(
      "`compactify_abs_tol` must be a length-1 double/integer >= 0",
      class = "epiprocess__tbl_diff2__compactify_abs_tol_invalid"
    )
  }

  all_names <- names(later_tbl)
  val_names <- all_names[!(all_names %in% ukey_names)]
  updates <- tbl_fast_anti_join(later_tbl, earlier_snapshot, ukey_names, val_names, compactify_abs_tol)
  if (later_format == "snapshot") {
    # Interpret `later_tbl` as a full snapshot, rather than a diff / sparse
    # update. That means that any ukeys in `earlier_snapshot` that don't appear
    # in `later_tbl` were deleted in the later snapshot.
    deletions <- tbl_fast_anti_join(earlier_snapshot[ukey_names], later_tbl[ukey_names], ukey_names, character(), 0)
    updates <- vec_rbind(updates, deletions) # fills val cols with NAs
  }
  # If `later_format == "update"`, we don't need to do anything special about
  # the above ukeys. The full snapshot for the later version would include the
  # corresponding rows unchanged, and the diff for these unchanged rows would be
  # empty.
  updates
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
    cli_abort(
      "`update` must be a tibble",
      class = "epiprocess__tbl_patch__update_class_invalid"
    )
  }
  if (is.null(snapshot)) {
    return(update)
  }
  if (!is_tibble(snapshot)) {
    cli_abort(
      "`snapshot` must be a tibble",
      class = "epiprocess__tbl_patch__snapshot_class_invalid"
    )
  }
  if (!is.character(ukey_names) || !all(ukey_names %in% names(snapshot))) {
    cli_abort(
      "`ukey_names` must be a subset of column names",
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
  # Find the "first" appearance of each ukey; since `update` is ordered before `snapshot`,
  # this means favoring the rows from `update` over those in `snapshot`.
  # This is like `!duplicated()` but faster, and like `vec_unique_loc()` but guaranteeing
  # that we get the first appearance since `vec_duplicate_id()` guarantees that
  # it points to the first appearance.
  is_only_or_favored_appearance <- dup_ids == vec_seq_along(result_tbl)
  result_tbl <- result_tbl[is_only_or_favored_appearance, ]

  result_tbl
}
