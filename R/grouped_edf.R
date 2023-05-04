#' Simple reclass function for `grouped_edf`s; typically favor `maybe` variant
#'
#' Ensures that result has class `grouped_edf`, but due to `dplyr`'s treatment
#' of trivial groupings, we may not want to always output `grouped_edf`
#' "when grouping".
#'
#' @param x Object to reclass to `grouped_edf`
#' @param potential_subsubclasses As in [`as_inheriting`]
#' @return A `grouped_edf`
#'
#' @noRd
reclass_definitely_grouped_edf = function(x, metadata, potential_subsubclasses=character(0L)) {
  as_inheriting(x, "grouped_edf", potential_subsubclasses)
}

#' Simple reclass function for `grouped_edf`s, unless not a `grouped_df`&`epi_df`
#'
#' Typically favor this over `reclass_definitely_grouped_edf`, as "grouping"
#' operations won't actually produce `grouped_df`s if it's a trival grouping,
#' and we want to add/drop the `grouped_edf` class in the same situations as
#' `grouped_df` for `epi_df`s. This is provided that we are indeed given an
#' `epi_df`; if instead we're given, e.g., something from
#' `dplyr_reconstruct.epi_df` that might have decayed to a non-`epi_df`, we
#' should not form a `grouped_edf`.
#'
#' @param x Object to reclass to `grouped_edf` if it's a `grouped_df`
#' @param potential_subsubclasses As in [`as_inheriting`]
#' @return A `grouped_edf` if `x` was a `grouped_df`; otherwise just `x`
#'
#' @noRd
reclass_maybe_grouped_edf = function(x, metadata, potential_subsubclasses=character(0L)) {
  if (inherits(x, "grouped_df") && inherits(x, "epi_df")) {
    reclass_definitely_grouped_edf(x, metadata, potential_subsubclasses)
  } else {
    x
  }
}

#' @rdname basic_s3_for_epi_df
#' @aliases grouped_edf
#' @export
group_by.epi_df = function(.data, ...) {
  metadata = attr(.data, "metadata")
  result = NextMethod()
  result <- reclass_epi_df(result, metadata, "grouped_df")
  result <- reclass_maybe_grouped_edf(result)
  result
  # reclass_epi_df(.data, metadata, "grouped_df")
  # reclass_epi_df(.data, metadata)
}

#' @importFrom dplyr group_data
#' @rdname basic_s3_for_epi_df
#' @export
group_data.grouped_edf = function(.data) {
  metadata = attr(.data, "metadata")
  reclass_epi_df(NextMethod(), metadata)
}

#' @importFrom dplyr dplyr_reconstruct
#' @export
#' @noRd
dplyr_reconstruct.grouped_edf = function(data, template) {
  # dplyr_reconstruct.grouped_df doesn't naturally call
  # dplyr_reconstruct.epi_df; do that first. We can't just call
  # dplyr_reconstruct.epi_df as it relies on NextMethod(), which relies on
  # actual S3 dispatch; get that S3 dispatch by calling the generic using a
  # tweaked version of `template` (the S3 dispatch arg for `dplyr_reconstruct`).
  template_class = class(template)
  edf_template = `class<-`(
    template,
    template_class[! template_class %in% c("grouped_edf", "grouped_df")]
  )
  data <- dplyr_reconstruct(data, edf_template)
  # `data` here is either `epi_df` or decayed. Now apply `grouped_df`
  # reconstruction (remember S3 dispatch arg is `template`):
  result = NextMethod()
  # Assume that dplyr_reconstruct.grouped_df hasn't done anything that would
  # cause an epi_df to need to decay (but won't output an `epi_df` class ever).
  # So just see if `data` was `epi_df` or decayed and mirror that by adding
  # `epi_df` class / not.
  if (inherits(data, "epi_df")) {
    metadata = attr(data, "metadata")
    result <- reclass_epi_df(result, metadata, "grouped_df")
  }
  result <- reclass_maybe_grouped_edf(result)
  result
}

#' @importFrom vctrs vec_restore
#' @export
#' @noRd
vec_restore.grouped_edf = function(x, to, ...) {
  dplyr_reconstruct(x, to)
}

#' @rdname basic_s3_for_epi_df
#' @export
ungroup.grouped_edf = function(x, ...) {
  metadata = attr(x, "metadata")
  result = NextMethod()
  result <- reclass_epi_df(result, metadata, "grouped_df")
  result <- reclass_maybe_grouped_edf(result)
  result
}

#' @export
`[.grouped_edf` = function(x, i, j, drop = FALSE) {
  dplyr_reconstruct(NextMethod(), x)
}

#' @export
`[[<-.grouped_edf` = function(x, ..., value) {
  dplyr_reconstruct(NextMethod(), x)
}

#' @export
`[<-.grouped_edf` = function(x, i, j, ..., value) {
  dplyr_reconstruct(NextMethod(), x)
}

#' @export
`$<-.grouped_edf` = function(x, name, ..., value) {
  dplyr_reconstruct(NextMethod(), x)
}

#' @importFrom dplyr do
#' @export
do.grouped_edf = function(.data, ...) {
  dplyr_reconstruct(NextMethod(), .data)
}

#' @export
dplyr_col_modify.grouped_edf = function(data, cols) {
  dplyr_reconstruct(NextMethod(), data)
}

#' @export
dplyr_row_slice.grouped_edf = function(data, i, ..., preserve = FALSE) {
  dplyr_reconstruct(NextMethod(), data)
}

#' @export
group_modify.grouped_edf = function(.data, .f, ..., .keep = FALSE, keep = deprecated()) {
  dplyr_reconstruct(NextMethod(), .data)
}

#' @importFrom dplyr group_trim
#' @export
group_trim.grouped_edf = function(.tbl, .drop = group_by_drop_default(.tbl)) {
  dplyr_reconstruct(NextMethod(), .tbl)
}

#' @export
`names<-.grouped_edf` = function(x, value) {
  dplyr_reconstruct(NextMethod(), x)
}

#' @export
summarise.grouped_edf = function(.data, ..., .by = NULL, .groups = NULL) {
  dplyr_reconstruct(NextMethod(), .data)
}
