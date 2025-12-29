# NOTE: when providing a method for a generic in another package
# That generic needs to be rexported


# tsibble -----------------------------------------------------------------

#' @importFrom tsibble as_tsibble
#' @export
tsibble::as_tsibble




# dplyr -------------------------------------------------------------------

#' @importFrom dplyr arrange
#' @export
dplyr::arrange

#' @importFrom dplyr filter
#' @export
dplyr::filter

#' @importFrom dplyr group_by
#' @export
dplyr::group_by

#' @importFrom dplyr ungroup
#' @export
dplyr::ungroup

#' @importFrom dplyr group_modify
#' @export
dplyr::group_modify

#' @importFrom dplyr mutate
#' @export
dplyr::mutate

#' @importFrom dplyr relocate
#' @export
dplyr::relocate

#' @importFrom dplyr rename
#' @export
dplyr::rename

#' @importFrom dplyr slice
#' @export
dplyr::slice


# tidyr -------------------------------------------------------------------

#' @importFrom tidyr unnest
#' @export
tidyr::unnest


#' @importFrom tidyr complete
#' @export
tidyr::complete

# We don't provide a method for full_seq, but complete-ing using
# full_seq(time_value) is still needed to make some downstream things behave
# nicely.  So make that more ergonomic/discoverable with a re-export:

#' @importFrom tidyr full_seq
#' @export
tidyr::full_seq


# ggplot2 -----------------------------------------------------------------

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' Bind multiple data frames by row and column
#'
#' These are S3 generics around `dplyr::bind_rows` and `dplyr::bind_cols`.
#'
#' @inheritParams dplyr::bind_rows
#' @inheritParams dplyr::bind_cols
#' @seealso [`dplyr::bind_rows()`], [`dplyr::bind_cols()`]
#' @name bind
#' @importFrom dplyr bind_rows
#' @export
bind_rows <- function(..., .id = NULL) {
    UseMethod("bind_rows")
}

#' @rdname bind
#' @importFrom dplyr bind_cols
#' @export
bind_cols <- function(
  ...,
  .name_repair = c("unique", "universal", "check_unique", "minimal")
) {
    UseMethod("bind_cols")
}
