#' Convert a line list to an `epi_archive` object
#'
#' @description
#' Converts a line list (a data frame where each row represents a case or event)
#' into an [`epi_archive`] object. This function requires "recorded" and
#' "deleted" timestamps, and generates a time series of counts (e.g. daily
#' hospitalizations) as they would have appeared at different points in time.
#'
#' @param x A data frame (line list).
#' @param ... Arguments passed to [`new_epi_archive`].
#' @param geo_value,time_value,version_recorded,version_deleted,other_keys
#'   <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns in `x` representing:
#'   * `geo_value`: the geographic location of the event.
#'   * `time_value`: the time of the event.
#'   * `version_recorded`: the time at which the event became known/recorded.
#'   * `version_deleted`: (optional) the time at which the event was
#'     removed/deleted. If `NULL` (default), it is assumed no events are
#'     deleted.
#'   * `other_keys`: (optional) additional key columns (e.g. age group).
#' @param is_deleted (optional) <[`tidy-select`][dplyr::dplyr_tidy_select]>
#'   Column in `x` (or a logical vector/predicate) indicating if the row is a
#'   deletion (`TRUE`/1) or an entry (`FALSE`/0). Only used for "chart-style"
#'   linelists (where `version_recorded` and `version_deleted` are the same
#'   column).
#' @param value Either `NULL` (default) or a string specifying the name of the
#'   output count column. If `NULL` and `other_keys` is empty, defaults to
#'   "count".
#' @param id <[`tidy-select`][dplyr::dplyr_tidy_select]> Optional column
#'   identifying unique events/cases.
#'
#' @return An [`epi_archive`] object.
#'
#' @examples
#' library(dplyr)
#'
#' linelist <- tibble(
#'   event_id = 1:3,
#'   geo_value = c("ca", "ca", "ca"),
#'   time_value = as.Date(c("2022-01-01", "2022-01-01", "2022-01-02")),
#'   report_date = as.Date(c("2022-01-02", "2022-01-02", "2022-01-03")),
#'   delete_date = as.Date(c("2022-01-04", NA, NA))
#' )
#'
#' archive <- linelist_to_archive(
#'   linelist,
#'   geo_value = geo_value,
#'   time_value = time_value,
#'   version_recorded = report_date,
#'   version_deleted = delete_date
#' )
#'
#' @importFrom rlang enquo as_label eval_tidy .data
#' @importFrom dplyr transmute select bind_rows group_by summarise arrange mutate ungroup
#' @importFrom tidyselect eval_select
#' @importFrom utils head
#' @export
linelist_to_archive <- function(x,
                                ...,
                                geo_value = NULL,
                                time_value = NULL,
                                version_recorded = NULL,
                                version_deleted = NULL,
                                is_deleted = NULL,
                                other_keys = NULL,
                                value = NULL,
                                id = NULL) {
  # Capture tidy selections
  geo_quo <- rlang::enquo(geo_value)
  time_quo <- rlang::enquo(time_value)
  ver_rec_quo <- rlang::enquo(version_recorded)
  ver_del_quo <- rlang::enquo(version_deleted)
  is_del_quo <- rlang::enquo(is_deleted)
  id_quo <- rlang::enquo(id)

  # Use 0-row slice for resolution to be explicit about not needing data rows
  x_schema <- head(x, 0)

  geo_col <- resolve_col(geo_quo, x_schema, "geo_value",
    default_names = geo_column_names()
  )
  time_col <- resolve_col(time_quo, x_schema, "time_value",
    default_names = time_column_names()
  )
  ver_rec_col <- resolve_col(ver_rec_quo, x_schema, "version_recorded",
    default_names = version_column_names()
  )
  ver_del_col <- resolve_col(ver_del_quo, x_schema, "version_deleted",
    required = FALSE
  )
  is_del_col <- resolve_col(is_del_quo, x_schema, "is_deleted", required = FALSE)
  id_col <- resolve_col(id_quo, x_schema, "id", required = FALSE)

  # other_keys
  quo_other <- rlang::enquo(other_keys)
  other_cols <- names(dplyr::select(x_schema, !!quo_other))

  # This avoids copy when filtering/processing x
  needed_cols <- c(
    geo_col, time_col, ver_rec_col, ver_del_col, is_del_col, id_col, other_cols
  )
  x <- x %>% dplyr::select(dplyr::all_of(unique(needed_cols)))

  if (!is.null(value)) {
    checkmate::assert_string(value)
  } else {
    value <- "n" # default
  }

  # Validation

  # Either we are chart style, and ver_rec_col == ver_del_col and we
  # require no NAs, or we are non-chart style, in which case
  # ver_rec_col should have no NAs and ver_del_col could have NAs.
  if (anyNA(x[[ver_rec_col]])) {
    cli::cli_abort("`{ver_rec_col}` must not contain NAs.",
                   class = "epiprocess__linelist_to_archive__ver_rec_had_nas")
  }
  validate_linelist_ids(x, id_col, geo_col, other_cols, time_col, ver_rec_col, ver_del_col, is_del_col, value)

  # Extract updates
  updates <- extract_linelist_updates(
    x, ver_rec_col, ver_del_col, is_del_col, geo_col, time_col, other_cols
  )


  # Get all unique keys found in updates
  epikeys_df <- updates %>%
    dplyr::distinct(geo_value, dplyr::across(dplyr::all_of(other_cols)))

  # Get the first time a time_value is recorded version for each time value.
  time_intros <- updates %>%
    dplyr::group_by(time_value) %>%
    dplyr::summarise(version = min(version), .groups = "drop")

  # Create zero-change filler rows
  zeros <- tidyr::expand_grid(epikeys_df, time_intros) %>%
    dplyr::mutate(change = 0)

  updates <- dplyr::bind_rows(
    zeros,
    if (!is.null(ver_del_col) || !is.null(is_del_col)) {
      updates %>% dplyr::filter(change != 0L) # compactify if recorded+deleted offset to 0
    } else {
      updates
    }
  )

  # Groups: geo, time, other_keys, version
  grp_vars <- c("geo_value", "time_value", other_cols, "version")

  # collapse updates at same version
  collapsed <- updates %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_vars))) %>%
    dplyr::summarise(change = sum(change), .groups = "drop")

  # Now cumsum over version for each key vars
  series_vars <- c("geo_value", "time_value", other_cols)

  final_df <- collapsed %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(series_vars))) %>%
    dplyr::arrange(version, .by_group = TRUE) %>%
    dplyr::mutate(!!value := cumsum(change)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"change") %>%
    as.data.frame()
  # We've done so much manipulation there should be no chance we alias
  # pre-existing columns ==> We own `final_df` and its columns ==> We
  # can mutate `final_df`, and we obey `data.table`'s memory model.
  data.table::setDT(final_df, c("geo_value", other_keys, "time_value", "version"))

  # Pass ... to new_epi_archive
  new_epi_archive(final_df, other_keys = other_cols, ...)
}

# Helper to resolve selection to a single string
resolve_col <- function(
  quo, data, arg_name, required = TRUE, default_names = NULL
) {
  # If the user didn't supply it or explicitly set it to `NULL`, the quo might be the default expression
  if (identical(rlang::quo_get_expr(quo), rlang::expr(NULL))) {
    selected_colnames <- vctrs::vec_set_intersect(names(data), default_names)
    if (length(selected_colnames) == 0L) {
      if (required) {
        cli::cli_abort("Could not automatically select column for `{arg_name}`; please specify it manually.",
                       class = "epiprocess__resolve_col__autoselection_failed")
      } else {
        return(NULL)
      }
    } else {
      cli::cli_alert_info("Defaulting to {qty(length(selected_colnames))} col{?s} {.var {selected_colnames}} as {.var {arg_name}}.")
    }
  } else { # user supplied a non-`NULL` argument
    selected_colnames <- names(eval_select(quo, data, allow_rename = FALSE))
  }

  if (length(selected_colnames) > 1) {
    cli::cli_abort("Selection for `{arg_name}` must match exactly one column.",
                   class = "epiprocess__resolve_col__selected_multiple")
  }

  selected_colnames
}

# Helper to extract and renamed
extract_standard <- function(df, v_col, change_val, geo_col,
                             time_col, other_cols) {
  if (is.null(v_col)) {
    return(NULL)
  }

  # Filter out NAs in version col
  df <- df[!is.na(df[[v_col]]), ]
  if (nrow(df) == 0) {
    return(NULL)
  }

  # Select cols
  sel_cols <- c(geo_col, time_col, other_cols, v_col)
  out <- df[sel_cols]

  # Rename to standard
  names(out) <- c("geo_value", "time_value", other_cols, "version")

  out %>%
    dplyr::count(
      geo_value, time_value, dplyr::across(dplyr::all_of(other_cols)), version,
      name = "change"
    ) %>%
    dplyr::mutate(change = change * change_val)
}

# Helper to extract updates
extract_linelist_updates <- function(x, ver_rec_col, ver_del_col, is_del_col,
                                     geo_col, time_col, other_cols) {
  # Here we use an optional variable to classify the event as deleted
  # or not in cases where it resembles a "chart" where each row is an update to
  # a patient record.
  if (!is.null(ver_del_col) && ver_rec_col == ver_del_col) {
    # Chart-style

    is_del_vals <- as.logical(x[[is_del_col]])

    entries <- extract_standard(
      x[!is_del_vals, ], ver_rec_col, 1, geo_col, time_col, other_cols
    )
    removals <- extract_standard(
      x[is_del_vals, ], ver_del_col, -1, geo_col, time_col, other_cols
    )
  } else {
    # Interval-style: separate columns (or no deletion column)
    entries <- extract_standard(x, ver_rec_col, 1, geo_col, time_col, other_cols)
    removals <- extract_standard(x, ver_del_col, -1, geo_col, time_col, other_cols)
  }

  updates <- dplyr::bind_rows(entries, removals)

  if (nrow(updates) == 0) {
    cli::cli_abort("No valid updates found.")
  }

  updates
}

# Helper to validate IDs
validate_linelist_ids <- function(x, id_col, geo_col, other_cols, time_col, ver_rec_col, ver_del_col, is_del_col = NULL, value) {
  is_chart_style <- !is.null(ver_del_col) && ver_rec_col == ver_del_col

  if (is_chart_style) {
    if (is.null(is_del_col)) {
      cli::cli_abort("If `version_recorded` and `version_deleted` are the same column, `is_deleted` must be provided.")
    }

    # is_del must be valid
    raw_is_del <- x[[is_del_col]]
    if (anyNA(raw_is_del)) cli::cli_abort("`{is_del_col}` must not contain NAs.")
    is_del <- as.logical(raw_is_del)
    if (anyNA(is_del)) cli::cli_abort("`{is_del_col}` must be coercible to logical without generating NAs.")

    # Masks
    entries_mask <- !is_del
    removals_mask <- is_del

    msg_dup_ent <- "Each `id` must have at most one entry (where `{is_del_col}` is FALSE)."
    msg_dup_rem <- "Each `id` must have at most one removal (where `{is_del_col}` is TRUE)."
    if (!is.null(id_col)) {
      contribs_df <- x %>%
        group_by(pick(all_of(c(id_col, time_col)))) %>%
        arrange(ver_rec_col) %>%
        mutate(!!value := !.data[[is_del_col]] - .data[[is_del_col]]) %>%
        ungroup()
      if (any(contribs_df[[value]] < 0L)) {
        cli_abort("An event was deleted before it was recorded, or was deleted and recorded with inconsistent time values.")
      }
    } # else do a similar check by geo_value x other_keys x time_value?
  } else {
    if (!is.null(id_col) && vctrs::vec_duplicate_any(x[[id_col]])) {
      cli_abort("Each `id` must have at most one entry (in non-chart-style linelist).")
    }
    if (!is.null(ver_del_col) && any(!is.na(x[[ver_del_col]]) & x[[ver_del_col]] < x[[ver_rec_col]])) {
      cli_abort("`{ver_del_col}` (removal) must be >= `{ver_rec_col}` (entry).")
    }
  }
}
utils::globalVariables("change")
