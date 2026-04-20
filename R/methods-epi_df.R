#' Convert to tibble
#'
#' Converts an `epi_df` object into a tibble, dropping metadata, any
#' grouping, and any unrelated classes and attributes.
#'
#' Advanced: if you are working with a third-party package that uses
#' `as_tibble()` on `epi_df`s but you actually want them to remain `epi_df`s,
#' use `attr(your_epi_df, "decay_to_tibble") <- FALSE` beforehand.
#'
#' @param x an `epi_df`
#' @param ... if present, forwarded to [`tibble::as_tibble`]
#' @importFrom tibble as_tibble new_tibble
#' @importFrom rlang dots_n
#' @importFrom vctrs vec_data vec_size
#' @export
as_tibble.epi_df <- function(x, ...) {
  # Note that some versions of `tsibble` overwrite `as_tibble.grouped_df`, which
  # also impacts grouped `epi_df`s, so don't rely on `NextMethod()`. Destructure
  # and redispatch instead.
  destructured <- vec_data(x) # -> data.frame, dropping extra attrs
  tbl <- if (dots_n(...) == 0 &&
    is.null(pkgconfig::get_config("tibble::rownames"))) { # nolint: indentation_linter
    # perf: new_tibble instead of as_tibble.data.frame which performs
    # extra checks whose defaults should be redundant here:
    new_tibble(destructured)
    # (^ We don't need to provide nrow= as we have >0 columns.)
  } else {
    as_tibble(destructured, ...)
  }
  if (attr(x, "decay_to_tibble") %||% TRUE) {
    tbl
  } else {
    # We specially requested via attr not to decay epi_df-ness but to drop any
    # grouping. (Miscellaneous attrs are also dropped.)
    reclass(tbl, attr(x, "metadata"))
  }
}

#' Convert to tsibble format
#'
#' Converts an `epi_df` object into a tsibble, where the index is taken to be
#' `time_value`, and the key variables taken to be `geo_value` along with any
#' others in the `other_keys` field of the metadata, or else explicitly set.
#'
#' @method as_tsibble epi_df
#' @param x an `epi_df`
#' @param key Optional. Any additional keys (other than `geo_value`) to add to
#'   the `tsibble`.
#' @param ... additional arguments passed on to `tsibble::as_tsibble()`
#' @export
as_tsibble.epi_df <- function(x, key, ...) {
  if (missing(key)) key <- c("geo_value", attributes(x)$metadata$other_keys)
  as_tsibble(
    tibble::as_tibble(x),
    key = tidyselect::all_of(key), index = "time_value",
    ...
  )
}

#' Base S3 methods for an `epi_df` object
#'
#' Print and summary functions for an `epi_df` object.
#'
#' @param x an `epi_df`
#' @method print epi_df
#' @param ... additional arguments to forward to `NextMethod()`, or unused
#' @export
print.epi_df <- function(x, ...) {
  cat(
    "An `epi_df` object,", prettyNum(nrow(x), ","), "x",
    prettyNum(ncol(x), ","), "with metadata:\n"
  )
  cat(sprintf("* %-9s = %s\n", "geo_type", attributes(x)$metadata$geo_type))
  cat(sprintf("* %-9s = %s\n", "time_type", attributes(x)$metadata$time_type))
  ok <- attributes(x)$metadata$other_keys
  if (length(ok) > 0) {
    cat(sprintf("* %-9s = %s\n", "other_keys", paste(ok, collapse = ", ")))
  }
  cat(sprintf("* %-9s = %s\n", "as_of", attributes(x)$metadata$as_of))
  # Conditional output (silent if attribute is NULL):
  cat(sprintf("* %-9s = %s\n", "decay_to_tibble", attr(x, "decay_to_tibble")))
  # Latency info:
  # Note: sections below use tryCatch as a defensive programming measure.
  tryCatch(
    print_latency_info(x),
    error = function(e) NULL
  )
  cat("\n")
  NextMethod()
}

# Internal helper for print.epi_df — compact aggregate view
print_latency_info <- function(x) {
  md <- attr(x, "metadata")
  as_of <- md$as_of
  as_of_valid <- !is.null(as_of) && !is.na(as_of) && (length(as_of) > 0)
  integer_time <- isTRUE(md$time_type %in% c("integer", "custom"))

  keys <- key_colnames(x)
  key_no_t <- setdiff(keys, "time_value")
  sigs <- setdiff(names(x), keys)
  # Exclude complex columns from signal calculations
  sigs <- sigs[vapply(
    x[sigs],
    function(col) is.numeric(col) || is.logical(col),
    logical(1)
  )]
  if (!as_of_valid) {
    return(invisible(NULL))
  }

  # Get min/max non-NA time_value per (keys × signal).
  smry_long <- epi_ts_range(x, key_no_t, sigs)

  # Compute lags for each time series in natural units
  combo_lags <- time_minus_time_in_n_steps(
    as_of, smry_long$max_t, md$time_type,
    # This prevents problems with latency calculations
    require_integer = FALSE
  )

  # Check for empty time series and create message
  empty_serie <- dplyr::case_when(
    all(smry_long$empty) ~ "* No time series detected",
    any(smry_long$empty) ~ "* Empty time series detected",
    TRUE ~ ""
  )

  # Remove NA lags
  combo_lags <- combo_lags[!is.na(combo_lags) & !smry_long$empty]

  if (length(combo_lags) >= 1) {
    # Format the lag range
    unit_format <- if (integer_time) "" else time_type_unit_pluralizer[[md$time_type]] %||% ""
    # For a range, we'll generally want the plural label
    unit <- if (integer_time) "" else cli::pluralize(paste0(" {qty(2)}", unit_format))
    lag_min <- as.integer(min(combo_lags))
    lag_max <- as.integer(max(combo_lags))

    # Create the range string
    range_str <- if (lag_min == lag_max) {
      sprintf("%d%s", lag_min, unit)
    } else {
      sprintf("%d\u2013%d%s", lag_min, lag_max, unit)
    }

    # Add hint if the lag range spread is large
    notable_threshold <- if (isTRUE(md$time_type == "day")) 7 else 2
    hint <- if ((lag_max - lag_min) >= notable_threshold) " (see summary() for per-signal details)" else ""
    # Check if there are multiple signals
    if (length(sigs) <= 1 || nrow(na.omit(x[sigs])) == 0) {
      sent_series <- ""
    } else {
      sent_series <- "across all time series"
    }

    lag_msg <- sprintf("* lag %s = %s%s\n", sent_series, range_str, hint)
  } else {
    lag_msg <- ""
  }
  # Print the latency info
  cat("Latency (lag from as_of to latest observation by time series):\n")
  cat(lag_msg)
  cat(empty_serie)
}

# Min/max non-NA time_value per (epikey x signal).
epi_ts_range <- function(x, key_no_t, sigs) {
  smry <- x %>%
    dplyr::ungroup()

  # long format for empty sigs and all NA sigs
  if (!(length(sigs) == 0)) {
    smry <- smry %>%
      tidyr::pivot_longer(dplyr::all_of(sigs),
        names_to = "..sig",
        values_to = "..val"
      )
  } else {
    smry <- smry %>%
      dplyr::mutate(..sig = NA, ..val = NA)
  }

  # Identify signal-specific ranges and identify non-lag gaps
  smry %>%
    dplyr::group_by(dplyr::pick(dplyr::all_of(c(key_no_t, "..sig")))) %>%
    dplyr::summarize(
      n_non_na = sum(!is.na(.data$..val)),
      n_t = dplyr::n(),
      empty = .data$n_non_na == 0,
      # If empty, we take the min/max of all time_values
      min_t = if (.data$empty) {
        min(.data$time_value)
      } else {
        min(.data$time_value[!is.na(.data$..val)])
      },
      max_t = if (.data$empty) {
        max(.data$time_value)
      } else {
        max(.data$time_value[!is.na(.data$..val)])
      },
      n_gap_na = sum(
        !all(is.na(.data$..val)) & is.na(.data$..val) &
          # The idea is to count the number of NA values that
          # are not at the beginning or end of the time series
          .data$time_value > .data$min_t & .data$time_value < .data$max_t
      ),
      .groups = "drop"
    )
}

#' Summarize `epi_df` object
#'
#' Prints a variety of summary statistics about the `epi_df` object, such as
#' the time range included and geographic coverage.
#'
#' @param object an `epi_df`
#' @param ... Additional arguments, for compatibility with `summary()`.
#'   Currently unused.
#'
#' @method summary epi_df
#' @importFrom rlang .data
#' @importFrom stats median
#' @rdname print.epi_df
#' @export
summary.epi_df <- function(object, ...) {
  cat("An `epi_df` x, with metadata:\n")
  cat(sprintf("* %-9s = %s\n", "geo_type", attributes(object)$metadata$geo_type))
  ok <- attributes(object)$metadata$other_keys
  if (length(ok) > 0) {
    cat(sprintf("* %-9s = %s\n", "other_keys", paste(ok, collapse = ", ")))
  }
  cat(sprintf("* %-9s = %s\n", "as_of", attributes(object)$metadata$as_of))
  cat("----------\n")
  summary_time_latency(object)
}

# Internal helper for summary.epi_df — orchestrates time range, gap, and latency analysis
summary_time_latency <- function(x) {
  if (nrow(x) == 0) {
    return(invisible(NULL))
  }

  keys <- key_colnames(x)
  key_no_t <- setdiff(keys, "time_value")
  sigs <- setdiff(names(x), keys)
  # Exclude complex columns from signal calculations
  sigs <- sigs[vapply(
    x[sigs],
    function(col) is.numeric(col) || is.logical(col),
    logical(1)
  )]
  md <- attr(x, "metadata")
  as_of <- md$as_of
  as_of_valid <- !is.null(as_of) && !is.na(as_of) && (length(as_of) > 0)
  integer_time <- isTRUE(md$time_type %in% c("integer", "custom"))

  # Per-(key combination × signal) time ranges
  # Note: sections below use tryCatch as a defensive programming measure.
  smry_ts <- tryCatch(epi_ts_range(x, key_no_t, sigs), error = function(e) NULL)

  if (is.null(smry_ts)) {
    return(invisible(NULL))
  }

  # Time range information
  tryCatch(epi_df_time_range_info(x, smry_ts), error = function(e) NULL)

  # Time gap information
  tryCatch(
    epi_df_time_gap_info(x, smry_ts, key_no_t, sigs, md),
    error = function(e) NULL
  )

  # Latency information
  tryCatch(
    epi_df_latency_info(
      x, smry_ts, key_no_t, sigs, md, as_of_valid, integer_time
    ),
    error = function(e) NULL
  )

  return(invisible(NULL))
}

# Internal helper for min/max time values summary
epi_df_time_range_info <- function(x, smry_ts) {
  cat("Time range:\n")
  all_empty <- all(smry_ts$empty) # nolint: object_usage_linter
  same_start <- dplyr::n_distinct(smry_ts$min_t[!smry_ts$empty]) <= 1 # nolint: object_usage_linter
  same_end <- dplyr::n_distinct(smry_ts$max_t[!smry_ts$empty]) <= 1 # nolint: object_usage_linter

  min_desc <- dplyr::case_when(
    all_empty ~ "",
    same_start ~ " (same for every time series)",
    TRUE ~ " (but some time series start later)"
  )
  max_desc <- dplyr::case_when(
    all_empty ~ "",
    same_end ~ " (same for every time series)",
    TRUE ~ " (but some time series end earlier)"
  )

  cat(sprintf("* %-27s = %s%s\n", "min time value", min(x$time_value), min_desc))
  cat(sprintf("* %-27s = %s%s\n", "max time value", max(x$time_value), max_desc))
}

# Internal helper for gap analysis summary
epi_df_time_gap_info <- function(x, smry_ts, key_no_t, sigs, md) {
  # Calculate gap metrics at the key-combination level
  smry_all_gaps <- smry_ts %>%
    dplyr::group_by(dplyr::pick(dplyr::all_of(key_no_t))) %>%
    dplyr::summarize(
      n_t = dplyr::first(.data$n_t),
      min_t = min(.data$min_t),
      max_t = max(.data$max_t),
      expected_n = time_minus_time_in_n_steps(.data$max_t, .data$min_t, md$time_type) + 1,
      has_implicit = dplyr::first(.data$n_t) < .data$expected_n,
      n_sig_imp = as.integer(.data$has_implicit) * length(sigs),
      # Total signals with internal non-lag gaps
      n_sig_gap = sum(.data$n_gap_na > 0),
      .groups = "drop"
    )

  cat("Gaps:\n")
  n_imp_keys <- sum(smry_all_gaps$n_sig_imp > 0, na.rm = TRUE)
  n_gap_keys <- sum(smry_all_gaps$n_sig_gap > 0, na.rm = TRUE)

  n_printed <- 0
  if (n_imp_keys > 0) {
    n_sig <- sum(smry_all_gaps$n_sig_imp)
    sig_label <- if (n_sig == 1) "signal" else "signals"
    cat(sprintf(
      "* implicit (missing rows in %d/%d key combinations, affecting %d %s)\n",
      n_imp_keys, nrow(smry_all_gaps), n_sig, sig_label
    ))
    n_printed <- 1
  }
  if (n_gap_keys > 0) {
    n_sig <- sum(smry_all_gaps$n_sig_gap)
    sig_label <- if (n_sig == 1) "signal" else "signals"
    cat(sprintf(
      "* explicit (non-lag NAs in %d/%d key combinations, affecting %d %s)\n",
      n_gap_keys, nrow(smry_all_gaps), n_sig, sig_label
    ))
    n_printed <- 1
  }

  if (n_printed == 0) {
    cat(sprintf("* %-27s = none detected\n", "time gaps"))
  }

  # Average rows per time value
  avg_rows <- nrow(x) / dplyr::n_distinct(x$time_value)
  cat(sprintf("* %-27s = %.2f\n", "average rows per time value", avg_rows))
}

# Internal helper for latency reporting summary
epi_df_latency_info <- function(x, smry_ts, key_no_t, sigs, md, as_of_valid, integer_time) {
  cat("Latency (lag from as_of to latest observation by time series):\n")

  # Check for empty time series and return message if none detected
  if (length(sigs) == 0) {
    cat("* No time series detected\n")
    return(invisible(NULL))
  }

  # Determine the unit for time
  unit_format <- if (integer_time) "" else time_type_unit_pluralizer[[md$time_type]] %||% ""
  # Note: a space is prefixed for unit label formatting
  unit <- if (integer_time) "" else cli::pluralize(paste0(" {qty(2)}", unit_format))

  max_sigs <- 8
  fired_reasons <- character(0)
  as_of <- md$as_of

  # Iterate over signals and print latency information
  for (sig in head(sigs, max_sigs)) {
    ts_sig <- smry_ts[smry_ts$..sig == sig, ]

    # Check for empty time series and return message if all NA detected
    if (all(ts_sig$empty)) {
      cat(sprintf("* %s: all NA\n", sig))
      next
    }

    # Range based on non-empty time series
    ts_non_empty <- ts_sig[!ts_sig$empty, ]
    lags <- if (as_of_valid) {
      time_minus_time_in_n_steps(as_of, ts_non_empty$max_t, md$time_type)
    } else {
      NA_real_
    }
    lag_min <- as.integer(min(lags, na.rm = TRUE))
    lag_max <- as.integer(max(lags, na.rm = TRUE))
    om <- max(ts_non_empty$max_t, na.rm = TRUE)

    # Format the lag range
    range_str <- if (as_of_valid && !all(is.na(lags))) {
      if (lag_min == lag_max) {
        sprintf("lag %d%s ", lag_min, unit)
      } else {
        sprintf("lag %d\u2013%d%s ", lag_min, lag_max, unit)
      }
    } else {
      ""
    }

    out <- sprintf("* %s: %s(max time %s)", sig, range_str, as.character(om))

    # Identify empty keys for this signal
    ts_empty <- ts_sig[ts_sig$empty, ]
    n_empty <- nrow(ts_empty)
    if (n_empty > 0) {
      out <- paste0(
        out,
        "; empty: ",
        format_key_combos(ts_empty[, key_no_t, drop = FALSE], max_sigs)
      )
    }

    # Identify lagging keys for this signal
    lagging_rows <- ts_non_empty[ts_non_empty$max_t < om, key_no_t, drop = FALSE]
    n_lagging <- nrow(lagging_rows)
    if (n_lagging > 0) {
      out <- paste0(
        out,
        "; lagging keys: ",
        format_key_combos(lagging_rows, max_sigs)
      )
    }

    # Collect notable reasons
    notable_threshold <- if (isTRUE(md$time_type == "day")) 7 else 2
    reasons <- c()
    if (as_of_valid && !all(is.na(lags)) && lag_max > notable_threshold) {
      unit_pl <- cli::pluralize(paste0(" {qty(", lag_max, ")}", unit_format))
      reasons <- c(
        reasons,
        sprintf("lag > %d%s", notable_threshold, unit_pl)
      )
    }
    if (n_lagging > 0) reasons <- c(reasons, "lagging keys")
    if (n_empty > 0) reasons <- c(reasons, "empty keys")
    if (length(reasons) > 0) {
      fired_reasons <- union(fired_reasons, reasons)
      out <- paste0(out, " (!)")
    }
    cat(out, "\n", sep = "")
  }

  # Print summary for other signals if there are more than max_sigs
  if ((n_more <- length(sigs) - max_sigs) > 0) {
    cat(sprintf(
      "* ... and %d other signal%s\n",
      n_more,
      if (n_more == 1) "" else "s"
    ))
  }

  # Print notable latency reasons if any
  if (length(fired_reasons) > 0) {
    cat(sprintf(
      "(!): notable latency (%s)\n",
      paste(fired_reasons, collapse = "; ")
    ))
  }
}

# Helper to format key combinations
# TODO: reuse function in #693
format_key_combos <- function(df, max_n = 8) {
  n <- nrow(df)
  if (n == 0) {
    return("")
  }

  # Paste across columns with "; "
  keys_str <- apply(df, 1, paste, collapse = "; ")

  if (n <= max_n) {
    paste(keys_str, collapse = "; ")
  } else {
    sprintf("%d keys (e.g., %s)", n, paste(head(keys_str, 2), collapse = "; "))
  }
}

#' Drop any `epi_df` metadata and class on a data frame
#'
#' Useful in implementing `?dplyr_extending` when manipulations cause invariants
#' of `epi_df`s to be violated and we need to return some other class. Note that
#' this will maintain any grouping (keeping the `grouped_df` class and
#' associated attributes, if present).
#'
#' @param x an `epi_df` or other data frame
#' @return `x` with any metadata dropped and the `"epi_df"` class, if previously
#'   present, dropped
#'
#' @keywords internal
decay_epi_df <- function(x) {
  attributes(x)$metadata <- NULL
  class(x) <- class(x)[class(x) != "epi_df"]
  x
}

# Implementing `dplyr_extending`: we have a few metadata attributes to consider:
# `as_of` is an attribute doesn't depend on the rows or columns, `geo_type` is a
# scalar attribute dependent on columns, and `other_keys` acts like an attribute
# vectorized over columns; `dplyr_extending` advice at time of writing says to
# implement `dplyr_reconstruct`, 1d `[`, `dplyr_col_modify`, and `names<-`, but
# not `dplyr_row_slice`; however, we'll also implement `dplyr_row_slice` anyway
# to prevent a `arrange` on grouped `epi_df`s from dropping the `epi_df` class.
# We'll implement `[` to allow either 1d or 2d. We'll also implement some other
# methods where we want to (try to) maintain an `epi_df`.

#' dplyr_reconstruct
#'
#' @param data tibble or `epi_df` (`dplyr` feeds in former, but we may
#'   directly feed in latter from our other methods)
#' @param template `epi_df` template to use to restore
#' @return `epi_df` or degrade into `tbl_df`
#' @importFrom dplyr dplyr_reconstruct
#' @importFrom cli cli_vec
#' @export
#' @keywords internal
dplyr_reconstruct.epi_df <- function(data, template) {
  # Start from a reconstruction for the backing S3 classes; this ensures that we
  # keep any grouping that has been applied:
  res <- NextMethod()

  reconstruct_light_edf(res, template)
}

#' Like `dplyr_reconstruct.epi_df` but not recomputing any grouping
#'
#' In the move to our current not-quite-proper/effective "implementation" of
#' [`dplyr::dplyr_extending`] for `epi_df`s, we moved a lot of checks in
#' `dplyr_reconstruct` and used it instead of `reclass()` in various
#' operations to prevent operations from outputting invalid metadata/classes,
#' instead of more careful tailored and relevant checks. However, this actually
#' introduced extra overhead due to `dplyr_reconstruct.epi_df()` passing off to
#' `dplyr_reconstruct.grouped_df()` when grouped, which assumes that it will
#' need to / should for safety recompute the groups, even when it'd be safe for
#' it not to do so. In many operations, we're using `NextMethod()` to dispatch
#' to `grouped_df` behavior if needed, and it should output something with valid
#' groupings.
#'
#' This function serves the original purpose of performing `epi_df`-centric
#' checks rather than just throwing on potentially-incorrect metadata like
#' `reclass()`, but without unnecessary `dplyr_reconstruct()` delegation.
#'
#' @keywords internal
reconstruct_light_edf <- function(data, template) {
  col_names <- names(data)

  # Duplicate columns, cli_abort
  dup_col_names <- col_names[duplicated(col_names)]
  if (length(dup_col_names) != 0) {
    cli_abort(c(
      "Duplicate column names are not allowed",
      "i" = "Duplicated column name{?s}:
        {cli_vec(unique(dup_col_names),
                 style = list('vec-sep2' = ', ', 'vec-last' = ', '))}"
    ))
  }

  not_epi_df <- !("time_value" %in% col_names) || !("geo_value" %in% col_names)

  if (not_epi_df) {
    # If we're calling on an `epi_df` from one of our own functions, we need to
    # decay to a non-`epi_df` result. If `dplyr` is calling, `x` is a tibble,
    # `data` is not an `epi_df` yet (but might, e.g., be a `grouped_df`), and we
    # simply need to skip adding the metadata & class. Current `decay_epi_df`
    # should work in both cases.
    return(decay_epi_df(data))
  }

  data <- reclass(data, attr(template, "metadata"))

  # XXX we may want verify the `geo_type` and `time_type` here. If it's
  # significant overhead, we may also want to keep this less strict version
  # around and implement some extra S3 methods that use it, when appropriate.

  # Amend additional metadata if some other_keys cols are dropped in the subset
  old_other_keys <- attr(template, "metadata")$other_keys
  attr(data, "metadata")$other_keys <- old_other_keys[old_other_keys %in% col_names]

  data
}

#' @export
`[.epi_df` <- function(x, i, j, drop = FALSE) {
  res <- NextMethod()

  if (!is.data.frame(res)) {
    return(res)
  }

  reconstruct_light_edf(res, x)
}

#' @export
`[<-.epi_df` <- function(x, i, j, ..., value) {
  res <- NextMethod()

  reconstruct_light_edf(res, x)
}

#' @export
`[[<-.epi_df` <- function(x, i, j, ..., value) {
  res <- NextMethod()

  reconstruct_light_edf(res, x)
}

#' @export
`$<-.epi_df` <- function(x, name, value) {
  res <- NextMethod()

  reconstruct_light_edf(res, x)
}

#' @importFrom dplyr dplyr_col_modify
#' @export
dplyr_col_modify.epi_df <- function(data, cols) {
  reconstruct_light_edf(NextMethod(), data)
}

#' @importFrom dplyr dplyr_row_slice
#' @export
dplyr_row_slice.epi_df <- function(data, i, ...) {
  reconstruct_light_edf(NextMethod(), data)
}

#' @export
`names<-.epi_df` <- function(x, value) {
  old_names <- names(x)
  old_metadata <- attr(x, "metadata")
  new_metadata <- old_metadata
  old_other_keys <- old_metadata[["other_keys"]]
  if (!is.null(old_other_keys)) {
    new_other_keys <- value[match(old_other_keys, old_names)]
    new_metadata[["other_keys"]] <- new_other_keys
  }
  result <- reclass(NextMethod(), new_metadata)
  reconstruct_light_edf(result, result)
}

#' @method group_by epi_df
#' @param .data an `epi_df`
#' @rdname print.epi_df
#' @export
group_by.epi_df <- function(.data, ...) {
  metadata <- attributes(.data)$metadata
  .data <- NextMethod()
  reclass(.data, metadata)
}

#' @method ungroup epi_df
#' @rdname print.epi_df
#' @export
ungroup.epi_df <- function(x, ...) {
  metadata <- attributes(x)$metadata
  x <- NextMethod()
  reclass(x, metadata)
}

#' @method group_modify epi_df
#' @rdname print.epi_df
#' @param .data an `epi_df`
#' @param .f function or formula; see [`dplyr::group_modify`]
#' @param .keep Boolean; see [`dplyr::group_modify`]
#' @export
group_modify.epi_df <- function(.data, .f, ..., .keep = FALSE) {
  reconstruct_light_edf(NextMethod(), .data)
}

#' "Complete" an `epi_df`, adding missing rows and/or replacing `NA`s
#'
#' A `tidyr::complete()` analogue for `epi_df` objects. This function
#' can be used, for example, to add rows for missing combinations
#' of `geo_value` and `time_value`, filling other columns with `NA`s.
#' See the examples for usage details.
#'
#' @param data an `epi_df`
#' @param ... see [`tidyr::complete`]
#' @param fill see [`tidyr::complete`]
#' @param explicit see [`tidyr::complete`]
#'
#' @method complete epi_df
#' @importFrom tidyr complete
#'
#' @examples
#' start_date <- as.Date("2020-01-01")
#' daily_edf <- tibble::tribble(
#'   ~geo_value, ~time_value, ~value,
#'   1, start_date + 1, 1,
#'   1, start_date + 3, 3,
#'   2, start_date + 2, 2,
#'   2, start_date + 3, 3,
#' ) %>%
#'   as_epi_df(as_of = start_date + 3)
#' # Complete without grouping puts all the geo_values on the same min and max
#' # time_value index
#' daily_edf %>%
#'   complete(geo_value, time_value = full_seq(time_value, period = 1))
#' # Complete with grouping puts all the geo_values on individual min and max
#' # time_value indices
#' daily_edf %>%
#'   group_by(geo_value) %>%
#'   complete(time_value = full_seq(time_value, period = 1))
#' # Complete has explicit=TRUE by default, but if it's FALSE, then complete
#' # only fills the implicit gaps, not those that are explicitly NA
#' daily_edf <- tibble::tribble(
#'   ~geo_value, ~time_value, ~value,
#'   1, start_date + 1, 1,
#'   1, start_date + 2, NA,
#'   1, start_date + 3, 3,
#'   2, start_date + 2, 2,
#'   2, start_date + 3, 3,
#' ) %>%
#'   as_epi_df(as_of = start_date + 3)
#' daily_edf %>%
#'   complete(
#'     geo_value,
#'     time_value = full_seq(time_value, period = 1),
#'     fill = list(value = 0),
#'     explicit = FALSE
#'   )
#' # Complete works for weekly data and can take a fill value
#' # No grouping
#' weekly_edf <- tibble::tribble(
#'   ~geo_value, ~time_value, ~value,
#'   1, start_date + 1, 1,
#'   1, start_date + 15, 3,
#'   2, start_date + 8, 2,
#'   2, start_date + 15, 3,
#' ) %>%
#'   as_epi_df(as_of = start_date + 3)
#' weekly_edf %>%
#'   complete(
#'     geo_value,
#'     time_value = full_seq(time_value, period = 7),
#'     fill = list(value = 0)
#'   )
#' # With grouping
#' weekly_edf %>%
#'   group_by(geo_value) %>%
#'   complete(
#'     time_value = full_seq(time_value, period = 7),
#'     fill = list(value = 0)
#'   )
#' @export
complete.epi_df <- function(data, ..., fill = list(), explicit = TRUE) {
  result <- reconstruct_light_edf(NextMethod(), data)
  if ("time_value" %in% names(rlang::call_match(dots_expand = FALSE)[["..."]])) {
    attr(result, "metadata")$time_type <- guess_time_type(result$time_value)
  }
  result
}

#' @method unnest epi_df
#' @rdname print.epi_df
#' @param data an `epi_df`
#' @export
unnest.epi_df <- function(data, ...) {
  reconstruct_light_edf(NextMethod(), data)
}

# Simple reclass function
reclass <- function(x, metadata) {
  class(x) <- unique(c("epi_df", class(x)))
  attr(x, "metadata") <- metadata
  x
}

#' Arrange an epi_df into a standard order
#'
#' Moves [key_colnames()] to the left, then arranges rows based on that
#' ordering. This function is mainly for use in tests and so that
#' other function output will be in predictable order, where necessary.
#'
#' @param x an `epi_df`. Other objects will produce a warning and return as is.
#' @param ... not used
#'
#' @keywords internal
#' @export
arrange_canonical <- function(x, ...) {
  UseMethod("arrange_canonical")
}

#' @export
arrange_canonical.default <- function(x, ...) {
  rlang::check_dots_empty()
  cli::cli_abort(c(
    "`arrange_canonical()` is only meaningful for an {.cls epi_df}."
  ))
  x
}

#' @export
arrange_canonical.epi_df <- function(x, ...) {
  rlang::check_dots_empty()
  x %>%
    arrange_row_canonical() %>%
    arrange_col_canonical()
}

arrange_row_canonical <- function(x, ...) {
  UseMethod("arrange_row_canonical")
}

#' @export
arrange_row_canonical.default <- function(x, ...) {
  rlang::check_dots_empty()
  cli::cli_abort(c(
    "`arrange_row_canonical()` is only meaningful for an {.cls epi_df}."
  ))
  x
}

#' @export
arrange_row_canonical.epi_df <- function(x, ...) {
  rlang::check_dots_empty()
  cols <- key_colnames(x)
  x[vctrs::vec_order(x[cols]), ]
}

arrange_col_canonical <- function(x, ...) {
  UseMethod("arrange_col_canonical")
}

#' @export
arrange_col_canonical.default <- function(x, ...) {
  rlang::check_dots_empty()
  cli::cli_abort(c(
    "`arrange_col_canonical()` is only meaningful for an {.cls epi_df}."
  ))
  x
}

#' @export
arrange_col_canonical.epi_df <- function(x, ...) {
  rlang::check_dots_empty()
  all_names <- names(x)
  key_names <- key_colnames(x)
  val_names <- all_names[!all_names %in% key_names]
  x[c(key_names, val_names)]
}

#' Group an `epi_df` object by default keys
#' @param x an `epi_df`
#' @param exclude character vector of column names to exclude from grouping
#' @return a grouped `epi_df`
#' @export
group_epi_df <- function(x, exclude = character()) {
  cols <- key_colnames(x, exclude = exclude)
  reclass(grouped_df(x, cols), attr(x, "metadata"))
}

#' Aggregate an `epi_df` object
#'
#' Aggregates an `epi_df` object by the specified group columns, summing the
#' `value` column, and returning an `epi_df`. If aggregating over `geo_value`,
#' the resulting `epi_df` will have `geo_value` set to `"total"`.
#'
#' @param .x an `epi_df`
#' @param sum_cols `r tidyselect_arg_roxygen`
#' @param group_cols character vector of column names to group by. "time_value" is
#'   included by default.
#' @return an `epi_df` object
#'
#' @examples
#' # This data has other_keys age_group and edu_qual:
#' grad_employ_subset
#'
#' # Aggregate num_graduates within each geo_value (and time_value):
#' grad_employ_subset %>%
#'   sum_groups_epi_df(num_graduates, group_cols = "geo_value")
#'
#' @export
sum_groups_epi_df <- function(.x, sum_cols, group_cols = "time_value") {
  assert_class(.x, "epi_df")
  assert_character(group_cols)
  checkmate::assert_subset(group_cols, key_colnames(.x))
  if (!"time_value" %in% group_cols) {
    group_cols <- c("time_value", group_cols)
  }
  # Attempt tidyselection ourselves to get "Error in `sum_groups_epi_df()`"
  # rather than "in `dplyr::summarize()`", before forwarding:
  sum_cols <- rlang::enquo(sum_cols)
  tidyselect::eval_select(sum_cols, .x)
  out <- group_by(.x, across(all_of(group_cols))) %>%
    dplyr::summarize(across(!!sum_cols, sum), .groups = "drop")

  # To preserve epi_df-ness, we need to ensure that the `geo_value` column is
  # present.
  if (!"geo_value" %in% group_cols) {
    out <- out %>%
      mutate(geo_value = "total") %>%
      relocate("geo_value", .before = 1)
  }

  # The `geo_type` will be correctly inherited here by the following logic:
  # - if `geo_value` is in `group_cols`, then the constructor will see the
  #   geo_value here and will correctly read the existing values
  # - if `geo_value` is not in `group_cols`, then the constructor will see
  #   the unrecognizeable "total" value and will correctly infer the "custom"
  #   geo_type.
  as_epi_df(
    out,
    as_of = attr(.x, "metadata")$as_of,
    other_keys = intersect(attr(.x, "metadata")$other_keys, group_cols)
  ) %>%
    arrange_canonical()
}

#' @method drop_na epi_df
#' @importFrom tidyr drop_na
#' @export
drop_na.epi_df <- function(data, ...) {
  res <- NextMethod()
  reclass(res, attr(data, "metadata"))
}

#' @method pivot_wider epi_df
#' @importFrom tidyr pivot_wider
#' @export
pivot_wider.epi_df <- function(data, ...) {
  res <- NextMethod()
  # Extract the 'names_from' field from the dots.
  dots <- rlang::enquos(...)
  names_from_enquo <- dots$names_from %||% rlang::quo(name)
  names_from_chr <- names(tidyselect::eval_select(
    names_from_enquo, data,
    allow_rename = FALSE
  ))
  template <- vctrs::vec_ptype(data)
  attr(template, "metadata")$other_keys <- vctrs::vec_set_difference(
    attr(template, "metadata")$other_keys, names_from_chr
  )
  reconstruct_light_edf(res, template)
}

#' @method pivot_longer epi_df
#' @importFrom tidyr pivot_longer
#' @export
pivot_longer.epi_df <- function(data, ..., names_to = "name") {
  res <- NextMethod()
  # Use setdiff to filter out the special `".value"` placeholder
  new_keys <- setdiff(names_to, ".value")
  template <- vctrs::vec_ptype(data)
  attr(template, "metadata")$other_keys <- unique(
    c(attr(template, "metadata")$other_keys, new_keys)
  )
  reconstruct_light_edf(res, template)
}
