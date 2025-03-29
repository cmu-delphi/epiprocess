#' Automatically plot an epi_df
#'
#' @param object An `epi_df`
#' @param ... <[`tidy-select`][dplyr_tidy_select]> One or more unquoted
#'   expressions separated by commas. Variable names can be used as if they
#'   were positions in the data frame, so expressions like `x:y` can
#'   be used to select a range of variables.
#' @param .color_by Which variables should determine the color(s) used to plot
#'   lines. Options include:
#'   * `all_keys` - the default uses the interaction of any key variables
#'     including the `geo_value`
#'   * `geo_value` - `geo_value` only
#'   * `other_keys` - any available keys that are not `geo_value`
#'   * `.response` - the numeric variables (same as the y-axis)
#'   * `all` - uses the interaction of all keys and numeric variables
#'   * `none` - no coloring aesthetic is applied
#' @param .facet_by Similar to `.color_by` except that the default is to display
#'   each numeric variable on a separate facet
#' @param .base_color Lines will be shown with this color if `.color_by == "none"`.
#'  For example, with a single numeric variable and faceting by `geo_value`, all
#'  locations would share the same color line.
#' @param .max_facets `r lifecycle::badge("deprecated")`
#' @param .facet_filter Select which facets will be displayed. Especially
#'   useful for when there are many `geo_value`'s or keys. This is a
#'   <[`rlang`][args_data_masking]> expression along the lines of [dplyr::filter()].
#'   However, it must be a single expression combined with the `&` operator. This
#'   contrasts to the typical use case which allows multiple comma-separated expressions
#'   which are implicitly combined with `&`. When multiple variables are selected
#'   with `...`, their names can be filtered in combination with other factors
#'   by using `.response_name`. See the examples below.
#'
#'
#'
#' @return A [ggplot2::ggplot] object
#' @export
#'
#' @examples
#' autoplot(cases_deaths_subset, cases, death_rate_7d_av)
#' autoplot(cases_deaths_subset, case_rate_7d_av, .facet_by = "geo_value")
#' autoplot(cases_deaths_subset, case_rate_7d_av,
#'   .color_by = "none",
#'   .facet_by = "geo_value"
#' )
#' autoplot(cases_deaths_subset, case_rate_7d_av,
#'   .color_by = "none",
#'   .base_color = "red", .facet_by = "geo_value"
#' )
#'
#' # .base_color specification won't have any effect due .color_by default
#' autoplot(cases_deaths_subset, case_rate_7d_av,
#'   .base_color = "red", .facet_by = "geo_value"
#' )
#'
#' # filter to only some facets, must be explicitly combined
#' autoplot(cases_deaths_subset, cases, death_rate_7d_av,
#'   .facet_by = "all",
#'   .facet_filter = (.response_name == "cases" & geo_value %in% c("tx", "pa")) |
#'     (.response_name == "death_rate_7d_av" &
#'       geo_value %in% c("ca", "fl", "ga", "ny"))
#' )
autoplot.epi_df <- function(
    object, ...,
    .color_by = c("all_keys", "geo_value", "other_keys", ".response", "all", "none"),
    .facet_by = c(".response", "other_keys", "all_keys", "geo_value", "all", "none"),
    .base_color = "#3A448F",
    .facet_filter = NULL,
    .max_facets = deprecated()) {
  .color_by <- rlang::arg_match(.color_by)
  .facet_by <- rlang::arg_match(.facet_by)
  .facet_filter <- rlang::enquo(.facet_filter)

  if (lifecycle::is_present(.max_facets)) {
    lifecycle::deprecate_warn(
      "0.11.1",
      "autoplot.epi_df(.max_facets = )",
      "autoplot.epi_df(.facet_filter = )"
    )
  }
  assert_character(.base_color, len = 1)

  key_cols <- key_colnames(object)
  non_key_cols <- setdiff(names(object), key_cols)
  geo_and_other_keys <- key_colnames(object, exclude = "time_value")

  # --- check for numeric variables
  vars <- autoplot_check_viable_response_vars(object, ..., non_key_cols = non_key_cols)
  nvars <- length(vars)

  # --- create a viable df to plot
  pos <- tidyselect::eval_select(
    rlang::expr(c("time_value", tidyselect::all_of(geo_and_other_keys), names(vars))), object
  )
  if (nvars > 1) {
    object <- tidyr::pivot_longer(
      object[pos], tidyselect::all_of(names(vars)),
      values_to = ".response",
      names_to = ".response_name"
    )
  } else {
    object <- dplyr::rename(object[pos], .response := !!names(vars)) # nolint: object_usage_linter
  }
  all_keys <- rlang::syms(as.list(geo_and_other_keys))
  other_keys <- rlang::syms(as.list(setdiff(geo_and_other_keys, "geo_value")))
  all_avail <- rlang::syms(as.list(c(
    geo_and_other_keys,
    if (nvars > 1) ".response_name" else NULL
  )))

  object <- object %>%
    dplyr::mutate(
      .colours = switch(.color_by,
        all_keys = interaction(!!!all_keys, sep = " / "),
        geo_value = .data$geo_value,
        other_keys = interaction(!!!other_keys, sep = " / "),
        all = interaction(!!!all_avail, sep = " / "),
        NULL
      ),
      .facets = switch(.facet_by,
        all_keys = interaction(!!!all_keys, sep = " / "),
        geo_value = as.factor(.data$geo_value),
        other_keys = interaction(!!!other_keys, sep = " / "),
        all = interaction(!!!all_avail, sep = " / "),
        NULL
      )
    )

  if (!rlang::quo_is_null(.facet_filter) && ".facets" %in% names(object)) {
    object <- dplyr::filter(object, !!.facet_filter) %>%
      dplyr::mutate(.facets = droplevels(.data$.facets))
    if (".colours" %in% names(object)) {
      object <- dplyr::mutate(object, .colours = droplevels(.data$.colours))
    }
  }

  p <- ggplot2::ggplot(object, ggplot2::aes(x = .data$time_value)) +
    ggplot2::theme_bw()

  if (".colours" %in% names(object)) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes(y = .data$.response, colour = .data$.colours),
      key_glyph = "timeseries"
    ) +
      ggplot2::scale_colour_viridis_d(name = "")
  } else if (length(vars) > 1 && .color_by == ".response") {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(
        y = .data$.response, colour = .data$.response_name
      )) +
      ggplot2::scale_colour_viridis_d(name = "")
  } else { # none
    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = .data$.response), color = .base_color)
  }

  if (".facets" %in% names(object)) {
    p <- p + ggplot2::facet_wrap(~.facets, scales = "free_y") +
      ggplot2::ylab(names(vars))
    if (.facet_by == "all") p <- p + ggplot2::ylab("")
  } else if ((length(vars) > 1 && .facet_by == ".response")) {
    p <- p + ggplot2::facet_wrap(~.response_name, scales = "free_y") +
      ggplot2::ylab("")
  } else {
    p <- p + ggplot2::ylab(names(vars))
  }
  p
}

autoplot_check_viable_response_vars <- function(
    object, ..., non_key_cols, call = caller_env()) {
  allowed <- purrr::map_lgl(object[non_key_cols], is.numeric)
  allowed <- allowed[allowed]
  if (length(allowed) == 0 && rlang::dots_n(...) == 0L) {
    cli::cli_abort("No numeric variables were available to plot automatically.",
      class = "epiprocess__no_numeric_vars_available",
      call = call
    )
  }
  vars <- tidyselect::eval_select(rlang::expr(c(...)), object)
  if (rlang::is_empty(vars)) { # find them automatically if unspecified
    vars <- tidyselect::eval_select(names(allowed)[1], object)
    cli::cli_warn(
      "Plot variable was unspecified. Automatically selecting {.var {names(allowed)[1]}}.",
      class = "epiprocess__unspecified_plot_var",
      call = call
    )
  } else { # if variables were specified, ensure that they are numeric
    ok <- names(vars) %in% names(allowed)
    if (!any(ok)) {
      cli::cli_abort(
        "None of the requested variables {.var {names(vars)}} are numeric.",
        class = "epiprocess__all_requested_vars_not_numeric",
        call = call
      )
    } else if (!all(ok)) {
      cli::cli_warn(
        c(
          "Only the requested variables {.var {names(vars)[ok]}} are numeric.",
          i = "`autoplot()` cannot display {.var {names(vars)[!ok]}}."
        ),
        class = "epiprocess__some_requested_vars_not_numeric",
        call = call
      )
      vars <- vars[ok]
    }
  }
  vars
}



#' Automatically plot an epi_archive
#'
#' @param object An `epi_archive`
#' @inheritParams autoplot.epi_df
#' @param .versions Select which versions will be displayed. By default, every
#'   a separate line will be shown with the data as it would have appeared on
#'   every day in the archive. This can sometimes become overwhelming. For
#'   example, daily data would display a line for what the data would have looked
#'   like on every single day. To override this, you can select specific dates,
#'   by passing a vector of values here. Alternatively, a sequence can be
#'   automatically created by passing a string like `"2 weeks"` or `"month"`.
#'   For time types where the `time_value` is a date object, any string that
#'   is interpretable by [seq.Date()] is allowed.
#'
#'   For `time_type = "integer"`, an integer larger than 1 will give a subset
#'   of versions.
#'
#' @return A [ggplot2::ggplot] object
#' @export
#'
#' @examples
#' autoplot(archive_cases_dv_subset, percent_cli, .versions = "week")
#' autoplot(archive_cases_dv_subset_all_states, percent_cli,
#'   .versions = "week",
#'   .facet_filter = geo_value %in% c("or", "az", "vt", "ms")
#' )
#' autoplot(archive_cases_dv_subset, percent_cli,
#'   .versions = "month",
#'   .facet_filter = geo_value == "ca"
#' )
autoplot.epi_archive <- function(object, ...,
                                 .base_color = "black",
                                 .versions = NULL,
                                 .facet_filter = NULL) {
  time_type <- object$time_type
  if (time_type == "custom") {
    cli_abort(
      "This `epi_archive` has custom `time_type`. This is currently unsupported.",
      class = "epiprocess__autoplot_archive_custom_time_type"
    )
  }

  max_version <- max(object$DT$version)
  min_version <- min(object$DT$version)

  tt_lookup <- c("day" = "day", "week" = "week", "yearmonth" = "month")
  .versions <- .versions %||% ifelse(time_type == "integer", 1L, unname(tt_lookup[time_type]))
  if (is.character(.versions) || length(.versions) == 1L) {
    if (is.numeric(.versions)) .versions <- round(abs(.versions))
    .versions <- seq(min_version, max_version - 1, by = .versions)
  } else if (methods::is(.versions, "Date") || is.numeric(.versions)) {
    .versions <- .versions[.versions >= min_version & .versions <= max_version]
  } else {
    cli_abort(
      "Requested `.versions` don't appear to match the available `time_type`.",
      class = "epiprocess__autoplot_archive_bad_versions"
    )
  }


  finalized <- epix_as_of(object, max_version)
  key_cols <- key_colnames(finalized)
  non_key_cols <- setdiff(names(finalized), key_cols)
  vars <- autoplot_check_viable_response_vars(finalized, ..., non_key_cols = non_key_cols)
  nvars <- length(vars)

  bp <- autoplot.epi_df(
    finalized, ...,
    .base_color = .base_color, .facet_by = "all",
    .facet_filter = {{ .facet_filter }}, .color_by = "none"
  )
  geo_and_other_keys <- key_colnames(object, exclude = c("time_value", "version"))
  all_avail <- rlang::syms(as.list(c(
    geo_and_other_keys,
    if (nvars > 1) ".response_name" else NULL
  )))

  snapshots <- purrr::map(
    .versions,
    function(v) {
      dplyr::mutate(epix_as_of(object, v), version = v)
    }
  ) %>%
    purrr::list_rbind() %>%
    dplyr::mutate(.facets = interaction(!!!all_avail, sep = " / "))

  if (nvars > 1) {
    snapshots <- tidyr::pivot_longer(
      snapshots, tidyselect::all_of(names(vars)),
      values_to = ".response",
      names_to = ".response_name"
    )
  } else {
    snapshots <- dplyr::rename(snapshots, .response := !!names(vars)) # nolint: object_usage_linter
  }
  snapshots <- snapshots %>%
    dplyr::filter(!is.na(.response), .data$.facets %in% unique(bp$data$.facets))

  bp <- bp +
    ggplot2::geom_line(
      data = snapshots,
      mapping = ggplot2::aes(y = .response, color = version, group = factor(version))
    )
  if (methods::is(.versions, "Date")) {
    bp <- bp + ggplot2::scale_color_viridis_c(name = "Version", trans = "date")
  } else {
    bp <- bp + ggplot2::scale_color_viridis_c(name = "Version")
  }

  # make the finalized layer last
  bp$layers <- rev(bp$layers)
  bp
}
