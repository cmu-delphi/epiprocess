#' Automatically plot an epi_df or epi_archive
#'
#' @param object,x An `epi_df` or `epi_archive`
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more unquoted
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
#'   <[`rlang`][rlang::args_data_masking]> expression along the lines of [dplyr::filter()].
#'   However, it must be a single expression combined with the `&` operator. This
#'   contrasts to the typical use case which allows multiple comma-separated expressions
#'   which are implicitly combined with `&`. When multiple variables are selected
#'   with `...`, their names can be filtered in combination with other factors
#'   by using `.response_name`. See the examples below.
#' @param .max_keys Maximum number of key combinations to display. If the data
#'   contains more key combinations than this limit, a random sample of size
#'   `.max_keys` is displayed, and a warning is issued. Set to `Inf` to
#'   display all keys. Does not apply if `interactive = TRUE`.
#' @param interactive Logical. If `TRUE`, returns an interactive
#'   [plotly::ggplotly()] widget instead of a static [ggplot2::ggplot()] object.
#'   This is especially useful for exploring datasets with many keys. Default is
#'   `FALSE`.
#'
#' @return A [ggplot2::ggplot] object OR [plotly::plotly] object if `interactive = TRUE`
#' @export
#' @name autoplot-epi
#'
#' @examples
#' # -- Use it on an `epi_df`
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
#' # Just an alias for convenience
#' plot(cases_deaths_subset, cases, death_rate_7d_av,
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
  .max_keys = 10,
  interactive = FALSE,
  .max_facets = deprecated()
) {
  .color_by <- rlang::arg_match(.color_by)
  .facet_by <- rlang::arg_match(.facet_by)
  .facet_filter <- rlang::enquo(.facet_filter)
  checkmate::assert_logical(interactive, len = 1L)
  checkmate::assert_number(.max_keys, lower = 1)

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

  object <- autoplot_subsample_keys(
    object, geo_and_other_keys, .max_keys, interactive,
    .facet_used = ".facets" %in% names(object)
  )

  p <- ggplot2::ggplot(object, ggplot2::aes(x = .data$time_value))

  if (identical(ggplot2::theme_get(), ggplot2::theme_gray())) {
    p <- p + ggplot2::theme_bw()
  }

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
      ggplot2::geom_line(
        ggplot2::aes(
          y = .data$.response,
          group = interaction(!!!all_avail)
        ),
        color = .base_color
      )
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
  if (interactive) {
    if (".facets" %in% names(object)) {
      # Use the interactive helper with dropdowns.
      trace_col <- if (".colours" %in% names(object)) ".colours" else if (nvars > 1) ".response_name" else NULL
      return(autoplot_plotly_dropdown(
        data = object,
        group_col = ".facets",
        trace_col = trace_col,
        .base_color = .base_color,
        yaxis_title = names(vars)[1]
      ))
    }
    return(autoplot_interactive_df(p, object, .max_keys))
  }
  p
}

autoplot_check_viable_response_vars <- function(
  object, ..., non_key_cols, call = caller_env()
) {
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


autoplot_subsample_keys <- function(
  object, geo_and_other_keys, .max_keys, interactive, .facet_used
) {
  if (interactive || is.infinite(.max_keys)) {
    return(object)
  }

  epikey_combinations <- rlang::inject(
    interaction(!!!object[geo_and_other_keys], sep = " / ", drop = TRUE)
  )
  unique_epikeys <- levels(epikey_combinations)
  num_epikeys <- length(unique_epikeys)

  if (num_epikeys <= .max_keys) {
    return(object)
  }

  max_keys_val <- .max_keys
  sampled_epikeys <- sample(unique_epikeys, max_keys_val)

  msg <- c(
    "Plotting {num_epikeys} keys can be slow and hard to read. Subsampling to {max_keys_val} keys.",
    i = "To plot all keys, use `autoplot(..., .max_keys = Inf)`.",
    i = "To explore all keys interactively, use `autoplot(..., interactive = TRUE)`."
  )
  if (.facet_used) {
    msg <- c(msg, i = "To plot specific keys, use `autoplot(..., .facet_filter = ...)`.")
  }

  cli::cli_warn(msg, class = "epiprocess__autoplot_max_keys_exceeded")
  object[epikey_combinations %in% sampled_epikeys, ]
}


autoplot_plotly_dropdown <- function(
  data, group_col, trace_col = NULL,
  color_map = NULL, .base_color = "#3A448F",
  xaxis_title = "Date", yaxis_title = "", legend_title = ""
) {
  rlang::check_installed("plotly")

  unique_groups <- levels(droplevels(as.factor(data[[group_col]])))

  # Prepare lines to be added to the plot
  trace_specs <- list()
  trace_group_idx <- integer(0)

  # Identify each dropdown option
  for (gi in seq_along(unique_groups)) {
    g <- unique_groups[gi]
    g_data <- data[data[[group_col]] == g, , drop = FALSE]

    # Handle multiple lines within one dropdown selection
    if (!is.null(trace_col) && trace_col %in% names(g_data)) {
      sub_trace_vals <- unique(g_data[[trace_col]])
      if (is.factor(sub_trace_vals)) {
        sub_trace_vals <- levels(droplevels(sub_trace_vals))
      } else {
        sub_trace_vals <- sort(sub_trace_vals)
      }
    } else {
      # one line per dropdown option
      sub_trace_vals <- "default"
    }

    # Creates the individual lines for a single dropdown selection
    for (si in seq_along(sub_trace_vals)) {
      s_val <- sub_trace_vals[si]
      s_data <- if (identical(s_val, "default")) g_data else g_data[g_data[[trace_col]] == s_val, , drop = FALSE]
      s_data <- s_data[order(s_data$time_value), , drop = FALSE]

      # "metadata" for the specific line
      lc <- if (!is.null(color_map)) {
        unname(color_map[as.character(s_val)])
      } else if (!is.null(trace_col) && trace_col %in% names(s_data)) {
        NULL
      } else {
        .base_color
      }

      label <- if (identical(s_val, "default")) "" else as.character(s_val)

      # Store the spec for later rendering
      trace_specs[[length(trace_specs) + 1L]] <- list(
        data = s_data, color = lc, label = label, gi = gi, s_val = s_val
      )
      trace_group_idx <- c(trace_group_idx, gi)
    }
  }


  # Add the line to the plot
  p <- Reduce(function(p, spec) {
    plotly::add_trace(p,
      data = spec$data,
      x = ~time_value, y = ~.response,
      type = "scatter", mode = "lines",
      line = list(color = spec$color %||% .base_color, width = 1.5),
      hoverinfo = "text",
      text = ~ paste0(
        "Key: ", .data[[group_col]],
        if (!is.null(spec$label) && spec$label != "") paste0("<br>", spec$label) else "",
        "<br>Date: ", time_value,
        "<br>Value: ", round(.response, 3)
      ),
      name = spec$label,
      legendgroup = spec$label,
      showlegend = (spec$gi == 1),
      visible = (spec$gi == 1)
    )
  }, trace_specs, init = plotly::plot_ly())

  # Creates the buttons for the dropdown menu and updates the plot title
  buttons <- lapply(seq_along(unique_groups), function(gi) {
    is_active <- trace_group_idx == gi
    list(
      method = "update",
      args = list(
        list(visible = as.list(is_active), showlegend = as.list(is_active)),
        list(title = list(text = paste("Key:", unique_groups[gi])))
      ),
      label = unique_groups[gi]
    )
  })

  p <- plotly::layout(p,
    title = list(text = paste("Key:", unique_groups[1])),
    xaxis = list(title = xaxis_title),
    yaxis = list(title = yaxis_title, fixedrange = TRUE),
    legend = list(title = list(text = legend_title)),
    updatemenus = list(
      list(
        type = "dropdown",
        active = 0,
        buttons = buttons,
        x = 0.05, y = 1.15
      )
    )
  ) %>%
    plotly::config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d"))

  return(p)
}

autoplot_interactive_df <- function(p, object, .max_keys) {
  rlang::check_installed("plotly")

  p_plotly <- plotly::ggplotly(p)

  if (!is.infinite(.max_keys) && (".colours" %in% names(object))) {
    trace_names <- purrr::map_chr(p_plotly$x$data, ~ .x$name %||% "")
    keys <- unique(trace_names[trace_names != ""])
    if (length(keys) > .max_keys) {
      # Keys to keep
      keep <- keys[seq_len(.max_keys)]
      p_plotly$x$data <- purrr::map(p_plotly$x$data, \(tr) {
        # Do not display keys that are not kept (still showing legend)
        if ((tr$name %||% "") %in% setdiff(keys, keep)) tr$visible <- "legendonly"
        tr
      })
    }
  }

  # Fix y-axis range
  p_plotly <- p_plotly %>%
    plotly::layout(
      yaxis = list(fixedrange = TRUE)
    ) %>%
    plotly::config(
      modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d")
    )

  return(p_plotly)
}

#' @param .versions Select which versions will be displayed. By default,
#'   a separate line will be shown with the data as it would have appeared on
#'   every day in the archive. This can sometimes become overwhelming. For
#'   example, daily data would display a line for what the data would have looked
#'   like on every single day. To override this, you can select specific dates,
#'   by passing a vector of values here. Alternatively, a sequence can be
#'   automatically created by passing a string like `"2 weeks"` or `"month"`.
#'   For time types where the `time_value` is a date object, any string that
#'   is interpretable by [base::seq.Date()] is allowed.
#'
#'   For `time_type = "integer"`, an integer larger than 1 will give a subset
#'   of versions.
#' @param .mark_versions Logical. Indicate whether to mark each version with
#'   a vertical line. Note that displaying many versions can become busy.
#' @param .max_keys Maximum number of key combinations to display. If the data
#'   contains more key combinations than this limit, a random sample of size
#'   `.max_keys` is displayed, and a warning is issued. Set to `Inf` to
#'   display all keys. Does not apply if `interactive = TRUE`.
#' @param interactive Logical. If `TRUE`, returns an interactive
#'   [plotly::ggplotly()] widget instead of a static [ggplot2::ggplot()] object.
#'   This is especially useful for exploring datasets with many keys.
#'
#' @export
#' @rdname autoplot-epi
#'
#' @examples
#'
#' # -- Use it on an archive
#'
#' autoplot(archive_cases_dv_subset, percent_cli, .versions = "week")
#' autoplot(archive_cases_dv_subset_all_states, percent_cli,
#'   .versions = "week",
#'   .facet_filter = geo_value %in% c("or", "az", "vt", "ms")
#' )
#' autoplot(archive_cases_dv_subset, percent_cli,
#'   .versions = "month",
#'   .facet_filter = geo_value == "ca"
#' )
#' autoplot(archive_cases_dv_subset_all_states, percent_cli,
#'   .versions = "1 month",
#'   .facet_filter = geo_value %in% c("or", "az", "vt", "ms"),
#'   .mark_versions = TRUE
#' )
#' # Just an alias for convenience
#' plot(archive_cases_dv_subset_all_states, percent_cli,
#'   .versions = "1 month",
#'   .facet_filter = geo_value %in% c("or", "az", "vt", "ms"),
#'   .mark_versions = TRUE
#' )
autoplot.epi_archive <- function(object, ...,
                                 .base_color = "black",
                                 .versions = NULL,
                                 .mark_versions = FALSE,
                                 .facet_filter = NULL,
                                 .max_keys = 6,
                                 interactive = FALSE) {
  time_type <- object$time_type
  checkmate::assert_number(.max_keys, lower = 1)
  checkmate::assert_logical(.mark_versions, len = 1L)
  checkmate::assert_logical(interactive, len = 1L)
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
    .versions <- .versions[min_version <= .versions & .versions <= max_version]
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

  eff_max_keys <- if (interactive) Inf else .max_keys

  bp <- autoplot.epi_df(
    finalized, ...,
    .base_color = .base_color, .facet_by = "all",
    .facet_filter = {{ .facet_filter }}, .color_by = "none",
    .max_keys = eff_max_keys
  ) + ggplot2::xlab("Date")

  if (interactive) {
    # Override the facet layer so plotly doesn't generate empty grid panels.
    # We still want `.facet_by = "all"` above to apply `.facet_filter` properly.
    bp <- bp + ggplot2::facet_null()
  }

  geo_and_other_keys <- key_colnames(object, exclude = c("time_value", "version"))
  all_avail <- rlang::syms(as.list(c(
    geo_and_other_keys,
    if (nvars > 1) ".response_name" else NULL
  )))

  snapshots <- purrr::map(
    .versions,
    function(v) {
      dplyr::mutate(epix_as_of(object, v), version = .env$v)
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
  if (".facets" %in% names(bp$data)) {
    snapshots <- snapshots %>%
      dplyr::filter(!is.na(.response), .data$.facets %in% unique(bp$data$.facets))
  } else {
    snapshots <- snapshots %>%
      dplyr::filter(!is.na(.response))
  }

  if (interactive) {
    return(autoplot_interactive_archive(
      snapshots, .base_color, methods::is(.versions, "Date")
    ))
  }


  bp <- bp +
    ggplot2::geom_line(
      data = snapshots,
      mapping = ggplot2::aes(y = .response, color = version, group = interaction(!!!all_avail, version))
    )

  if (methods::is(.versions, "Date")) {
    bp <- bp + ggplot2::scale_color_viridis_c(name = "Version", trans = "date")
  } else {
    bp <- bp + ggplot2::scale_color_viridis_c(name = "Version")
  }

  if (.mark_versions) {
    bp <- bp +
      ggplot2::geom_vline(
        data = snapshots,
        ggplot2::aes(color = version, xintercept = version),
        linewidth = .5,
        linetype = 3,
        show.legend = FALSE
      )
  }
  # make the finalized layer last
  bp$layers <- rev(bp$layers)

  bp
}

autoplot_interactive_archive <- function(snapshots, .base_color, .versions_are_dates) {
  # Build color palette
  all_versions <- sort(unique(snapshots$version))
  n_versions <- length(all_versions)
  if (n_versions == 1L) {
    version_colors <- .base_color
  } else {
    version_colors <- c(
      grDevices::hcl.colors(n_versions - 1L, palette = "viridis"),
      .base_color
    )
  }
  color_map <- stats::setNames(version_colors, as.character(all_versions))

  # Formatted labels for the legend
  if (.versions_are_dates) {
    snapshots$version_label <- as.character(as.Date(snapshots$version, origin = "1970-01-01"))
  } else {
    snapshots$version_label <- as.character(snapshots$version)
  }

  # Use the generalized dropdown helper
  autoplot_plotly_dropdown(
    data = snapshots,
    group_col = ".facets",
    trace_col = "version",
    color_map = color_map,
    legend_title = "Version"
  )
}


#' @export
#' @rdname autoplot-epi
plot.epi_df <- function(x, ...) {
  autoplot(x, ...)
}

#' @export
#' @rdname autoplot-epi
plot.epi_archive <- function(x, ...) {
  autoplot(x, ...)
}
