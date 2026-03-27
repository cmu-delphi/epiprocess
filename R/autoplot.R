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
#'   display all keys. Subsampling is not performed if `.interactive = TRUE`
#'   (though a similar limit may be applied to initial legend visibility) when
#'   `.facet_to_dropdown = FALSE`.
#' @param .interactive Logical. If `TRUE`, returns an interactive
#'   [plotly::ggplotly()] widget instead of a static [ggplot2::ggplot()] object.
#'   This is especially useful for exploring datasets with many keys. Default is
#'   `FALSE`.
#' @param .facet_to_dropdown Logical. If `TRUE`, and `.interactive = TRUE`, any
#'   facets will be converted into a dropdown menu. This is useful for
#'   maximizing screen real estate when there are many facets. Default is
#'   `FALSE`.
#'
#' @return A [`ggplot2::ggplot`] object, OR [`plotly::plotly`] object if `.interactive = TRUE`
#' @export
#' @name autoplot-epi
#'
#' @examples
#'
#' # -- Use it on an `epi_df`
#' autoplot(cases_deaths_subset, case_rate_7d_av, death_rate_7d_av)
#'
#' # Launch interactive version in web browser:
#' autoplot(cases_deaths_subset, case_rate_7d_av, death_rate_7d_av,
#'   .interactive = TRUE
#' )
#'
#' # Use dropdowns instead of facets for interactive plots
#' autoplot(cases_deaths_subset, case_rate_7d_av, death_rate_7d_av,
#'   .interactive = TRUE, .facet_to_dropdown = TRUE
#' )
#'
#' autoplot(cases_deaths_subset, case_rate_7d_av,
#'   .color_by = "none",
#'   .facet_by = "geo_value"
#' )
#'
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
#'
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
  .max_facets = deprecated(),
  .max_keys = 10,
  .interactive = FALSE,
  .facet_to_dropdown = FALSE
) {
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
  checkmate::assert_logical(.interactive, len = 1L, any.missing = FALSE)
  checkmate::assert_logical(.facet_to_dropdown, len = 1L, any.missing = FALSE)
  checkmate::assert_number(.max_keys, lower = 1)
  checkmate::assert_character(.base_color, len = 1)

  key_cols <- key_colnames(object)
  non_key_cols <- vctrs::vec_set_difference(names(object), key_cols)
  geo_and_other_keys <- key_colnames(object, exclude = "time_value")

  # --- check for numeric variables
  vars <- autoplot_check_viable_response_vars(object, ..., non_key_cols = non_key_cols)
  nvars <- length(vars)

  # --- create a viable df to plot
  pos <- tidyselect::eval_select(
    rlang::expr(c("time_value", tidyselect::all_of(geo_and_other_keys), tidyselect::all_of(vars))), object,
    allow_rename = FALSE
  )
  object <- tidyr::pivot_longer(
    object[pos], tidyselect::all_of(vars),
    values_to = ".response",
    names_to = ".response_name"
  )
  all_avail_names <- c(
    geo_and_other_keys,
    if (nvars > 1) ".response_name" else NULL
  )
  # Get facet and color variables
  facet_vars <- autoplot_resolve_vars(.facet_by, geo_and_other_keys, nvars, all_avail_names)
  color_vars <- autoplot_resolve_vars(.color_by, geo_and_other_keys, nvars, all_avail_names, color = TRUE)
  color_vars <- vctrs::vec_set_difference(color_vars, facet_vars)

  color_expr <- autoplot_make_interaction_expr(color_vars)
  facet_expr <- autoplot_make_interaction_expr(facet_vars)

  object <- object %>%
    dplyr::mutate(
      .colours = !!color_expr,
      .facets = !!facet_expr
    )

  # Drop .colours when redundant (each facet panel has only one color level)
  if (".colours" %in% names(object)) {
    if (".facets" %in% names(object)) {
      needs_color <- any(vapply(
        split(object$.colours, object$.facets),
        function(x) length(unique(x)) > 1L,
        logical(1)
      ))
    } else {
      needs_color <- length(unique(object$.colours)) > 1L
    }
    if (!needs_color) {
      object$.colours <- NULL
    }
  }

  # Apply facet filter if provided
  if (!rlang::quo_is_null(.facet_filter)) {
    object <- dplyr::filter(object, !!.facet_filter) %>%
      dplyr::mutate(dplyr::across(
        tidyselect::any_of(c(".facets", ".colours")),
        droplevels
      ))
  }

  # Subsample keys if needed
  object <- autoplot_subsample_keys(object, .max_keys, .interactive)

  # If interactive and dropdowns requested, build dropdown plot
  if (.interactive && .facet_to_dropdown && !is.null(facet_expr)) {
    trace_col <- if (".colours" %in% names(object)) {
      ".colours"
    } else if (length(vars) > 1 && .color_by == ".response") {
      ".response_name"
    } else {
      NULL
    }

    # Set y-axis title based on facet variable
    yaxis_title <- if (.facet_by %in% c("all", ".response")) "" else paste0(vars, collapse = ", ")

    return(autoplot_plotly_dropdown(
      data = object,
      group_col = ".facets",
      trace_col = trace_col,
      .base_color = .base_color,
      yaxis_title = yaxis_title,
      dropdown_prefix = autoplot_get_label(.facet_by, facet_vars, format = "prefix")
    ))
  }

  # --- build mappings
  plot_mappings <- list()
  if (".colours" %in% names(object)) {
    plot_mappings$colour <- rlang::expr(.data$.colours)
  } else {
    plot_mappings$group <- rlang::expr(interaction(!!!rlang::syms(all_avail_names), sep = "; "))
  }

  p <- rlang::inject(ggplot2::ggplot(object, ggplot2::aes(
    x = .data$time_value,
    y = .data$.response,
    !!!plot_mappings
  )))

  if (identical(ggplot2::theme_get(), ggplot2::theme_gray())) {
    p <- p + ggplot2::theme_bw()
  }

  # --- add layers
  if (!is.null(plot_mappings$colour)) {
    p <- p + suppressWarnings(ggplot2::geom_line(key_glyph = "timeseries")) +
      ggplot2::scale_colour_viridis_d(name = autoplot_get_label(.color_by, color_vars, format = "none"))
  } else {
    p <- p + suppressWarnings(ggplot2::geom_line(
      color = .base_color,
      key_glyph = "timeseries"
    ))
  }

  use_facets <- ".facets" %in% names(object)

  if (use_facets) {
    p <- p + ggplot2::facet_wrap(~.facets, scales = "free_y")
    p <- p + ggplot2::labs(subtitle = autoplot_get_label(.facet_by, facet_vars, format = "facet"))
  }

  # Omit y-label when the response name already appears in the facet strip labels.
  y_label <- if (use_facets && ".response_name" %in% facet_vars) "" else paste(names(vars), collapse = ", ")
  p <- p + ggplot2::ylab(y_label)

  if (.interactive) {
    return(autoplot_interactive(p, object, .max_keys, .facet_by))
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
  vars <- tidyselect::eval_select(rlang::expr(c(...)), object, allow_rename = FALSE)
  if (rlang::is_empty(vars)) { # find them automatically if unspecified
    if (length(allowed) == 1L) {
      vars <- names(allowed)[1]
      cli::cli_warn(
        "Plot variable was unspecified. Automatically selecting {.var {vars}}.",
        class = "epiprocess__unspecified_plot_var",
        call = call
      )
    } else if ("value" %in% names(allowed)) {
      vars <- "value"
      cli::cli_warn(
        "Plot variable was unspecified. Automatically selecting {.var value}.",
        class = "epiprocess__unspecified_plot_var",
        call = call
      )
    } else {
      vars <- names(allowed)
      cli::cli_warn(
        c(
          "Plot variable was unspecified. Automatically selecting all numeric columns: {.var {names(allowed)}}.",
          ">" = "To plot specific columns, specify them, e.g. `autoplot(x, {names(allowed)[1]})`."
        ),
        class = "epiprocess__unspecified_plot_var",
        call = call
      )
    }
  } else { # if variables were specified, ensure that they are numeric
    vars <- names(vars)
    ok <- vars %in% names(allowed)
    if (!any(ok)) {
      cli::cli_abort(
        "{?The requested variable /None of the requested variables }{.var {vars}} {?is not/are} numeric.",
        class = "epiprocess__all_requested_vars_not_numeric",
        call = call,
        qty = length(vars)
      )
    } else if (!all(ok)) {
      cli::cli_warn(
        c(
          "`autoplot()` cannot display {.var {vars[!ok]}}, as they are not numeric",
          ">" = "Only plotting {.var {vars[ok]}}."
        ),
        class = "epiprocess__some_requested_vars_not_numeric",
        call = call,
        qty = sum(ok)
      )
      vars <- vars[ok]
    }
  }
  vars
}


autoplot_subsample_keys <- function(
  object, .max_keys, .interactive, .caller = "autoplot"
) {
  if (.interactive || is.infinite(.max_keys)) {
    return(object)
  }

  facet_lvls <- if (".facets" %in% names(object)) {
    levels(droplevels(object$.facets))
  } else {
    character(0)
  }
  color_col <- if (".colours" %in% names(object)) {
    ".colours"
  } else if (".key_interaction" %in% names(object)) {
    ".key_interaction"
  } else {
    NULL
  }
  color_lvls <- if (!is.null(color_col)) {
    levels(droplevels(object[[color_col]]))
  } else {
    character(0)
  }

  n_facets_all <- max(1L, length(facet_lvls))
  n_colors_all <- max(1L, length(color_lvls))

  if (n_facets_all * n_colors_all <= .max_keys) {
    return(object)
  }

  # If one dimension is small (< half all combinations),
  # keep it whole and reduce only the other.
  # Otherwise split proportionally.
  if (2 * n_colors_all < .max_keys) {
    n_c <- n_colors_all
    n_f <- floor(.max_keys / n_c)
  } else if (2 * n_facets_all < .max_keys) {
    n_f <- n_facets_all
    n_c <- floor(.max_keys / n_f)
  } else {
    n_f <- floor(sqrt(.max_keys * n_facets_all / n_colors_all))
    n_c <- floor(.max_keys / n_f)
    n_f <- floor(.max_keys / n_c)
  }
  n_f <- max(1L, min(n_facets_all, as.integer(n_f)))
  n_c <- max(1L, min(n_colors_all, as.integer(n_c)))

  if (n_f < n_facets_all) {
    object <- object[object$.facets %in% sample(facet_lvls, n_f), ]
  }
  if (n_c < n_colors_all) {
    object <- object[object[[color_col]] %in% sample(color_lvls, n_c), ]
  }
  object <- dplyr::mutate(object, dplyr::across(
    tidyselect::any_of(c(".facets", ".colours", ".key_interaction")), droplevels
  ))
  caller <- .caller

  msg <- c(
    "Too many key combinations to display clearly. Showing {n_f * n_c} of {n_facets_all * n_colors_all}.",
    ">" = "To plot all keys, use {.code {caller}(..., .max_keys = Inf)}."
  )
  if (caller == "autoplot") {
    msg <- c(
      msg,
      ">" = "To explore all keys interactively, use {.code autoplot(..., .interactive = TRUE)}.",
      if (length(facet_lvls) > 0L) {
        c(">" = "To plot specific keys, use {.code autoplot(..., .facet_filter = ...)}.")
      }
    )
  }
  cli::cli_warn(msg, class = "epiprocess__autoplot__max_keys_exceeded")
  object
}


autoplot_plotly_dropdown <- function(
  data, group_col, trace_col = NULL,
  color_map = NULL, .base_color = "#3A448F",
  xaxis_title = "Date", yaxis_title = "", legend_title = "",
  dropdown_prefix = "Key: "
) {
  # Initialize color map if trace_col is provided but map is not
  if (is.null(color_map) && !is.null(trace_col) && trace_col %in% names(data)) {
    all_trace_vals <- sort(unique(data[[trace_col]]))
    n_colors <- length(all_trace_vals)
    colors <- grDevices::hcl.colors(n_colors, palette = "viridis")
    color_map <- stats::setNames(colors, all_trace_vals)
  }

  # Split data by the group column to avoid quadratic filtering performance
  unique_groups <- as.character(sort(unique(data[[group_col]])))
  group_data_list <- dplyr::group_split(data, as.character(.data[[group_col]]))

  # Identify each dropdown option and its traces
  trace_specs <- purrr::map2(group_data_list, unique_groups, function(g_data, g) {
    # Handle multiple lines within one dropdown selection
    if (!is.null(trace_col) && trace_col %in% names(g_data)) {
      sub_trace_vals <- sort(unique(g_data[[trace_col]]))
    } else {
      # one line per dropdown option
      sub_trace_vals <- "default"
    }

    # Creates the individual lines for a single dropdown selection
    purrr::map(sub_trace_vals, function(s_val) {
      s_data <- if (identical(s_val, "default")) g_data else g_data[g_data[[trace_col]] == s_val, ]
      s_data <- s_data[order(s_data$time_value), ]

      # "metadata" for the specific line
      lc <- if (!is.null(color_map)) {
        unname(color_map[as.character(s_val)])
      } else if (!is.null(trace_col) && trace_col %in% names(s_data)) {
        NULL
      } else {
        .base_color
      }

      list(
        data = s_data,
        color = lc %||% .base_color,
        label = if (identical(s_val, "default")) "" else as.character(s_val),
        g_val = g,
        show_legend = length(sub_trace_vals) > 1
      )
    })
  }) %>%
    purrr::list_flatten()

  # Add the line to the plot
  p <- purrr::reduce(trace_specs, function(p, spec) {
    plotly::add_trace(p,
      data = spec$data,
      x = ~time_value, y = ~.response,
      type = "scatter", mode = "lines",
      line = list(color = spec$color, width = 1.5),
      hoverinfo = "text",
      text = ~ paste0(
        dropdown_prefix, get(group_col),
        if (spec$label != "") paste0("<br>", spec$label) else "",
        "<br>Date: ", time_value,
        "<br>Value: ", round(.response, 3)
      ),
      name = spec$label,
      legendgroup = spec$label,
      showlegend = (spec$g_val == unique_groups[1]) && spec$show_legend,
      visible = (spec$g_val == unique_groups[1])
    )
  }, .init = plotly::plot_ly())

  # Creates the buttons for the dropdown menu and updates the plot title
  trace_group_vals <- purrr::map_chr(trace_specs, ~ .x$g_val)
  buttons <- purrr::map(unique_groups, function(g) {
    is_active <- trace_group_vals == g
    show_legend <- is_active & purrr::map_lgl(trace_specs, ~ .x$show_legend)
    list(
      method = "update",
      args = list(
        list(visible = as.list(is_active), showlegend = as.list(show_legend)),
        list(title = list(text = paste0(dropdown_prefix, g)))
      ),
      label = g
    )
  })

  p %>%
    plotly::layout(
      title = list(text = paste0(dropdown_prefix, unique_groups[1])),
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
}

autoplot_resolve_vars <- function(opt, geo_and_other_keys, nvars, all_avail_names, color = FALSE) {
  vars <- switch(opt,
    all_keys = geo_and_other_keys,
    geo_value = "geo_value",
    other_keys = vctrs::vec_set_difference(geo_and_other_keys, "geo_value"),
    .response = if (nvars > 1) ".response_name" else character(0),
    all = all_avail_names,
    none = character(0)
  )

  if (color && nvars > 1) {
    vars <- vctrs::vec_set_union(vars, ".response_name")
  }
  vars
}

autoplot_make_interaction_expr <- function(vars) {
  if (length(vars) == 0) {
    return(NULL)
  }
  if (length(vars) == 1) {
    return(rlang::expr(as.factor(!!rlang::sym(vars))))
  }
  rlang::expr(interaction(!!!rlang::syms(vars), sep = "; "))
}

autoplot_get_label <- function(type, vars = character(0), format = c("none", "prefix", "facet")) {
  format <- rlang::arg_match(format)
  if (type == "none" || length(vars) == 0) {
    return(if (format == "facet") NULL else "")
  }

  has_resp <- ".response_name" %in% vars
  keys <- vctrs::vec_set_difference(vars, ".response_name")

  if (length(keys) == 0) {
    label <- "Indicator"
  } else {
    if (length(keys) == 1 && keys == "geo_value") {
      key_label <- if (format == "prefix") "Geo" else "Location"
    } else if (length(keys) == 1) {
      key_label <- keys
    } else {
      key_label <- if (type == "other_keys") paste(keys, collapse = " ; ") else "Keys"
    }

    if (has_resp) {
      label <- paste0(key_label, " \u00d7 Indicator(s)")
    } else {
      label <- key_label
    }
  }

  if (format == "prefix") {
    return(paste0(label, ": "))
  }
  if (format == "facet") {
    return(paste0("Facet by: ", label))
  }

  label
}

autoplot_interactive <- function(p, object, .max_keys, .facet_by = "none") {
  p_plotly <- plotly::ggplotly(p)

  if (!is.infinite(.max_keys) &&
        (".colours" %in% names(object))) {
    trace_names <- purrr::map_chr(p_plotly$x$data, ~ .x$name %||% "")
    keys <- unique(trace_names[trace_names != ""])
    if (length(keys) > .max_keys) {
      # Keys to keep
      keep <- sample(keys, .max_keys)
      p_plotly$x$data <- purrr::map(p_plotly$x$data, \(tr) {
        # Do not display keys that are not kept (still showing legend)
        if ((tr$name %||% "") %in% vctrs::vec_set_difference(keys, keep)) tr$visible <- "legendonly"
        tr
      })
      cli::cli_inform(
        c(
          "Plotting {.val {(length(keys))}} keys can be hard to read.",
          "i" = "Showing a random subset of {.val {(.max_keys)}} keys by default.",
          ">" = "Select additional keys in the legend on the right.",
          ">" = "To see all keys, set {.code .max_keys = Inf} or use {.code plotly::style(p, visible = TRUE)}."
        ),
        class = "epiprocess__autoplot_interactive_subsetting"
      )
    }
  }

  # Fix y-axis range for all facet panels (yaxis, yaxis2, yaxis3, ...)
  yaxis_names <- grep("^yaxis", names(p_plotly$x$layout), value = TRUE)
  if (length(yaxis_names) == 0L) yaxis_names <- "yaxis"
  for (ax in yaxis_names) {
    p_plotly$x$layout[[ax]]$fixedrange <- TRUE
  }
  p_plotly <- p_plotly %>%
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
                                 .interactive = FALSE,
                                 .facet_to_dropdown = FALSE) {
  time_type <- object$time_type
  checkmate::assert_number(.max_keys, lower = 1)
  checkmate::assert_logical(.mark_versions, len = 1L, any.missing = FALSE)
  checkmate::assert_logical(.interactive, len = 1L, any.missing = FALSE)
  if (time_type == "custom") {
    cli::cli_abort(
      "This `epi_archive` has custom `time_type`. This is currently unsupported.",
      class = "epiprocess__autoplot_archive_custom_time_type"
    )
  }

  max_version <- max(object$DT$version)
  min_version <- min(object$DT$version)

  tt_lookup <- c("day" = "day", "week" = "week", "yearmonth" = "month")
  .versions <- .versions %||% ifelse(time_type == "integer", 1L, unname(tt_lookup[time_type]))
  if ((is.character(.versions) || rlang::is_bare_numeric(.versions)) && length(.versions) == 1L) {
    # Interpret `.versions` as a period (even if archive versions are also bare numeric...)
    if (is.numeric(.versions)) .versions <- round(abs(.versions))
    .versions <- seq(min_version, max_version, by = .versions)
  } else if (inherits(.versions, "Date") || is.numeric(.versions)) {
    old_n_versions <- length(.versions)
    .versions <- .versions[min_version <= .versions & .versions <= max_version]
    if (length(.versions) != old_n_versions) {
      cli::cli_inform(paste(
        "Removed entries from `.versions` that weren't in the range of archive",
        "versions with update rows."
      ))
    }
  } else {
    cli::cli_abort(
      "Requested `.versions` don't appear to match the available `time_type`.",
      class = "epiprocess__autoplot_archive_bad_versions"
    )
  }

  split_out_finalized <- !.interactive
  if (split_out_finalized) {
    .versions <- .versions[.versions != max_version]
  }

  finalized <- epix_as_of(object, max_version)
  key_cols <- key_colnames(finalized)
  non_key_cols <- vctrs::vec_set_difference(names(finalized), key_cols)
  vars <- autoplot_check_viable_response_vars(finalized, ..., non_key_cols = non_key_cols)
  nvars <- length(vars)

  eff_max_keys <- if (.interactive) Inf else .max_keys

  bp <- autoplot(
    finalized, ...,
    .base_color = .base_color, .facet_by = "all",
    .facet_filter = {{ .facet_filter }}, .color_by = "none",
    .max_keys = eff_max_keys, .facet_to_dropdown = FALSE
  ) + ggplot2::xlab("Date")

  geo_and_other_keys <- key_colnames(object, exclude = c("time_value", "version"))

  snapshots <- purrr::map(
    .versions,
    function(v) {
      dplyr::mutate(epix_as_of(object, v), version = .env$v)
    }
  ) %>%
    purrr::list_rbind()

  if (nvars > 1) {
    snapshots <- tidyr::pivot_longer(
      snapshots, tidyselect::all_of(vars),
      values_to = ".response",
      names_to = ".response_name"
    )
  } else {
    snapshots <- dplyr::rename(snapshots, .response := !!vars) # nolint: object_usage_linter
  }

  all_avail_names <- c(
    geo_and_other_keys,
    if (nvars > 1) ".response_name" else NULL
  )
  all_avail <- rlang::syms(all_avail_names)

  snapshots <- snapshots %>%
    dplyr::mutate(.facets = interaction(!!!all_avail, sep = "; "))

  snapshots <- snapshots %>%
    dplyr::filter(!is.na(.response), .data$.facets %in% unique(bp$data$.facets))

  if (.interactive && .facet_to_dropdown) {
    return(autoplot_interactive_archive(
      snapshots, .base_color, "all",
      facet_vars = all_avail_names
    ))
  }

  bp <- bp +
    ggplot2::geom_line(
      data = snapshots,
      mapping = ggplot2::aes(y = .response, color = version, group = interaction(!!!all_avail, version, sep = "; "))
    )

  if (inherits(.versions, "Date")) {
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

  if (.interactive) {
    return(autoplot_interactive(bp, snapshots, .max_keys, .facet_by = "all"))
  }

  bp
}

autoplot_interactive_archive <- function(snapshots, .base_color, .facet_by, facet_vars = character(0)) {
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

  # Use the generalized dropdown helper
  prefix <- autoplot_get_label(.facet_by, facet_vars, format = "prefix")
  autoplot_plotly_dropdown(
    data = snapshots,
    group_col = ".facets",
    trace_col = "version",
    color_map = color_map,
    legend_title = "Version",
    dropdown_prefix = prefix
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
