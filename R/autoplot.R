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
#' @param .base_color Lines will be shown with this color. For example, with a
#'   single numeric variable and faceting by `geo_value`, all locations would
#'   share the same color line.
#' @param .max_facets Cut down of the number of facets displayed. Especially
#'   useful for testing when there are many `geo_value`'s or keys.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' autoplot(jhu_csse_daily_subset, cases, death_rate_7d_av)
#' autoplot(jhu_csse_daily_subset, case_rate_7d_av, .facet_by = "geo_value")
#' autoplot(jhu_csse_daily_subset, case_rate_7d_av,
#'   .color_by = "none",
#'   .facet_by = "geo_value"
#' )
#' autoplot(jhu_csse_daily_subset, case_rate_7d_av, .color_by = "none", 
#'   .base_color = "red", .facet_by = "geo_value")
#' 
#' # .base_color specification won't have any effect due .color_by default
#' autoplot(jhu_csse_daily_subset, case_rate_7d_av,
#'   .base_color = "red", .facet_by = "geo_value") 
autoplot.epi_df <- function(
    object, ...,
    .color_by = c("all_keys", "geo_value", "other_keys", ".response", "all", "none"),
    .facet_by = c(".response", "other_keys", "all_keys", "geo_value", "all", "none"),
    .base_color = "#3A448F",
    .max_facets = Inf) {
  .color_by <- match.arg(.color_by)
  .facet_by <- match.arg(.facet_by)

  arg_is_scalar(.max_facets)
  if (is.finite(.max_facets)) arg_is_int(.max_facets)
  arg_is_chr_scalar(.base_color)

  key_cols <- key_colnames(object)
  non_key_cols <- setdiff(names(object), key_cols)
  geo_and_other_keys <- kill_time_value(key_cols)

  # --- check for numeric variables
  allowed <- purrr::map_lgl(object[non_key_cols], is.numeric)
  allowed <- allowed[allowed]
  if (length(allowed) == 0) {
    cli::cli_abort("No numeric variables were available to plot automatically.")
  }
  vars <- tidyselect::eval_select(rlang::expr(c(...)), object)
  if (rlang::is_empty(vars)) { # find them automatically if unspecified
    vars <- tidyselect::eval_select(names(allowed)[1], object)
    cli::cli_warn(
      "Plot variable was unspecified. Automatically selecting {.var {names(allowed)[1]}}."
    )
  } else { # if variables were specified, ensure that they are numeric
    ok <- names(vars) %in% names(allowed)
    if (!any(ok)) {
      cli::cli_abort(
        "None of the requested variables {.var {names(vars)}} are numeric."
      )
    } else if (!all(ok)) {
      cli::cli_warn(c(
        "Only the requested variables {.var {names(vars)[ok]}} are numeric.",
        i = "`autoplot()` cannot display {.var {names(vars)[!ok]}}."
      ))
      vars <- vars[ok]
    }
  }

  # --- create a viable df to plot
  pos <- tidyselect::eval_select(
    rlang::expr(c("time_value", geo_and_other_keys, names(vars))), object
  )
  if (length(vars) > 1) {
    object <- tidyr::pivot_longer(
      object[pos], tidyselect::all_of(names(vars)),
      values_to = ".response",
      names_to = ".response_name"
    )
  } else {
    object <- dplyr::rename(object[pos], .response := !!names(vars))
  }
  all_keys <- rlang::syms(as.list(geo_and_other_keys))
  other_keys <- rlang::syms(as.list(setdiff(geo_and_other_keys, "geo_value")))
  all_avail <- rlang::syms(as.list(c(geo_and_other_keys, ".response_name")))

  object <- object %>%
    dplyr::mutate(
      .colours = switch(.color_by,
        all_keys = interaction(!!!all_keys, sep = "/"),
        geo_value = geo_value,
        other_keys = interaction(!!!other_keys, sep = "/"),
        all = interaction(!!!all_avail, sep = "/"),
        NULL
      ),
      .facets = switch(.facet_by,
        all_keys = interaction(!!!all_keys, sep = "/"),
        geo_value = as.factor(geo_value),
        other_keys = interaction(!!!other_keys, sep = "/"),
        all = interaction(!!!all_avail, sep = "/"),
        NULL
      )
    )

  if (.max_facets < Inf && ".facets" %in% names(object)) {
    n_facets <- nlevels(object$.facets)
    if (n_facets > .max_facets) {
      top_n <- levels(as.factor(object$.facets))[seq_len(.max_facets)]
      object <- dplyr::filter(object, .facets %in% top_n) %>%
        dplyr::mutate(.facets = droplevels(.facets))
      if (".colours" %in% names(object)) {
        object <- dplyr::mutate(object, .colours = droplevels(.colours))
      }
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
