#' Plot a heatmap for an epi_df
#'
#' @inheritParams autoplot-epi
#' @param x An `epi_df` object.
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more unquoted
#'   expressions separated by commas. Variable names can be used as if they
#'   were positions in the data frame, so expressions like `x:y` can
#'   be used to select a range of variables. If no variables are specified,
#'   all numeric columns will be plotted and a warning issued.
#' @return A [ggplot2::ggplot] object.
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c .data theme_get theme_gray theme_bw facet_wrap labs
#' @importFrom rlang sym !! inject syms
#' @importFrom dplyr mutate rename
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom cli cli_abort
#' @export
#'
#' @examples
#' # Use it on an `epi_df`
#' plot_heatmap(cases_deaths_subset, case_rate_7d_av)
#'
#' # Plotting multiple variables
#' plot_heatmap(cases_deaths_subset, case_rate_7d_av, death_rate_7d_av)
plot_heatmap <- function(x, ..., .max_keys = 60) {
  # Validate that x is an epi_df
  if (!inherits(x, "epi_df")) {
    cli::cli_abort("x must be an `epi_df` object.",
      class = "epiprocess__plot_heatmap__invalid_x"
    )
  }

  key_cols <- key_colnames(x)
  non_key_cols <- setdiff(names(x), key_cols)

  # Variable selection
  vars <- autoplot_check_viable_response_vars(x, ..., non_key_cols = non_key_cols)
  nvars <- length(vars)

  # Key subsampling
  geo_and_other_keys <- key_colnames(x, exclude = "time_value")
  x <- autoplot_subsample_keys(
    x,
    geo_and_other_keys = geo_and_other_keys,
    .max_keys = .max_keys,
    .interactive = FALSE,
    .facet_used = nvars > 1
  )

  # Create a valid df to plot based on selected vars
  pos <- tidyselect::eval_select(
    rlang::expr(c("time_value", tidyselect::all_of(geo_and_other_keys), tidyselect::all_of(vars))), x,
    allow_rename = FALSE
  )
  if (nvars > 1) {
    x <- tidyr::pivot_longer(
      x[pos], tidyselect::all_of(vars),
      values_to = ".response",
      names_to = ".response_name"
    )
  } else {
    x <- dplyr::rename(x[pos], .response := !!vars) # nolint: object_usage_linter
  }

  # We use interaction of all geo/other keys for the y-axis
  plot_df <- x %>%
    dplyr::mutate(
      .y_axis = interaction(!!!rlang::syms(geo_and_other_keys), sep = "; ")
    )

  # Create plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(
    x = time_value,
    y = .y_axis,
    fill = .response
  )) +
    ggplot2::geom_tile(
      color = "white",
      linewidth = min(0.1, 1 / length(unique(plot_df$time_value)))
    ) +
    ggplot2::scale_fill_viridis_c(name = "Value") +
    ggplot2::labs(x = "Date", y = "") +
    ggplot2::coord_cartesian(expand = FALSE)

  if (nvars > 1) {
    p <- p + ggplot2::facet_wrap(~.response_name, scales = "free_y")
  }

  if (identical(ggplot2::theme_get(), ggplot2::theme_gray())) {
    p <- p + ggplot2::theme_bw()
  }

  return(p)
}
