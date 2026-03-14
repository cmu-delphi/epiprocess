#' Plot a heatmap for an epi_df
#'
#' @inheritParams autoplot-epi
#' @param x An `epi_df` object.
#' @param fill <[`tidy-select`][dplyr::dplyr_tidy_select]> The column to use for the fill aesthetic.
#'   If NULL, the first numeric non-key column is used.
#' @param ... Additional arguments passed to [ggplot2::geom_tile()].
#'
#' @return A [ggplot2::ggplot] object.
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c .data theme_get theme_gray theme_bw
#' @importFrom rlang enquo quo_is_null sym !! inject syms
#' @importFrom dplyr mutate
#' @importFrom cli cli_abort
#' @export
plot_heatmap <- function(x, fill = NULL, ...,
                         .max_keys = 60) {
  # Validate that x is an epi_df
  if (!inherits(x, "epi_df")) {
    cli::cli_abort("x must be an `epi_df` object.",
      class = "epiprocess__plot_heatmap__invalid_x"
    )
  }

  fill_quo <- rlang::enquo(fill)
  key_cols <- key_colnames(x)
  non_key_cols <- setdiff(names(x), key_cols)

  # --- Reuse autoplot utility for variable selection
  vars <- autoplot_check_viable_response_vars(x, !!fill_quo, non_key_cols = non_key_cols)
  fill_name <- names(vars)[1]
  fill_quo <- rlang::sym(fill_name)

  # --- Reuse autoplot utility for key subsampling
  geo_and_other_keys <- key_colnames(x, exclude = "time_value")
  x <- autoplot_subsample_keys(
    x,
    geo_and_other_keys = geo_and_other_keys,
    .max_keys = .max_keys,
    .interactive = FALSE,
    .facet_used = FALSE
  )

  # We use interaction of all geo/other keys for the y-axis
  plot_df <- x %>%
    dplyr::mutate(
      .y_axis = interaction(!!!rlang::syms(geo_and_other_keys), sep = "; ")
    )

  # Create plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(
    x = time_value,
    y = .y_axis,
    fill = !!fill_quo
  )) +
    ggplot2::geom_tile(color = "white", linewidth = 0.1, ...) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(x = "Date", y = "")

  if (identical(ggplot2::theme_get(), ggplot2::theme_gray())) {
    p <- p + ggplot2::theme_bw()
  }

  return(p)
}
