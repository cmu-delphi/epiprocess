#' Detect outliers
#'
#' Applies one or more outlier detection methods to a given signal variable, and
#' optionally aggregates the outputs to create a consensus result. See the
#' [outliers
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/outliers.html) for
#' examples.
#'
#' @template x-y
#' @param methods A tibble specifying the method(s) to use for outlier
#'   detection, with one row per method, and the following columns:
#'   * `method`: Either "rm" or "stl", or a custom function for outlier
#'   detection; see details for further explanation.
#'   * `args`: Named list of arguments that will be passed to the detection
#'   method.
#'   * `abbr`: Abbreviation to use in naming output columns with results from
#'   this method.
#' @param combiner String, one of "median", "mean", or "none", specifying how to
#'   combine results from different outlier detection methods for the thresholds
#'   determining whether a particular observation is classified as an outlier,
#'   as well as a replacement value for any outliers.  If "none", then no
#'   summarized results are calculated. Note that if the number of `methods`
#'   (number of rows) is odd, then "median" is equivalent to a majority vote for
#'   purposes of determining whether a given observation is an outlier.
#' @template detect-outlr-return
#'
#' @details Each outlier detection method, one per row of the passed `methods`
#'   tibble, is a function that must take as its first two arguments `x` and
#'   `y`, and then any number of additional arguments. The function must return
#'   a tibble with the number of rows equal to `length(y)`, and with columns
#'   `lower`, `upper`, and `replacement`, representing lower and upper bounds
#'   for what would be considered an outlier, and a posited replacement value,
#'   respectively.
#'
#'  For convenience, the outlier detection method can be specified (in the
#'   `method` column of `methods`) by a string "rm", shorthand for
#'   `detect_outlr_rm()`, which detects outliers via a rolling median; or by
#'   "stl", shorthand for `detect_outlr_stl()`, which detects outliers via an
#'   STL decomposition.
#'
#' @export
#' @importFrom dplyr select
#' @examples
#' detection_methods <- dplyr::bind_rows(
#'   dplyr::tibble(
#'     method = "rm",
#'     args = list(list(
#'       detect_negatives = TRUE,
#'       detection_multiplier = 2.5
#'     )),
#'     abbr = "rm"
#'   ),
#'   dplyr::tibble(
#'     method = "stl",
#'     args = list(list(
#'       detect_negatives = TRUE,
#'       detection_multiplier = 2.5,
#'       seasonal_period = 7
#'     )),
#'     abbr = "stl_seasonal"
#'   ),
#'   dplyr::tibble(
#'     method = "stl",
#'     args = list(list(
#'       detect_negatives = TRUE,
#'       detection_multiplier = 2.5,
#'       seasonal_period = NULL
#'     )),
#'     abbr = "stl_nonseasonal"
#'   )
#' )
#'
#' x <- incidence_num_outlier_example %>%
#'   dplyr::select(geo_value, time_value, cases) %>%
#'   as_epi_df() %>%
#'   group_by(geo_value) %>%
#'   mutate(outlier_info = detect_outlr(
#'     x = time_value, y = cases,
#'     methods = detection_methods,
#'     combiner = "median"
#'   )) %>%
#'   unnest(outlier_info)
detect_outlr <- function(x = seq_along(y), y,
                         methods = tibble::tibble(
                           method = "rm",
                           args = list(list()),
                           abbr = "rm"
                         ),
                         combiner = c("median", "mean", "none")) {
  # Validate combiner
  combiner <- match.arg(combiner)

  # Validate that x contains all distinct values
  if (any(duplicated(x))) {
    cli_abort(
      "`x` cannot contain duplicate values. (If being run on a
      column in an `epi_df`, did you group by relevant key variables?)"
    )
  }

  # Run all outlier detection methods
  results <- purrr::pmap_dfc(methods, function(method, args, abbr) {
    if (is.character(method)) method <- paste0("detect_outlr_", method)

    # Call the method
    results <- do.call(method, args = c(list("x" = x, "y" = y), args))

    # Validate the output
    assert_data_frame(results)
    if (!test_subset(c("lower", "upper", "replacement"), colnames(results))) {
      cli_abort(
        "Columns `lower`, `upper`, and `replacement` must be present in the output of the outlier detection method."
      )
    }

    # Update column names with model abbreviation
    colnames(results) <- paste(abbr, colnames(results), sep = "_")
    return(results)
  })

  # Combine information about detected outliers
  if (combiner != "none") {
    if (combiner == "mean") {
      combine_fun <- mean
    } else if (combiner == "median") {
      combine_fun <- median
    }

    for (target in c("lower", "upper", "replacement")) {
      results[[paste0("combined_", target)]] <- apply(
        results %>%
          dplyr::select(dplyr::ends_with(target)), 1, combine_fun
      )
    }
  }

  return(results)
}

#' Detect outliers based on a rolling median
#'
#' Detects outliers based on a distance from the rolling median specified in
#' terms of multiples of the rolling interquartile range (IQR).
#'
#' @template x-y
#' @param n Number of time steps to use in the rolling window. Default is 21.
#'   This value is centrally aligned. When `n` is an odd number, the rolling
#'   window extends from `(n-1)/2` time steps before each design point to `(n-1)/2`
#'   time steps after. When `n` is even, then the rolling range extends from
#'   `n/2-1` time steps before to `n/2` time steps after.
#' @template outlier-detection-options
#' @template detect-outlr-return
#'
#' @export
#' @examples
#' # Detect outliers based on a rolling median
#' incidence_num_outlier_example %>%
#'   dplyr::select(geo_value, time_value, cases) %>%
#'   as_epi_df() %>%
#'   group_by(geo_value) %>%
#'   mutate(outlier_info = detect_outlr_rm(
#'     x = time_value, y = cases
#'   )) %>%
#'   unnest(outlier_info)
detect_outlr_rm <- function(x = seq_along(y), y, n = 21,
                            log_transform = FALSE,
                            detect_negatives = FALSE,
                            detection_multiplier = 2,
                            min_radius = 0,
                            replacement_multiplier = 0) {
  # Transform if requested
  if (log_transform) {
    # Replace all negative values with 0
    y <- pmax(0, y)
    offset <- as.integer(any(y == 0))
    y <- log(y + offset)
  }

  # Detect negatives if requested
  if (detect_negatives && !log_transform) {
    min_lower <- 0
  } else {
    min_lower <- -Inf
  }

  # Make an epi_df for easy sliding
  z <- as_epi_df(tibble::tibble(geo_value = 0, time_value = x, y = y))

  # Calculate lower and upper thresholds and replacement value
  z <- z %>%
    epi_slide(fitted = median(y), before = floor((n - 1) / 2), after = ceiling((n - 1) / 2)) %>%
    dplyr::mutate(resid = y - fitted) %>%
    roll_iqr(
      n = n,
      detection_multiplier = detection_multiplier,
      min_radius = min_radius,
      replacement_multiplier = replacement_multiplier,
      min_lower = min_lower
    )

  # Undo log transformation if necessary
  if (log_transform) {
    z$lower <- exp(z$lower) - offset
    z$upper <- exp(z$upper) - offset
    z$replacement <- exp(z$replacement) - offset
  }

  return(z)
}

#' Detect outliers based on an STL decomposition
#'
#' Detects outliers based on a seasonal-trend decomposition using LOESS (STL).
#'
#' @template x-y
#' @param n_trend Number of time steps to use in the rolling window for trend.
#'   Default is 21.
#' @param n_seasonal Number of time steps to use in the rolling window for
#'   seasonality. Default is 21.
#' @param n_threshold Number of time steps to use in rolling window for the IQR
#'   outlier thresholds.
#' @param seasonal_period Integer specifying period of seasonality. For example,
#'   for daily data, a period 7 means weekly seasonality. The default is `NULL`,
#'   meaning that no seasonal term will be included in the STL decomposition.
#' @template outlier-detection-options
#' @template detect-outlr-return
#'
#' @details The STL decomposition is computed using the `feasts` package. Once
#'   computed, the outlier detection method is analogous to the rolling median
#'   method in `detect_outlr_rm()`, except with the fitted values and residuals
#'   from the STL decomposition taking the place of the rolling median and
#'   residuals to the rolling median, respectively.
#'
#' The last set of arguments, `log_transform` through `replacement_multiplier`,
#'   are exactly as in `detect_outlr_rm()`.
#'
#' @importFrom stats median
#' @importFrom tidyselect starts_with
#' @export
#' @examples
#' # Detects outliers based on a seasonal-trend decomposition using LOESS
#' incidence_num_outlier_example %>%
#'   dplyr::select(geo_value, time_value, cases) %>%
#'   as_epi_df() %>%
#'   group_by(geo_value) %>%
#'   mutate(outlier_info = detect_outlr_stl(
#'     x = time_value, y = cases,
#'     seasonal_period = 7
#'   )) %>% # weekly seasonality for daily data
#'   unnest(outlier_info)
detect_outlr_stl <- function(x = seq_along(y), y,
                             n_trend = 21,
                             n_seasonal = 21,
                             n_threshold = 21,
                             seasonal_period = NULL,
                             log_transform = FALSE,
                             detect_negatives = FALSE,
                             detection_multiplier = 2,
                             min_radius = 0,
                             replacement_multiplier = 0) {
  # Transform if requested
  if (log_transform) {
    # Replace all negative values with 0
    y <- pmax(0, y)
    offset <- as.integer(any(y == 0))
    y <- log(y + offset)
  }

  # Make a tsibble for fabletools, setup and run STL
  z_tsibble <- tsibble::tsibble(x = x, y = y, index = x)

  stl_formula <- y ~ trend(window = n_trend) +
    season(period = seasonal_period, window = n_seasonal)

  stl_components <- z_tsibble %>%
    fabletools::model(feasts::STL(stl_formula, robust = TRUE)) %>%
    generics::components() %>%
    tibble::as_tibble() %>%
    dplyr::select(.data$trend:.data$remainder) %>% #
    dplyr::rename_with(~"seasonal", tidyselect::starts_with("season")) %>%
    dplyr::rename(resid = .data$remainder)

  # Allocate the seasonal term from STL to either fitted or resid
  if (!is.null(seasonal_period)) {
    stl_components <- stl_components %>%
      dplyr::mutate(
        fitted = .data$trend + .data$seasonal
      )
  } else {
    stl_components <- stl_components %>%
      dplyr::mutate(
        fitted = .data$trend,
        resid = .data$seasonal + resid
      )
  }

  # Detect negatives if requested
  if (detect_negatives && !log_transform) {
    min_lower <- 0
  } else {
    min_lower <- -Inf
  }

  # Make an epi_df for easy sliding
  z <- as_epi_df(tibble::tibble(geo_value = 0, time_value = x, y = y))

  # Calculate lower and upper thresholds and replacement value
  z <- z %>%
    dplyr::mutate(
      fitted = stl_components$fitted,
      resid = stl_components$resid
    ) %>%
    roll_iqr(
      n = n_threshold,
      detection_multiplier = detection_multiplier,
      min_radius = min_radius,
      replacement_multiplier = replacement_multiplier,
      min_lower = min_lower
    )

  # Undo log transformation if necessary
  if (log_transform) {
    z$lower <- exp(z$lower) - offset
    z$upper <- exp(z$upper) - offset
    z$replacement <- exp(z$replacement) - offset
  }

  return(z)
}

# Common function for rolling IQR, using fitted and resid variables
roll_iqr <- function(z, n, detection_multiplier, min_radius,
                     replacement_multiplier, min_lower) {
  if (typeof(z$y) == "integer") {
    as_type <- as.integer # nolint: object_usage_linter
  } else {
    as_type <- as.numeric
  }

  epi_slide(z, roll_iqr = stats::IQR(resid), before = floor((n - 1) / 2), after = ceiling((n - 1) / 2)) %>%
    dplyr::mutate(
      lower = pmax(
        min_lower,
        fitted - pmax(min_radius, detection_multiplier * roll_iqr)
      ),
      upper = fitted + pmax(min_radius, detection_multiplier * roll_iqr),
      replacement = dplyr::case_when(
        (y < lower) ~ as_type(fitted - replacement_multiplier * roll_iqr),
        (y > upper) ~ as_type(fitted + replacement_multiplier * roll_iqr),
        TRUE ~ y
      )
    ) %>%
    dplyr::select(.data$lower, .data$upper, .data$replacement) %>%
    tibble::as_tibble()
}
