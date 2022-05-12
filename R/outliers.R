#' Detect outliers 
#'
#' Applies one or more outlier detection methods to a given signal variable, and
#' optionally aggregates the outputs to create a consensus result. See the
#' [outliers
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/outliers.html) for
#' examples.  
#' 
#' @param x Design points corresponding to the signal values `y`. Default is
#'   `seq_along(y)` (that is, equally-spaced points from 1 to the length of
#'   `y`).
#' @param y Signal values.
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
#' @return An tibble with number of rows equal to `length(y)` and columns giving
#'   the outlier detection thresholds and replacement values from each detection
#'   method. 
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
detect_outlr = function(x = seq_along(y), y,
                        methods = tibble::tibble(method = "rm",
                                         args = list(list()),
                                         abbr = "rm"),
                        combiner = c("median", "mean", "none")) {
  # Validate combiner
  combiner = match.arg(combiner)
  
  # Validate that x contains all distinct values
  if (any(duplicated(x))) {
    Abort("`x` cannot contain duplicate values. (If being run on a column in an `epi_df`, did you group by relevant key variables?)")
  }

  # Run all outlier detection methods
  results = purrr::pmap_dfc(methods, function(method, args, abbr) {
    if (is.character(method)) method = paste0("detect_outlr_", method)
    
    # Call the method
    results = do.call(method, args = c(list("x" = x, "y" = y), args))

   # Validate the output
    if (!is.data.frame(results) ||
        !all(c("lower", "upper", "replacement") %in% colnames(results))) {
      Abort("Outlier detection method must return a data frame with columns `lower`, `upper`, and `replacement`.")
    }

    # Update column names with model abbreviation
    colnames(results) = paste(abbr, colnames(results), sep = "_") 
    return(results)
  })
  
  # Combine information about detected outliers
  if (combiner != "none") {
    if (combiner == "mean") combine_fun = mean
    else if (combiner == "median") combine_fun = median
    
    for (target in c("lower", "upper", "replacement")) {
      results[[paste0("combined_", target)]] = apply(
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
#' @param x Design points corresponding to the signal values `y`. Default is
#'   `seq_along(y)` (that is, equally-spaced points from 1 to the length of
#'   `y`).
#' @param y Signal values.
#' @param n Number of time steps to use in the rolling window. Default is 21. 
#' @param log_transform Should a log transform be applied before running outlier
#'   detection? Default is `FALSE`. If `TRUE`, and zeros are present, then the
#'   log transform will be padded by 1.
#' @param detect_negatives Should negative values automatically count as
#'   outliers? Default is `FALSE`.
#' @param detection_multiplier Value determining how far the outlier detection
#'   thresholds are from the rolling median, which are calculated as (rolling 
#'   median) +/- (detection multiplier) * (rolling IQR). Default is 2. 
#' @param min_radius Minimum distance between rolling median and threshold, on
#'   transformed scale. Default is 0.
#' @param replacement_multiplier Value determining how far the replacement
#'   values are from the rolling median. The replacement is the original value
#'   if it is within the detection thresholds, or otherwise it is rounded to the
#'   nearest (rolling median) +/- (replacement multiplier) * (rolling IQR).
#'   Default is 0.
#' @return A tibble with number of rows equal to `length(y)`, and columns
#'   `lower`, `upper`, and `replacement`.
#'
#' @export
detect_outlr_rm = function(x = seq_along(y), y, n = 21,
                           log_transform = FALSE,
                           detect_negatives = FALSE,
                           detection_multiplier = 2,
                           min_radius = 0,
                           replacement_multiplier = 0) {
  # Transform if requested
  if (log_transform) {
    # Replace all negative values with 0
    y = pmax(0, y)
    offset = as.integer(any(y == 0))
    y = log(y + offset)
  }

  # Detect negatives if requested
  if (detect_negatives && !log_transform) min_lower = 0
  else min_lower = -Inf

  # Make an epi_df for easy sliding
  z = as_epi_df(tibble::tibble(geo_value = 0, time_value = x, y = y))
  
  # Calculate lower and upper thresholds and replacement value
  z = z %>%
    epi_slide(fitted = median(y), n = n, align = "center") %>%
    dplyr::mutate(resid = y - fitted) %>%
    roll_iqr(n = n,
             detection_multiplier = detection_multiplier,
             min_radius = min_radius,
             replacement_multiplier = replacement_multiplier,
             min_lower = min_lower)

  # Undo log transformation if necessary
  if (log_transform) {
    z$lower = exp(z$lower) - offset
    z$upper = exp(z$upper) - offset
    z$replacement = exp(z$replacement) - offset
  }

  return(z)
}

#' Detect outliers based on an STL decomposition 
#'
#' Detects outliers based on a seasonal-trend decomposition using LOESS (STL).  
#'
#' @param x Design points corresponding to the signal values `y`. Default is
#'   `seq_along(y)` (that is, equally-spaced points from 1 to the length of
#'   `y`).
#' @param y Signal values.
#' @param n_trend Number of time steps to use in the rolling window for trend.
#'   Default is 21.
#' @param n_seasonal Number of time steps to use in the rolling window for
#'   seasonality. Default is 21. 
#' @param n_threshold Number of time steps to use in rolling window for the IQR
#'   outlier thresholds.
#' @param seasonal_period Integer specifying period of seasonality. For example,
#'   for daily data, a period 7 means weekly seasonality. The default is `NULL`,
#'   meaning that no seasonal term will be included in the STL decomposition.
#' @param log_transform Should a log transform be applied before running outlier
#'   detection? Default is `FALSE`. If `TRUE`, and zeros are present, then the
#'   log transform will be padded by 1.
#' @param detect_negatives Should negative values automatically count as
#'   outliers? Default is `FALSE`.
#' @param detection_multiplier Value determining how far the outlier detection
#'   thresholds are from the rolling median, which are calculated as (rolling 
#'   median) +/- (detection multiplier) * (rolling IQR). Default is 2. 
#' @param min_radius Minimum distance between rolling median and threshold, on
#'   transformed scale. Default is 0.
#' @param replacement_multiplier Value determining how far the replacement
#'   values are from the rolling median. The replacement is the original value
#'   if it is within the detection thresholds, or otherwise it is rounded to the
#'   nearest (rolling median) +/- (replacement multiplier) * (rolling IQR).
#'   Default is 0.
#' @return A tibble with number of rows equal to `length(y)`, and columns
#'   `lower`, `upper`, and `replacement`. 
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
detect_outlr_stl = function(x = seq_along(y), y,
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
    y = pmax(0, y)
    offset = as.integer(any(y == 0))
    y = log(y + offset)
  }
  
  # Make a tsibble for fabletools, setup and run STL
  z_tsibble = tsibble::tsibble(x = x, y = y, index = x)
  
  stl_formula = y ~ trend(window = n_trend) +
    season(period = seasonal_period, window = n_seasonal)

  stl_components = z_tsibble %>%
    fabletools::model(feasts::STL(stl_formula, robust = TRUE)) %>%
    generics::components() %>%
    tibble::as_tibble() %>%
    dplyr::select(trend:remainder) %>%
    dplyr::rename_with(~ "seasonal", tidyselect::starts_with("season")) %>%
    dplyr::rename(resid = remainder)
    
  # Allocate the seasonal term from STL to either fitted or resid 
  if (!is.null(seasonal_period)) {
    stl_components = stl_components %>%
      dplyr::mutate(
        fitted = trend + seasonal)
  } else {
    stl_components = stl_components %>%
      dplyr::mutate(
        fitted = trend,
        resid = seasonal + resid)
  }

  # Detect negatives if requested
  if (detect_negatives && !log_transform) min_lower = 0
  else min_lower = -Inf
  
  # Make an epi_df for easy sliding
  z = as_epi_df(tibble::tibble(geo_value = 0, time_value = x, y = y))
  
  # Calculate lower and upper thresholds and replacement value  
  z = z %>%
    dplyr::mutate(
      fitted = stl_components$fitted,
      resid = stl_components$resid) %>%
    roll_iqr(n = n_threshold,
             detection_multiplier = detection_multiplier, 
             min_radius = min_radius,
             replacement_multiplier = replacement_multiplier,
             min_lower = min_lower)

  # Undo log transformation if necessary
  if (log_transform) {
    z$lower = exp(z$lower) - offset
    z$upper = exp(z$upper) - offset
    z$replacement = exp(z$replacement) - offset
  }

  return(z)
}

# Common function for rolling IQR, using fitted and resid variables
roll_iqr = function(z, n, detection_multiplier, min_radius,
                    replacement_multiplier, min_lower) {
  if (typeof(z$y) == "integer") as_type = as.integer
  else as_type = as.numeric
  
  epi_slide(z, roll_iqr = stats::IQR(resid), n = n, align = "center") %>%
    dplyr::mutate(
      lower = pmax(min_lower,
                   fitted - pmax(min_radius, detection_multiplier * roll_iqr)),
      upper = fitted + pmax(min_radius, detection_multiplier * roll_iqr),
      replacement = dplyr::case_when(
      (y < lower) ~ as_type(fitted - replacement_multiplier * roll_iqr),
      (y > upper) ~ as_type(fitted + replacement_multiplier * roll_iqr),
      TRUE ~ y)) %>%
    dplyr::select(lower, upper, replacement) %>%
    tibble::as_tibble()
}
