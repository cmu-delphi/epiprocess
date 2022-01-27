#' Detect outliers in a variable in an `epi_df` object
#'
#' Applies one or more outlier detection methods to a variable in an `epi_df`
#' object, and optionally aggregates the results to create consensus results.
#' See the [outliers
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/outliers.html) for
#' examples.
#'
#' @details Each outlier detection method, one per row of the passed `methods`
#'   tibble, is a function that must take as its first two arguments `x` and
#'   `var`, indicating a data frame and a given variable on which to run outlier
#'   detection, and then any number of additional arguments. The function must
#'   return a tibble with the same number of rows as `x`, and with columns
#'   `lower`, `upper`, and `replacement`, representing lower and upper bounds
#'   for what would be considered an outlier, and a posited replacement value,
#'   respectively.
#'
#'  For convenience, the outlier detection method can be specified (in the
#'   `method` column of `methods`) by a string "rm", shorthand for
#'   `epi_detect_outlr_rm()`, which detects outliers via a rolling median; or by
#'   "stl", shorthand for `epi_detect_outlr_stl()`, which detects outliers via
#'   an STL decomposition.
#' 
#' @param x The `epi_df` object under consideration.
#' @param var The variable in `x` on which to run outlier detection.
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
#' @param new_col_name String indicating the name of the new column that will
#'   contain the results of outlier detection. Default is "outlier_info"; note
#'   that setting `new_col_name` equal to an existing column name will overwrite
#'   this column.
#'
#' @return An `epi_df` object given by appending a new column to `x`, named
#'   according to the `new_col_name` argument, containing the outlier detection
#'   thresholds and replacement values from all detection methods.
#'
#' @importFrom dplyr group_modify mutate select 
#' @importFrom purrr map pmap_dfc
#' @importFrom tidyselect ends_with all_of
#' @importFrom rlang !! abort enquo
#' @export
epi_detect_outlr = function(x, var,
                            methods = tibble(
                              method = "rm", args = list(list()), abbr = "rm"),
                            combiner = c("median", "mean", "none"),
                            new_col_name = "outlier_info") {
  # Check we have an `epi_df` object
  if (!inherits(x, "epi_df")) abort("`x` must be of class `epi_df`.")
  
  # Check that we have a variable to do computations on
  if (missing(var)) abort("`var` must be specified.")
  var = enquo(var)

  # Validate combiner
  combiner = match.arg(combiner)

  # Save the metadata (dplyr drops it)
  metadata = attributes(x)$metadata

  # Outlier detection per group (in case x is grouped) 
  x = x %>%
    group_modify(epi_detect_outlr_one_grp,
                 var = var,
                 methods = methods,
                 combiner = combiner,
                 new_col_name = new_col_name)

  # Attach the class and metadata and return
  class(x) = c("epi_df", class(x))
  attributes(x)$metadata = metadata
  return(x)
}

# Outlier detection over a single group
epi_detect_outlr_one_grp = function(.data_group, var, methods, combiner,
                                    new_col_name, ...) {
  # Run all outlier detection methods
  results = pmap_dfc(
    methods,
    function(method, args, abbr) {
      if (is.character(method)) {
        method = paste0("epi_detect_outlr_", method)
      }
      
      # Call the method
      results = do.call(method,
                        args = c(list("x" = .data_group, "var" = var), args))

      # Validate the output
      if (!is.data.frame(results) ||
          !all(c("lower", "upper", "replacement") %in%
               colnames(results))) {
        abort(paste("Outlier detection method must return a data frame with",
                    "columns `lower`, `upper`, and `replacement`."))
      }

      # Update column names with model abbreviation
      colnames(results) = paste(abbr, colnames(results), sep = "_")
      return(results)
    }
  )

  # Combine information about detected outliers
  if (combiner != "none") {
    if (combiner == "mean") {
      combine_fun = base::mean
    } else if (combiner == "median") {
      combine_fun = stats::median
    }

    for (target in c("lower", "upper", "replacement")) {
      results[[paste0("combined_", target)]] = apply(
        results %>%
        select(ends_with(target)),
        1,
        combine_fun
      )
    }
  }
  
  # Assemble outlier information as columns, return
  new_col_values = map(
    seq_len(nrow(results)),
    function(i) return(results[i, ])
  )

  return(.data_group %>% mutate(!!new_col_name := new_col_values))
}

#' Detect outliers based on a rolling median
#'
#' Detects outliers based on a distance from the rolling median specified in
#' terms of multiples of the rolling interquartile range (IQR).
#'
#' @param x The `epi_df` object under consideration.
#' @param var The variable in `x` on which to run outlier detection.
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
#'
#' @return A tibble with the same number of rows as `x`, and columns `lower`,
#'   `upper`, and `replacement`. 
#'
#' @importFrom dplyr mutate pull select
#' @importFrom rlang !!
#' @export
epi_detect_outlr_rm = function(x, var, n = 21,
                               log_transform = FALSE,
                               detect_negatives = FALSE,
                               detection_multiplier = 2,
                               min_radius = 0,
                               replacement_multiplier = 0) {
  # Transform if requested
  if (log_transform) {
    # Replace all negative values with 0
    x = x %>% mutate(!!var := pmax(0, !!var))

    # Offset is 1 if any zero values in var, 0 otherwise
    offset = as.integer(any(x %>% pull(!!var) == 0))

    # Transform var
    x = x %>% mutate(!!var := log(!!var + offset))
  }

  # Detect negatives if requested
  if (detect_negatives && !log_transform) {
    min_lower = 0
  } else {
    min_lower = -Inf
  }

  # Calculate lower and upper thresholds and replacement value
  x = x %>%
    epi_slide(fitted = median(!!var), n = n, align = "center") %>%
    mutate(resid = !!var - fitted) %>%
    roll_iqr(var, n,
             detection_multiplier = detection_multiplier,
             min_radius = min_radius,
             replacement_multiplier = replacement_multiplier,
             min_lower = min_lower)

  # Undo log transformation if necessary
  if (log_transform) {
    x$lower = exp(x$lower) - offset
    x$upper = exp(x$upper) - offset
    x$replacement = exp(x$replacement) - offset
  }

  # Return
  return(x)
}

#' Detect outliers based on an STL decomposition 
#'
#' Detects outliers based on a seasonal-trend decomposition using LOESS (STL).  
#'
#' @details The STL decomposition is computed using the `feasts` package. Once
#'   computed, the outlier detection method is analogous to the rolling median
#'   method in `epi_detect_outlr_rm()`, except with the fitted values and
#'   residuals from the STL decomposition taking the place of the rolling median
#'   and residuals to the rolling median, respectively.
#'
#' The last set of arguments, `log_transform` through `replacement_multiplier`,
#'   are exactly as in `epi_detect_outlr_rm()`; refer to its help file for their
#'   description.
#'
#' @param x The `epi_df` object under consideration.
#' @param var The variable in `x` on which to run outlier detection.
#' @param n_trend Number of time steps to use in the rolling window for trend.
#'   Default is 21.
#' @param n_seasonal Number of time steps to use in the rolling window for
#'   seasonality. Default is 21. 
#' @param n_threshold Number of time steps to use in rolling window for the IQR
#'   outlier thresholds.
#' @param seasonal_period Integer specifying period of seasonality. For example,
#'   for daily data, a period 7 means weekly seasonality. The default is `NULL`,
#'   meaning that no seasonal term will be included in the STL decomposition.
#' 
#' @return A tibble with the same number of rows as `x`, and columns `lower`,
#'   `upper`, and `replacement`. 
#'
#' @importFrom dplyr case_when mutate pull select transmute
#' @importFrom fabletools model
#' @importFrom feasts STL
#' @importFrom rlang !!
#' @export
epi_detect_outlr_stl = function(x, var,
                               n_trend = 21,
                               n_seasonal = 21,
                               n_threshold = 21,
                               seasonal_period = NULL,
                               log_transform = FALSE,
                               detect_negatives = FALSE,
                               detection_multiplier = 2,
                               min_radius = 0,
                               replacement_multiplier = 0) {
  # Make x into a tsibble for use with fable
  x_tsibble = x %>%
    select(time_value, y = !!var) %>%
    tsibble::as_tsibble(index = time_value)

 # Transform if requested
  if (log_transform) {
    # Replace all negative values with 0
    x_tsibble = x_tsibble %>% mutate(y := pmax(0, y))

    # Offset is 1 if any zero values in var, 0 otherwise
    offset = as.integer(any(x_tsibble %>% pull(y) == 0))

    # Transform var
    x_tsibble = x_tsibble %>% mutate(y := log(y + offset))
  }
  
  stl_formula = y ~ trend(window = n_trend) +
    season(period = seasonal_period, window = n_seasonal)

  stl_components = x_tsibble %>%
    model(STL(stl_formula, robust = TRUE)) %>%
    generics::components() %>%
    tibble::as_tibble() %>%
    transmute(
      trend = trend,
      seasonal = season_week,
      resid = remainder
    )
  
  # Allocate the seasonal term from STL to either fitted or resid 
  if (!is.null(seasonal_period)) {
    stl_components = stl_components %>%
      mutate(
        fitted = trend + seasonal
      )
  } else {
    stl_components = stl_components %>%
      mutate(
        fitted = trend,
        resid = seasonal + resid
      )
  }

  # Detect negatives if requested
  if (detect_negatives && !log_transform) {
    min_lower = 0
  } else {
    min_lower = -Inf
  }
  
  # Calculate lower and upper thresholds and replacement value  
  x = x %>%
    mutate(
      fitted = stl_components$fitted,
      resid = stl_components$resid) %>%
    roll_iqr(var, n_threshold,
             detection_multiplier = detection_multiplier,
             min_radius = min_radius,
             replacement_multiplier = replacement_multiplier,
             min_lower = min_lower)

  # Undo log transformation if necessary
  if (log_transform) {
    x$lower = exp(x$lower) - offset
    x$upper = exp(x$upper) - offset
    x$replacement = exp(x$replacement) - offset
  }

  # Return
  return(x)
}

# Common function for rolling IQR, using fitted and resid variables
roll_iqr = function(x, var, n, detection_multiplier, min_radius,
                    replacement_multiplier, min_lower) { 
  epi_slide(x, roll_iqr = IQR(resid), n = n, align = "center") %>%
    mutate(
      lower = pmax(min_lower,
                   fitted - pmax(min_radius, detection_multiplier * roll_iqr)),
      upper = fitted + pmax(min_radius, detection_multiplier * roll_iqr),
      replacement = dplyr::case_when(
      (!!var < lower) ~ fitted - replacement_multiplier * roll_iqr,
      (!!var > upper) ~ fitted + replacement_multiplier * roll_iqr,
      TRUE ~ !!var
      )) %>%
    select(lower, upper, replacement) %>%
    unclass() %>%
    tibble::as_tibble()
}
