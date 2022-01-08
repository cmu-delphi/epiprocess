#' Detect outliers in a variable in an `epi_tibble` object
#'
#' Applies one or more outlier detection methods to a variable in an
#' `epi_tibble` object, and optionally aggregates the results to create
#' consensus results.
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
#'   `detect_outliers_rm()`, which detects outliers via a rolling median; or by
#'   "stl", shorthand for `detect_outliers_stl()`, which detects outliers via an
#'   STL decomposition.
#' 
#' @param x The `epi_tibble` object under consideration.
#' @param var The variable in `x` on which to run outlier detection.
#' @param methods A tibble specifying the method(s) to use for outlier
#'   detection, with one row per method, and the following columns: 
#'   * `method` <str>: Either "rm" or "stl", or a custom function for outlier
#'   detection; see details for further explanation.   
#'   * `method_args` <lst>: Named list of arguments that will be passed to the
#'   detection method.
#'   * `method_abbr` <chr>: Abbreviation to use in naming output columns with
#'   results from this method.
#' @param combiner String, one of "median", "mean", or "none", specifying how to
#'   combine results from different outlier detection methods for the thresholds
#'   determining whether a particular observation is classified as an outlier,
#'   as well as a replacement value for any outliers.  If "none", then no
#'   summarized results are calculated. Note that if the number of `methods`
#'   (number of rows) is odd, then "median" is equivalent to a majority vote for
#'   purposes of determining whether a given observation is an outlier.
#' @param new_col_name String indicating the name of the new column that will
#'   contain the results of outlier detection. Default is "outliers"; note that
#'   setting `new_col_name` equal to an existing column name will overwrite this
#'   column.
#'
#' @return An `epi_tibble` object given by appending a new column to `x`, named 
#'   according to the `new_col_name` argument, containing the outlier detection
#'   thresholds and replacement values from all detection methods. 
#'
#' @importFrom dplyr group_modify mutate select 
#' @importFrom purrr map pmap_dfc
#' @importFrom tidyselect ends_with all_of
#' @importFrom rlang abort enquo
#' @export
detect_outliers = function(x, var,
                           methods = tibble(
                             method = "rm",
                             method_args = list(list()),
                             method_abbr = "rm"
                           ),
                           combiner = c("median", "mean", "none"),
                           new_col_name = "outliers") {
  # Check we have an `epi_tibble` object
  if (!inherits(x, "epi_tibble")) abort("`x` must be of class `epi_tibble`.")
  
  # Check that we have a variable to do computations on
  if (missing(var)) abort("`var` must be specified.")
  var = enquo(var)

  # Validate combiner
  combiner = match.arg(combiner)

  # Save the metadata (dplyr drops it)
  metadata = attributes(x)$metadata

  # Do outlier detection for each group in x
  x = x %>%
    group_modify(detect_outliers_one_grp,
                 var = var,
                 methods = methods,
                 combiner = combiner,
                 new_col_name = new_col_name)

  # Attach the class and metadata and return
  class(x) = c("epi_tibble", class(x))
  attributes(x)$metadata = metadata
  return(x)
}

detect_outliers_one_grp = function(.data_group,
                                   var,
                                   methods,
                                   combiner,
                                   new_col_name,
                                   ...) {
  # Run all outlier detection methods
  results = pmap_dfc(
    methods,
    function(method, method_args, method_abbr) {
      if (is.character(method)) {
        method = paste0("detect_outliers_", method)
      }

      # Call the method
      method_results = do.call(method,
                               args = c(list("x" = .data_group,
                                             "var" = var),
                                        method_args))

      # Validate the output
      if (!is.data.frame(method_results) ||
          !all(c("lower", "upper", "replacement") %in%
               colnames(method_results))) {
        abort(paste("Outlier detection method must return a data frame with",
                    "columns `lower`, `upper`, and `replacement`."))
      }

      # Update column names with model abbreviation
      colnames(method_results) = paste(method_abbr,
                                       colnames(method_results),
                                       sep = "_")

      return(method_results)
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

  return(.data_group %>%
         mutate(!!new_col_name := new_col_values))
}

#' Detect outliers based on a rolling median
#'
#' Detects outliers based on a distance from the rolling median specified in
#' terms of multiples of the rolling interquartile range (IQR).
#'
#' @param x The `epi_tibble` object under consideration.
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
#' @export
detect_outliers_rm = function(x, var,
                              n = 21,
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
  outliers <- x %>%
    epi_slide(slide_fun = function(x, var, ...) x %>% pull(!!var) %>% median(),
              n = n, align = "centered",
              new_col_name = "fitted",
              var = var) %>%
    mutate(resid = !!var - fitted) %>%
    roll_iqr(n,
             detection_multiplier = detection_multiplier,
             min_radius = min_radius,
             replacement_multipler = replacement_multipler,
             min_lower = min_lower)

  # Undo log transformation if necessary
  if (log_transform) {
    outliers$lower <- exp(outliers$lower) - offset
    outliers$upper <- exp(outliers$upper) - offset
    outliers$replacement <- exp(outliers$replacement) - offset
  }

  # Return
  return(outliers)
}

# So that nobody's code breaks if they were using method = "rolling_median"
detect_outliers_rolling_median = detect_outliers_rm

#' Detect outliers based on an STL decomposition 
#'
#' Detects outliers based on a seasonality and trend decomposition using loess
#' (STL). 
#'
#' @details The STL decomposition is computed using the `feasts` package. Once
#'   computed, the outlier detection method is analogous to the rolling median
#'   method in `detect_outliers_rm()`, except with the fitted values and
#'   residuals from the STL decomposition taking the place of the rolling median
#'   and residuals to the rolling median, respectively.
#'
#' The last set of arguments, `log_transform` through `replacement_multiplier`,
#'   are exactly as in `detect_outliers_rm()`; refer to its help file for their
#'   description.
#'
#' @param x The `epi_tibble` object under consideration.
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
#' @export
detect_outliers_stl = function(x, var,
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
  x_tsibble <- x %>%
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
    fabletools::model(feasts::STL(stl_formula, robust = TRUE)) %>%
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
  outliers <- x %>%
    mutate(
      fitted = stl_components$fitted,
      resid = stl_components$resid) %>%
    roll_iqr(n_threshold,
             detection_multiplier = detection_multiplier,
             min_radius = min_radius,
             replacement_multipler = replacement_multipler,
             min_lower = min_lower)
 
  # Undo log transformation if necessary
  if (log_transform) {
    outliers$lower <- exp(outliers$lower) - offset
    outliers$upper <- exp(outliers$upper) - offset
    outliers$replacement <- exp(outliers$replacement) - offset
  }

  # Return
  return(outliers)
}

# Common function for rolling IQR, using fitted and resid variables

roll_iqr = function(x, var, n, detection_multiplier, min_radius,
                    replacement_multipler, min_lower) {
  epi_slide(slide_fun = function(x, var, ...) x %>% pull(resid) %>% IQR(),
            n = n, align = "centered", new_col_name = "roll_iqr") %>%
    mutate(
      lower = pmax(min_lower,
                   fitted - pmax(min_radius, detection_multiplier * roll_iqr)),
      upper = fitted + pmax(min_radius, detection_multiplier * roll_iqr),
      replacement = dplyr::case_when(
      (!!var < lower) ~ fitted - replacement_multiplier * roll_iqr,
      (!!var > upper) ~ fitted + replacement_multiplier * roll_iqr,
      TRUE ~ !!var
      )
    ) %>%
    # Keep just the columns we want
    select(lower, upper, replacement) %>%
    # Drop any extra classes; just keep tibble
    unclass() %>%
    tibble::as_tibble()
}

#' Plot outlier detection bands and/or points identified as outliers
#' 
#' @param x An `epi_tibble` object including results for outlier detection as
#'   returned by `detect_outliers`
#' @param var The variable in `x` in which to look for outliers.
#' @param outliers_col_name The variable in `x` with outlier detection results.
#'   This is the value used as the `new_col_name` argument to `detect_outliers`.
#' @param include_bands Boolean; if `TRUE`, outlier detection thresholds are
#'   shown
#' @param include_points Boolean; if `TRUE`, outliers are highlighted with
#'   points
#' @param combined_only Boolean; if `TRUE`, only the combined results across
#'   all detection methods are displayed
#' @param facet_vars character vector of variables to facet by.
#'   Default is `"geo_type"`. If `NULL`, no facetting is done.
#' @param facet_nrow, facet_ncol Integer number of rows and columns for
#'   facetting.
#' @param plot Boolean; if `TRUE`, the plot is printed.
#'
#' @return invisibly, a `ggplot` object
#'
#' @importFrom dplyr filter vars pull
#' @importFrom tidyr unnest pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_ribbon facet_wrap
#' @importFrom rlang enquo abort
#'
#' @export
plot_outliers = function(x, var,
                         outliers_col = outliers,
                         include_bands = FALSE,
                         include_points = TRUE,
                         combined_only = TRUE,
                         facet_vars = vars(geo_value),
                         facet_nrow = NULL,
                         facet_ncol = NULL,
                         facet_scales = "fixed",
                         plot = TRUE) {
  # Check that we have a variable to do computations on
  if (missing(var)) abort("`var` must be specified.")
  var = enquo(var)
  outliers_col = enquo(outliers_col)

  # convert outlier detection results to long format for easy ggplot-ing
  outliers_long = x %>%
    unnest(!!outliers_col) %>%
    pivot_longer(
      cols = x %>% pull(!!outliers_col) %>% `[[`(1) %>% colnames(),
      names_to = c("detection_method", ".value"),
      names_pattern = "(.+)_(.+)"
    )

  # if requested, filter to only combined method
  if (combined_only) {
    outliers_long = outliers_long %>%
      filter(detection_method == "combined")
  }

  # start of plot with observed data
  p <- ggplot() +
    geom_line(mapping = aes(x = time_value, y = !!var), data = x)

  # if requested, add bands
  if (include_bands) {
    p <- p +
      geom_ribbon(
        data = outliers_long,
        mapping = aes(x = time_value,
                      ymin = lower, ymax = upper,
                      color = detection_method),
        fill = NA
      )
  }

  # if requested, add points
  if (include_points) {
    outliers_detected <- outliers_long %>%
      filter((!!var < lower) | (!!var > upper))
    p <- p +
      geom_point(
        data = outliers_detected,
        mapping = aes(x = time_value,
                      y = !!var,
                      color = detection_method,
                      shape = detection_method)
      )
  }

  # if requested, add facetting
  if (!is.null(facet_vars)) {
    p <- p +
      facet_wrap(facet_vars,
                 nrow = facet_nrow, ncol = facet_ncol,
                 scales = facet_scales)
  }

  # if requested, print the plot
  if (plot) {
    print(p)
  }

  # return the plot, invisibly
  return(invisible(p))
}
