#' Detect outliers in variables in an `epi_tibble` object
#'
#' Applies one or more outlier detection algorithms to variables in an
#' `epi_tibble`, and optionally aggregates the results to create consensus
#' results. 
#'
#' @param x The `epi_tibble` object under consideration.
#' @param var The variable in `x` on which to run outlier detection.
#' @param methods A tibble specifying methods to use for outlier
#'   detection. Contains the following columns:
#' * `method` <str>: The name of the detection method or a function for
#'   outlier detection.
#' * `method_args` <lst>: A named list of arguments that will be passed to the
#'   detection method.
#' * `method_abbr` <chr>: An abbreviation to use in naming output columns with
#'   results from this method.
#' Each outlier detection method must return a data frame with columns `lower`,
#'   `upper`, and `replacement`.
#' @param combine_method String, either "median", "mean", or "none", specifying
#'   how to combine results from different outlier detection methods for the
#'   thresholds determining whether a particular observation is classified as an
#'   outlier, as well as a replacement value for any outliers.  If "none", no
#'   summarized results are calculated. Note that if the number of `methods`
#'   (number of rows) is odd, then "median" is equivalent to a majority vote for
#'   purposes of determining whether a given observation is an outlier.
#' @param new_col_name String indicating the name of the new column that will
#'   contain the results of outlier detection. Default is "outliers"; note that
#'   setting `new_col_name` equal to an existing column name will overwrite this
#'   column.
#'
#' @return The input `epi_tibble` `x` augmented with outlier detection
#'   thresholds and replacement values from all detection methods.
#'
#' @importFrom dplyr group_modify mutate select 
#' @importFrom purrr map pmap_dfc
#' @importFrom tidyselect ends_with all_of
#' @importFrom tibble as_tibble
#' @importFrom rlang abort enquo
#' @export
detect_outliers = function(x, var,
                           methods = tibble(
                             method = "rolling_median",
                             method_args = list(list()),
                             method_abbr = "rolling_median"
                           ),
                           combine_method = c("median", "mean", "none"),
                           new_col_name = "outliers") {
  # Check that we have a variable to do computations on
  if (missing(var)) abort("`var` must be specified.")
  var = enquo(var)

  # Validate combine_method
  combine_method = match.arg(combine_method)

  # Save the metadata (dplyr drops it)
  metadata = attributes(x)$metadata

  # Do outlier detection for each group in x
  x = x %>%
    group_modify(detect_outliers_one_grp,
                 var = var,
                 methods = methods,
                 combine_method = combine_method,
                 new_col_name = new_col_name)

  # Attach the class and metadata and return
  class(x) = c("epi_tibble", class(x))
  attributes(x)$metadata = metadata
  return(x)
}

detect_outliers_one_grp = function(.data_group,
                                   var,
                                   methods,
                                   combine_method,
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
                               args = c(list("x" = .data_group, "var" = var),
                                        method_args))

      # Validate the output
      if (!is.data.frame(method_results) ||
          !all(c("lower", "upper", "replacement") %in%
               colnames(method_results))) {
        abort(paste("Outlier detection methods must return a data frame with",
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
  if (combine_method != "none") {
    if (combine_method == "mean") {
      combine_fun = base::mean
    } else if (combine_method == "median") {
      combine_fun = stats::median
    }

    for (target in c("lower", "upper", "replacement")) {
      results[[paste0("combined_", target)]] = apply(
        results %>% select(ends_with(target)),
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

#' Detect outliers based on a distance from the rolling median specified in
#' terms of multiples of the rolling IQR.
#'
#' @param x The `epi_tibble` object under consideration.
#' @param var The variable in `x` in which to look for outliers.
#' @param n Number of time steps to use in the window. Default is 15.
#' @param transform string specifying transformation to use: "none" or "log"
#' @param detection_multiplier specification of how far the outlier detection
#'   threshold is from the rolling median. The thresholds are calculated as
#'   rolling median +/- detection_multiplier * rolling IQR
#' @param min_radius minimum distance between rolling median and threshold,
#'   on transformed scale
#' @param detect_negatives boolean; if TRUE, negative values count as outliers
#' @param replacement_multiplier specification of how far the replacement value
#'   is from the rolling median. The replacement is the original value if it is
#'   within the detection thresholds, or otherwise
#'   rolling median +/- replacement_multiplier * rolling IQR
#'
#' @return a tibble with columns `lower`, `upper`, and `replacement`
#'
#' @importFrom dplyr mutate pull case_when select
#' @importFrom tibble as_tibble
#'
#' @export
detect_outliers_rolling_median = function(x, var,
                                          n = 21,
                                          transform = c("none", "log"),
                                          detection_multiplier = 2,
                                          min_radius = 0,
                                          detect_negatives = FALSE,
                                          replacement_multiplier = 0) {
  # transform if requested
  transform = match.arg(transform)
  if (transform == "log") {
    # replace all negative values with 0
    x = x %>% mutate(!!var := pmax(0, !!var))

    # offset is 1 if any zero values in var, 0 otherwise
    offset = as.integer(any(x %>% pull(!!var) == 0))

    # transform var
    x = x %>% mutate(!!var := log(!!var + offset))
  }

  # calculate lower and upper thresholds and replacement value
  if (detect_negatives && transform == "none") {
    min_lower = 0
  } else {
    min_lower = -Inf
  }
  outliers <- x %>%
    epi_slide(slide_fun = function(x, var, ...) x %>% pull(!!var) %>% median(),
              n = n, align = "centered",
              new_col_name = "roll_median",
              var = var) %>%
    mutate(resid = !!var - roll_median) %>%
    epi_slide(slide_fun = function(x, var, ...) x %>% pull(resid) %>% IQR(),
              n = n, align = "centered",
              new_col_name = "roll_iqr") %>%
    mutate(
      lower = pmax(min_lower,
                   roll_median - pmax(min_radius, detection_multiplier * roll_iqr)),
      upper = roll_median + pmax(min_radius, detection_multiplier * roll_iqr),
      replacement = case_when(
        (!!var < lower) ~ roll_median - replacement_multiplier * roll_iqr,
        (!!var > upper) ~ roll_median + replacement_multiplier * roll_iqr,
        TRUE ~ !!var
      )
    ) %>%
    # keep just the columns we want
    select(lower, upper, replacement) %>%
    # drop any extra classes; just keep tibble
    unclass() %>%
    as_tibble()

  # undo log transformation if necessary
  if (transform == "log") {
    outliers$lower <- exp(outliers$lower) - offset
    outliers$upper <- exp(outliers$upper) - offset
    outliers$replacement <- exp(outliers$replacement) - offset
  }

  # return
  return(outliers)
}



#' Detect outliers based on residuals from an STL decomposition of a signal.
#'
#' @param x The `epi_tibble` object under consideration.
#' @param var The variable in `x` in which to look for outliers.
#' @param n_trend Number of time steps to use in the window for trend.
#'   Default is 21.
#' @param n_seasonal Number of time steps to use in the window for seasonality.
#'   Default is 21.
#' @param n_threshold Number of time steps to use in window for IQR used to
#'   set outlier thresholds.
#' @param seasonal_period integer period of seasonality, e.g. 7 for weekly
#'   seasonality with daily data. The default is `NULL`, meaning that no
#'   seasonal term is included in the STL decomposition.
#' @param transform string specifying transformation to use: "none" or "log"
#' @param detection_multiplier specification of how far the outlier detection
#'   threshold is from the rolling median. The thresholds are calculated as
#'   rolling median +/- detection_multiplier * rolling IQR
#' @param min_radius minimum distance between rolling median and threshold,
#'   on transformed scale
#' @param detect_negatives boolean; if TRUE, negative values count as outliers
#' @param replacement_multiplier specification of how far the replacement value
#'   is from the rolling median. The replacement is the original value if it is
#'   within the detection thresholds, or otherwise
#'   rolling median +/- replacement_multiplier * rolling IQR
#'
#' @return a tibble with columns `lower`, `upper`, and `replacement`
#'
#' @importFrom dplyr transmute
#' @importFrom tsibble as_tsibble
#' @importFrom fabletools model
#' @importFrom feasts STL
#' @importFrom tibble as_tibble
#'
#' @export
detect_outliers_stl = function(x, var,
                               n_trend = 21, n_seasonal = 21, n_threshold = 21,
                               seasonal_period = NULL,
                               include_seasonality = TRUE,
                               transform = c("none", "log"),
                               detection_multiplier = 2,
                               min_radius = 0,
                               detect_negatives = FALSE,
                               replacement_multiplier = 0) {
  # make x into a tsibble for use with fable
  x_tsibble <- x %>%
    select(time_value, y = !!var) %>%
    as_tsibble(index = time_value)

  # transform if requested
  transform = match.arg(transform)
  if (transform == "log") {
    # replace all negative values with 0
    x_tsibble = x_tsibble %>% mutate(y := pmax(0, y))

    # offset is 1 if any zero values in var, 0 otherwise
    offset = as.integer(any(x_tsibble %>% pull(y) == 0))

    # transform var
    x_tsibble = x_tsibble %>% mutate(y := log(y + offset))
  }

  stl_formula = y ~ trend(window = n_trend) +
                    season(period = seasonal_period, window = n_seasonal)

  stl_components = x_tsibble %>%
    model(STL(stl_formula, robust = TRUE)) %>%
    generics::components() %>%
    as_tibble() %>%
    transmute(
      trend = trend,
      seasonal = season_week,
      resid = remainder
    )
  
  # allocate the seasonal term from the STL decomposition to either the fitted
  # or the resid term
  if (include_seasonality) {
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

  # calculate lower and upper thresholds and replacement value
  if (detect_negatives && transform == "none") {
    min_lower = 0
  } else {
    min_lower = -Inf
  }
  outliers <- x %>%
    mutate(
      fitted = stl_components$fitted,
      resid = stl_components$resid) %>%
    epi_slide(slide_fun = function(x, var, ...) x %>% pull(resid) %>% IQR(),
              n = n_threshold, align = "centered",
              new_col_name = "roll_iqr") %>%
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
    # keep just the columns we want
    select(lower, upper, replacement) %>%
    # drop any extra classes; just keep tibble
    unclass() %>%
    as_tibble()

  # undo log transformation if necessary
  if (transform == "log") {
    outliers$lower <- exp(outliers$lower) - offset
    outliers$upper <- exp(outliers$upper) - offset
    outliers$replacement <- exp(outliers$replacement) - offset
  }

  # return
  return(outliers)
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
