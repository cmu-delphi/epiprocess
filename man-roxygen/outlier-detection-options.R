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
