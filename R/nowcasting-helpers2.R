
# extract2_horizon <- function(x, ekvs, var, horizon, ...) UseMethod("extract2_horizon")

# #' @export
# extract2_horizon.epi_archive <- function(x, ekvs, var, horizon, ...) {
#   assert_true(obj_is_tblish(ekvs))
#   ekvs <- tblish_cast_cols(ekvs, x$DT[, key(x$DT)]) # FIXME minus the time_value
#   assert_string(var) # XXX vs. tidyselect?
#   assert_subset(var, names(x$DT))
#   if (x$time_type == "week") {
#     if (!inherits(horizon, "difftime")) {
#       cli_abort("For weekly time_type, `horizon` must be a difftime.")
#     }
#     if (nrow(x$DT) == 0L) {
#       assert_subset(units_horizon, c("days", "weeks"))
#     } else {
#       if (length(unique(as.double(x$DT$version) %% 7)) != 1L) {
#         # FIXME this is probably going to eventually be pretty common,
#         # along with datetime version... TODO go back and consider
#         # adding the Hub version->reference_date concept.  Would have
#         # to be supplied since we don't yet store enough metadata.
#         # But then this would maybe handle this case and simplify the
#         # other complicated reasoning below.
#         cli_abort(c("`time_value`s are weekly, but `version`s are not; this is currently not supported",
#                     ">" = "please thin out the versions to a weekly cadence, or use daily time values if possible"),
#                   class = "epiprocess__extract2_horizon__weekly_time_nonweekly_version")
#       }
#       alignment_horizon_days_dbl <- as.double(x$DT$time_value[[1L]] - x$DT$version[[1L]]) %% 7
#       if (units(horizon) == "days" && as.double(horizon) %% 7 != alignment_horizon_days_dbl) {
#         cli_abort(c("`horizon` is requesting impossible `time_value`s",
#                     "i" = 'horizon: {horizon} days',
#                     "i" = '`time_value`s of `x` fall on {format(x$DT$time_value[[1L]], "%a")}',
#                     "i" = '`version`s of `x` fall on {format(x$DT$version[[1L]], "%a")}',
#                     "x" = "Adding {horizon} days to any `version` will not
#                            correspond to any possible `time_value`",
#                     "i" = "`epi_archive`s currently don't store enough information
#                            about the weekly time values to know how to recover",
#                     ">" = 'Update `horizon` so that adding it to a
#                            {format(x$DT$version[[1L]], "%a")} will yield a
#                            {format(x$DT$time_value[[1L]], "%a")}'
#                     ),
#                   class = "epiprocess__extract2_horizon__horizon_days_misaligned")
#       } else if (units(horizon) == "weeks" && alignment_horizon_days_dbl != 0) {
#         cli_abort(c("`horizon` is in terms of weeks, but `time_value`s and `version`s
#                      fall on differing weekdays; not sure how to align them",
#                     ">" = "provide `horizon` in terms of days instead"),
#                   class = "epiprocess__extract2_horizon__horizon_weeks_misaligned")
#       } else {
#         valid_units <- if (alignment_horizon_days_dbl == 0L) c("days", "weeks") else "days"
#         if (! units(horizon) %in% valid_units || ! rlang::is_integerish(as.double(horizon))) {
#           cli_abort('`horizon` must be in terms of integer number of
#                      {cli_vec(valid_units, style = list("vec-last" = " or "))}',
#                   class = "epiprocess__extract2_horizon__horizon_bad_units_or_not_integer")
#         }
#       }
#     }
#   } else {
#     validate_slide_window_arg(horizon, x$time_type, lower = -Inf, allow_inf = FALSE)
#   }
#   ektvs <- ekvs
#   tblish_col(ektvs, "time_value") <- tblish_col(ektvs, "version") + horizon
#   ektvs <- tblish_select(ektvs, key(x$DT)) # in case data.table relies on var order not names
#   result <- x$DT[ektvs, var, on = key(x$DT), with = FALSE, roll = TRUE]
#   result <- result[[var]]
#   # We don't actually have observations for versions past versions_end.
#   #
#   # XXX perhaps we should instead base this on the max version with a
#   # diff overall or by epikey.
#   vec_slice(result, ektvs$version > x$versions_end) <- NA
#   result
# }

# XXX mostly ditching tblish for now

#' For each `ekt_var_set` key, the `version`s it had with a "real" diff for any `val_var_set`
#'
#' Here, "real" means either (a) changing from unobserved to a non-NA
#' initial estimate, or (b) changing from one estimate to a different
#' estimate.  It does not count moving from unobserved to an NA
#' initial estimate, as these are often not actually present in the
#' underlying data and are simply present due to storing multiple val
#' vars in the same archive.
#'
#' The choice of `ekt_var_set` is a tradeoff between (a) recognizing the
#' maturity of estimates that remained the same between versions for
#' reasons other than an outage, plus enabling models to treat more
#' estimates as equally mature, vs. (b) recognizing the immaturity of
#' estimates that remained the same between versions due to more
#' complex types of outages.
#'
#' * If some epikeys (geodemographic groups) can have pauses in
#'   reporting not shared by others, then you should probably include
#'   `c("geo_value", x$other_keys)` in `ekt_var_set`.
#'
#' * If some `time_value`s can have pauses in reporting not shared by
#'   others, then you should probably include `"time_value"` in
#'   `ekt_var_set`.  Depending on how downstream models treat NAs, you
#'   may be able to get away with not adding it if these pauses in
#'   reporting simply mean that no new `time_value`s are being added
#'   to the data set, but all previously-observed `time_value`s are
#'   receiving revised estimates.
#'
#' @param x (unvalidated) an `epi_archive`
#' @param ekt_var_set optional; (unvalidated) subset of
#'   `key_colnames(x, exclude = "version")`; defaults to
#'   `key_colnames(x, exclude = c("time_value" ,"version"))`
#' @param val_var_set technically optional; (unvalidated) subset of
#'   `val_colnames(x)`, but likely just a single var; defaults to
#'   entire set
#' @return tibble with names matching `ekt_var_set`; note that if you
#'   want downstream code to adapt to the choice of `ekt_var_set`, you
#'   will also need to pass `ekt_var_set` to later processing and may
#'   need to specify `relationship = "many-to-{one,many}"` if joining
#'   to other data.
#'
#' @keywords internal
epix_diff_keys <- function(x,
                           ekt_var_set = key_colnames(x, exclude = c("time_value" ,"version")),
                           val_var_set = val_colnames(x)) {
  ektv_vars <- key_colnames(x)
  diff_keys <- x$DT[, c(ektv_vars, val_var_set), with = FALSE] %>%
    setDF() %>%
    as_tibble() %>%
    filter(!update_is_locf(., ektv_vars, 0, TRUE)) %>%
    distinct(pick(all_of(c(ekt_var_set, "version"))))
  diff_keys
}

extract2_tvshift <- function(x, ektvs, var, tshift, vshift, vtol = NULL, ...) UseMethod("extract2_tvshift")

#' @export
extract2_tvshift.epi_archive <- function(x, ektvs, var, tshift, vshift, vtol = NULL, ...) {
  assert_class(ektvs, "tbl_df")
  ektvs <- tblish_cast_cols(ektvs, x$DT[, key_colnames(x)])
  assert_string(var)
  assert_subset(var, names(x$DT))
  tshift <- time_delta_standardize(tshift, x$time_type, "fast")
  version_type <- guess_time_type(x$DT$version)
  vshift <- time_delta_standardize(vshift, version_type, "fast")
  if (is.null(vtol)) {
    ek_vars <- c("geo_value", x$other_keys)
    x_var_diff_ekvs <- epix_diff_keys(x, ek_vars, var)
    low_vgap <- x_var_diff_ekvs %>%
      summarize(.by = all_of(ek_vars),
                # re-using reserved name `version` for version gaps:
                version = diff(sort(unique(version)))) %>%
      .$version %>%
      # Don't be too strict; if schedule isn't perfectly regular
      # (e.g., there are delays around holidays) plus there are
      # special data set overhauls, we don't want presence of the
      # latter to make us balk at the former:
      quantile(probs = 0.10) %>%
      unname()
    # TODO something like vtol_excl <- min(min_vgap, tspacing/2)
    # except may need to standardize types, apply some sort of floor,
    # etc.
    stop("TODO finish")
  } else {
    # TODO difftime -> approx floor delta?
    vtol <- time_delta_standardize(vtol, version_type, "fast")
    stop("TODO finish")
  }
  stop("TODO finish")
}

# XXX Reconsidering `vtol` arg... we are going to constrain ektvs to
# "real" ones anyway... except if vshift is nonzero, then we need the
# shifted version to be "real", not the anchor version to be "real".
# But then if we don't constrain ektvs to be "real", then the extract2
# approach may fall apart, and we will have to be joining tibbles
# again...  Also, vtol may be useful for lag analysis.
#
# No, we need to constrain analogue ektvs to also be "real" for the
# involved signal.  "Real" to "real".

# In production, there is the forecast time / the
# "safe"/late-enough-to-have-data forecast time, then there is the
# "real" version it uses.  Should we be finding analogues based on the
# former or the latter?  A shifting prediction schedule would be based
# on target real updates.  We may not want to apply that leeway to
# auxiliary covariates.  But for delayed forecasts we could also
# benefit from later versions of covariates, though that should likely
# be reflected in the test time characterization.  And this is about
# getting training data.  Prediction schedule doesn't really matter.
# Do we consider delayed target reporting valid for this purpose?
#
# Seems like may be forced into having analogue tvs be made via shifts
# of tvs, filtered to after the first real version for the target
# signal (so that predictor percentage availability metrics don't toss
# everything).  Vs. starting with the "real" rtvs.  Though... if we
# are judging by target signal anyway, perhaps can start with analogue
# rtvs based on the target signal anyway...

# Should default vtol be 3h or 3d?  Seems like may want to use floor
# of 1/2 of median inter-real version gap.  Though for daily-weekly
# need to make sure analogue rtvs are weekly, either via automatic
# rule or manual wday matching rule.

# How bad is large vtol?  Lining up with substantially more/less
# accurate data?

# Situation not just of data being more latent, but also less latent.
# Might need backoff to stale version that actually know patterns of.
