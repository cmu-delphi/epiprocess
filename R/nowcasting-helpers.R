
epix_predictor_training_data <- function(archive, varname, lag,
                                         versions = epix_slide_versions_default(archive),
                                         predictor_name = "{.col}_{.dir}_{.amt}") {
  assert_class(archive, "epi_archive")
  time_type <- archive$time_type
  assert_string(varname)
  checkmate::assert_true(varname %in% colnames(archive$DT))
  lag <- time_delta_standardize(lag, time_type)
  versions <- vec_cast_patched(versions, archive$DT$version)
  assert_string(predictor_name)
  predictor_name <- glue::glue_data(list(
    .col = varname,
    .dir = if (lag >= 0) "lag" else "lead",
    .amt = paste0(time_delta_to_n_steps(lag, time_type), time_type_unit_abbr(time_type))
  ), predictor_name)
  #
  epikey_names <- key_colnames(archive, exclude = c("time_value", "version"))
  epikeys <- unique(archive$DT, by = epikey_names, cols = character())
  requests <- epikeys[, list(version = versions), by = names(epikeys)][
  , time_value := version - ..lag
  ]
  result <- archive$DT[
    requests,
    c(key(archive$DT), varname), with = FALSE,
    on = key(archive$DT),
    roll = TRUE, nomatch = NA,
    allow.cartesian = TRUE # remove unnecessary & misleading check
  ]
  nms <- names(result)
  nms[[match(varname, nms)]] <- predictor_name
  setnames(result, nms)
  setDF(result)
  as_tibble(result) %>%
    select(-time_value) %>%
    rename(forecast_date = version)
}
# TODO indexing based on a reference_date / reference_time? as in Hub?
