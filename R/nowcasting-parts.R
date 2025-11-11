# nowcast(archive,
# 	targets_spec,
# 	predictors_spec
# 	weighting scheme? grouping regime? combined? mixed into above?
# 	) {
#   # c(training_targets, testing_targets) %<-% target_spec(archive)
#   # c(training_predictors, testing_predictors) %<-% predictor_spec(archive, training_targets)
#   have output of specs be a list<struct(training, testing)>>? or should this be the job of some function factories? but how would they know how to split when this seems coupled with the predictor selection?
# }
#
# predictor selection splitter
# target splitter
#
# Seems tricky to split based on predictor availability and hand off
# training sets rather than recompute, while remaining a friendly
# interface.
#
# Predictor splitter:
# - on latest_long set? might divide into unnecessarily many groups.
# - on prediction selection results? but that would require calculating on each epikey; seems expensive
# - integrated into selector? so whenever there is a different in availability, it can branch off decision-making?
#
# Predictor split merging step?
#
# We may also want to include 1-ahead iterative modeling...
#
# Suppose we have a simple nowcaster:
#
# simple_nowcaster(archive, target_vars, target_lags, predictor_vars, predictor_lags, weighting_scheme) {
#   target_data <- prepare_targets(target_vars, target_lags)
#   predictor_data <- prepare_predictors(predictor_vars, predictor_lags)
#   fit
#   predict
# }
#
# Then if we want to do predictor availability grouping:
#
# nowcaster2(archive, target_vars, target_lags, predictor_spec, weighting_scheme) {
#   target_data <- prepare_targets(target_vars, target_lags)
#   predictor_sets <- predictor_spec(archive, target_data)
#   for (predictor_set in predictor_sets) {
#     simple_nowcaster(archive,
#                      predictor_set$target_epikeys, ***
#                      predictor_set$target_vars, .......)
#   }
# }
#
# Not looking like a crisp delegation.  Need to brainstorm alternative breakdowns.
#
# There will also be select-predictors-per-target
# vs. select-predictors-jointly-but-split-based-on-ahead-availability-potentially-at-last-minute
# vs. unwise-select-predictors-jointly-and-na-omit-across-all-targets
# vs. iterative-one-ahead-outputting-distributions
#
# This is likely an outer layer?  Potentially built atop a univariate_target_forecaster?
#
# univariate_target_forecaster(archive, target_var, target_lag, predictor_spec, weighting_scheme??) {
# }
#
# multi_target_forecaster(archive, target_vars, target_lags, predictor_spec(s??), weighting_scheme(s??)??) {
#   map2(target_vars, target_lags, ~ univariate_target_forecaster(.......)) %>%
#     list_rbind()
#  # or one of the other approaches... might want more specific naming
# }
#
# but based on predictor part above, wondering if maybe target_var &
# target_lag in univariate should be replaced by training_targets and
# testing_targets (except no need for testing_targets except maybe for
# checks... but might be good to have standardized things outputting
# train&test...)
#
# weighting stuff... not sure if should be in arg here or a separate factory...
#
# back to predictors... what about
#
# some_predictor_layer(training_targets, testing_targets, training_target_weights, predictor_spec)??
#
# how do better compose?  abstraction for either spec or data for targets, predictors?
#
# trying to come up with these concepts vs. something like a
# mapper/callback-type approach vs. task list vs. task iterator.
#
# for debuggability, might prefer the task list or iterator vs. having
# to peer into nested functions.  though list might bloat memory, and
# iterator code might look confusing if error occurs within & target
# search might require high-powered coro generators...
