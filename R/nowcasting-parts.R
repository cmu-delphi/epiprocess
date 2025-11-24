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
#
# target acquirer: archive, settings -> target sets: iter<(train_tbl, test_tbl, weights)>
# - want test_tbl to identify epikeys... perhaps also x versions?
# - if want to pass to multivariate target engine or avoid multiple predictor searches, can output a size 1 result
# predictor acquirerer: archive, target set, settings -> predictor sets: iter<(train_tbl, test_tbl, weights)>
#
# issue: for backcasting or iterative forecasting, how does predictor selection know where to center search?
# * ideas:
#   - wide format + have target prep provide time center metadata
#   - wide format + pass along target description
#   - custom wide data structure to provide metadata that want?
#   - or just attach attrs to cols?
#   - long format, time_value-based target ids?
#   - long format, lag + signal name target ids?
# * wide format closer to what engine needs, and join to fewer rows, and make obvious how many target archetypes there are
# * long formats have easier access to extra information, and allows target sets to vary by epikey
#
# Target bundler?
#
# Predictor parts...
# - predictor proposer (test nonmissingness/completeness, spacing)
#   - except with differing non-NA max_t actually want to adjust the proposals, so this also might naturally have splits within itself
# - task splitter (test subtask nonmissingness/completeness, maybe some training stuff)
# - predictor filterer/judger (training stuff), perhaps somehow also used to inform task splitter?
#
# Try to represent these all within same framework?
# - fn(later_or_inner_pipeline) fn(targets, predictors, training, testing) function factories...
#   - need to force factory args
#   - generate black boxes without completely-separate metadata wrapping stuff
#   - handles target transformations
#   - keep earlier/outer stuff on stack... good (debug) and bad (RAM)
# - fn(targets, predictors, training, testing, later_or_inner_pipeline) functions...
#   - can be put into a list and filtered/combined easily
#   - handles target transformations, though looks weird in "sequence" with others
#   - ensembles would still look black-box-y unless handled separately
#     - though... are ensembles actually possible here?  testing vs. predictions formats...
# - fn(targets, predictors, training, testing) -> iter<(targets, predictors, training, testing)>
#   - not sure how easy to write... splitting might actually become more natural, but looping less natural
#   - doesn't handle transformations
# - fn(iter<(targets, predictors, training, testing)>) -> iter<(targets, predictors, training, testing)>
#   - might enable use of more iter tools, feel more natural if comfortable with
#   - but both this and above are forcing use of iterators...
# - fn(targets, predictors, training, testing, later_or_inner_pipeline) -> iter<(targets, predictors, training, testing, later_or_inner_pipeline)>
#   - could sort of handle target transformations, but awkwardly, having to tack something onto the later_or_inner_pipeline
#     - and can't determine where "inner" part ends...
#     - except no... if we end with the breakdowns to be fed into an engine, there's not an opportunity to reverse transformation
#   - ensembles... still black boxes w/o extra management
#   - iter -> iter format doesn't seem possible?
# - fn(iter<targets, predictors, training, testing, postprocessing>) -> iter<(targets, predictors, training, testing, postprocessing)>
#   - now this might handle target transformations
#   - postprocessing black box
#   - ensemble black box w/o extra metadata
# - (fn(iter<targs, feats, train, test>) -> same, fn(iter<targs, feats, train, preds>) -> same)
#   - still have whole ensemble thing
# - seems like there is a distinction between "later" and "inner"
#   - function-passing doesn't distinguish but is general
#   - task splitting/transformation approaches seem like they are only natural for "later"
#
# What would applying pipeline of continuations look like?
# for (i = imax..1) {
#   f <- partial(transformers[[i]], ..........., f)..... but does partial force f?
# }
# f(......)?
# 2x stack frames, stack frames not identifying steps
#
# vs.:
#
# run_step(steps, i)(...):
#   transformers[[i]](..., run_step(steps, i+1L))
# 2x stack frames, stack frames not identifying steps
#
# iterator-based approaches...
# - iter -> iter still going to involve large stack trace
# - elt -> iter might still... if don't want to store full breakdown, then would still be doing later within earlier handling
#
# Consider some layer where steps declare inner & outer things, but
# their main function does not bake them in, and instead expects them
# to be passed in by the runner?
# - allows meta collection
# - may look alien
# - risk that runner doesn't properly pass
#
# None of these is actually providing nice named stack traces...
#
# And actually, it's not clear that any of the above can build proper
# trained ensembles this way.  though some feature selection stuff
# might be good to happen as part of the component specification, this
# is still in-sample in a way.  We'd need to have an evolution of
# fits...  Same deal with calibration.  Could be done separately with
# epix_slide, though without the conditioning on feature selection
# etc.  That might be good.  But perhaps this format provides more
# flexibility on how to train it?  Actually, the problems may only
# arise when want to talk about training predictions, which are never
# even formed in this framework.  So of course trained ensembles and
# calibrators would not be part of it.  Though could potentially think
# about... if want to condition on current feat selection, then could
# gen training predictions by a sequence of anchor_version filters &
# applying the engine (though might incorporate extra info like center
# of centering transformation that do not want to be included..., plus
# computationally this probably makes caching possibilities more
# restricted).
#
# Decisions about which things to condition on might be
# nice... perhaps could be formulated as cv being inner or outer
# relative to e.g., feat selection or centering/scaling.  CV step
# would need to span the engine, and have idea of predictions,
# evaluation, ...  Might also want/need to have concept of fitted
# parameters, ...  Should param/debug stuff also be output of steps?
#
# Might not be that far from current framework... we have training
# timeless-pseudo-archive/version-series/NOT and testing timeless
# snapshot... already "conditioned on" earlier parts of pipeline;
# perhaps could just run a heads_map/version-series-cv w/ each fold
# applying later part of pipeline, eval/fit, apply to testing.
# - Problem: training targets... not actually a version series; it's
#   based on testing version data... but maybe an appropriately-placed
#   populate_target_training_data-type step will make this work
#   properly? and other placement provide more flexibility?
#
# elt -> iter style might actually provide more flexibility in
# pipeline loop/iter; could do all the iter_map_iters in one go,
# provide all debug info in one frame, ...  (Not sure about
# async-future-friendliness.)
#
#
#
# geo-pooling vs. splitting stuff?
#
# cross-geo stuff
#
#
#
#
#
#
# TODO rather than filter to every 7d/etc., actually perform averaging?  would that be accommodated by framework?


# for (predictor in proposed_predictors) {
#   if (predictor all acceptable) {
#     include in predictor list
#   } else if (predictor all unacceptable) {
#     continue
#   } else {
#     split into two,
#   }
# }

# predictor evaluations.... output {keys where acceptable, keys where unacceptable & reason} or something allowing multipartitioning and/or differing reasons (but do not split just based on reason).  or just flag & reason for all

# for (predictor in predictor_proposals) {
#   for (criterion in criteria) {
#     attach evaluation
#   }
#   split based on inclusion/exclusion...
# }

# but what about early continuations if rejected everywhere?  criteria dependent on previous ones passing?  (but also want extra debug info...)

full_chr_mapping_standardize <- function(full_mapping, chr_keys, full_mapping_arg = rlang::caller_arg(full_mapping), call = rlang::caller_env()) {
  if (is.null(names(full_mapping))) {
    vctrs::vec_set_names(vctrs::vec_recycle(full_mapping, vec_size(chr_keys)), chr_keys)
  } else {
    checkmate::assert_names(vctrs::vec_names(full_mapping), permutation.of = chr_keys, .var.name = full_mapping_arg)
    vctrs::vec_slice(full_mapping, chr_keys)
  }
}

#' Standardize a partial names->values mapping
#'
#' @examples
#'
#' partial_chr_mapping_standardize(c(target = 1), 0, c("target", "aux1", "aux2"))
#'
#' partial_chr_mapping_standardize(c(target = 1), c(target = 0, aux1 = 0, aux2 = 0), c("target", "aux1", "aux2"))
#'
#' @keywords internal
partial_chr_mapping_standardize <- function(partial_mapping, default_full_mapping, chr_keys, partial_mapping_arg = rlang::caller_arg(partial_mapping), call = rlang::caller_env()) {
  if (is.null(names(partial_mapping))) {
    vctrs::vec_set_names(vctrs::vec_recycle(partial_mapping, vec_size(chr_keys)), chr_keys)
  } else {
    checkmate::assert_names(vctrs::vec_names(partial_mapping), subset.of = chr_keys, .var.name = partial_mapping_arg)
    result <- full_chr_mapping_standardize(default_full_mapping, chr_keys)
    vctrs::vec_slice(result, names(partial_mapping)) <- partial_mapping
    result
  }
}

# window_predictor_shift_proposer <- function(archive,
#                                             target_shifts,
#                                             predictors = unique(target_shifts$var),
#                                             search_within = as.difftime(60, units = "days"),
#                                             search_offset = 0L,
#                                             min_spacing = as.difftime(7, units = "days"),
#                                             min_n_shifts = 0L,
#                                             max_n_shifts = 3L) {
#   .......
# }

# TODO abstract away shifts, averages, etc.; at core a fn mapping key-versions to val; metadata to describe relative window

# Other TODOs:
# - [ ] handle hole in versions from this govt outage well... not epix_slide default versions...
# - [ ] also for small backcast lookbehinds, ensure not missing half a week of later data?  just fix an offset to max in window?
