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
# Going back to function factories... are these actually going to be
# clearest?  Can separate into function(non-step hyperparms)
# function(inside_steps, later_steps) {...} to potentially allow for
# metadata structure.
#
# What about just elt -> elt?  And everything that branches or has
# post-processing is required to rbind or perform whatever other
# combination is required at the end.  Still need way to register
# inner pipelines...
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



# new_pipeline_step <- function(hyperparams, inner_pipelines, transform) {

# }

# new_pipeline_step_generator <- function() {
# }

# transform_target <- function(targets, transform, inverse_transform) {
#   new_pipeline_step(list(targets, transform, inverse_transform), )
# }

# new_pipeline_segment_factory <- function(factory_fn, settings_names = rlang::fn_fmls_names(factory_fn)) {
#   new_pipeline_segment(
#     function(input) {
#       # TODO validate factory_fn output format?
#       factory_fn NOT(input$targets, input$features, input$training, input$testing, input$inferences)
#     }
#   )
# }

check_env_has <- function(x, nms, inherit = FALSE, .var.name = checkmate::vname(x)) {
  has <- rlang::env_has(x, nms, inherit = inherit)
  if (all(has)) {
    TRUE
  } else {
    missing_nms <- names(has)[!has]
    cli::format_inline('{ .var.name} is missing direct{if (inherit) " or inherited" else ""} bindings for {format_varnames(nms)}')
  }
}

assert_env_has <- function(x, nms, inherit = FALSE, .var.name = checkmate::vname(x)) {
  assert(check_env_has(x, nms, inherit = inherit, .var.name = .var.name))
}

# TODO simplify args... just training, testing, meta/info, sources?
#
# segment-specific meta (with names/indices determined by pipeline construction call)
# vs. some global meta (combined predictor and target specs?)
# vs. both
#
# output of each segment just its own specific meta; forbid modifying
# others'? though global/shared meta needs mutability
#
# how to access meta info when splitting by target?
# * meta specific to the split?
# * context parent pipeline step meta? (global name pool or ???)
#
# workflows info obj in global meta?

# TODO handle hole in versions from this govt outage well... not epix_slide default versions...
# TODO also for small backcast lookbehinds, ensure not missing half a week of later data?  just fix an offset to max in window? except then not aligned with target... maybe prefer making a partial week target separate from full-week ones

new_pipeline_segment <- function(marker_subclass, segment_fn, settings_names = names(environment(segment_fn))) {
  # FIXME what happens with `...` args?
  assert_function(
    segment_fn,
    args = c("targets", "features", "training", "testing", "inferences", "archive"), ordered = TRUE,
    nargs = 6L
  )
  assert_env_has(environment(segment_fn), settings_names, inherit = TRUE)
  # Enforce forcing of args in case we forgot (but requiring NSE args to be munged):
  rlang::env_get_list(environment(segment_fn), settings_names, inherit = TRUE)

  pipeline_segment <- function(input) {
    segment_fn(input$targets, input$features, input$training, input$testing, input$inferences, input$archive)
    # TODO validate factory_fn output format?
  }
  class(pipeline_segment) <- c(marker_subclass, "pipeline_segment")
  attr(pipeline_segment, "epiprocess:::settings_env") <- environment(segment_fn)
  attr(pipeline_segment, "epiprocess:::settings_names") <- settings_names
  pipeline_segment
}

remove_str_dotdot <- function(object) {
  result <- list(object)
  class(result) <- "remove_str_dotdot"
  result
}

#' @export
str.remove_str_dotdot <- function(object,
                                  ...,
                                  nest.lev = 0,
                                  indent.str = paste(rep.int(" ", max(0, nest.lev + 1)),
                                                     collapse = "..")) {
  indent.str <- gsub("\\.\\.$", "", indent.str)
  str(object[[1L]], ..., nest.lev = nest.lev, indent.str = indent.str, comp.str = "")
}

#' `str` impl for pipeline segments
#'
#' @examples
#' transform_target_segment("y", log, exp, engine_segment(parsnip::linear_reg())) %>% str()
#'
#' @export
#' @keywords internal
str.pipeline_segment <- function(object,
                                 ...,
                                 nest.lev = 0,
                                 indent.str = paste(rep.int(" ", max(0, nest.lev + 1)),
                                                    collapse = "..")) {
  cat(cli::format_inline('<{class(object)[[1L]]}>:\n'))
  settings <- rlang::env_get_list(attr(object, "epiprocess:::settings_env"),
                                  attr(object, "epiprocess:::settings_names"),
                                  inherit = TRUE)
  str(
    lapply(settings, remove_str_dotdot),
    # settings,
    nest.lev = nest.lev + 1L,
    # indent.str = paste0(gsub("\U00251C\U002500", "\U002502 ", indent.str), "\U00251C\U002500 "),
    # indent.str = indent.str,
    indent.str = paste0(indent.str, "\U002502 "),
    no.list = TRUE,
    comp.str = "\U0008\U0008\U00251C\U002500",
    collapse = ""
  )
}

#' @export
print.pipeline_segment <- function(x, ...) {
  str(x)
}

# TODO $, [, [[ to access settings of pipeline segment

# TODO str etc.... needs to look different with `pipeline` (segment sequences) there...

#' attach_semistable_target_training_segment
#'
#' @examples
#'
#' attach_semistable_target_training_segment()(list(
#'   targets = tibble(varname = "percent_cli", version_relative_time = 0:1),
#'   features = tibble(),
#'   training = NULL,
#'   testing = NULL,
#'   inferences = NULL,
#'   archive = archive_cases_dv_subset
#' ))$training
attach_semistable_target_training_segment <-
  function(time_until_semistable = as.difftime(60, units = "days"),
           anchor_versions = NULL,
           out_name = "{.col}_{.amt}{.dir}_evaluation",
           nomatch = NA,
           drop_na = TRUE) {
  new_pipeline_segment(
    "attach_semistable_target_training_data",
    function(targets, features, training, testing, inferences, archive) {
    # XXX some awkwardness from not having archive$time_type earlier...
      if (inherits(time_until_semistable, "difftime")) {
        time_until_semistable <- difftime_approx_ceiling_time_delta(time_until_semistable, archive$time_type)
      }
      if (is.null(anchor_versions)) {
        anchor_versions <- vctrs::vec_unique(archive$DT$version)
      }
      if (!is.null(training)) {
        anchor_versions <- vctrs::vec_set_intersect(anchor_versions, training$key$anchor_version)
      }
      purrr::walk2(targets$varname, targets$version_relative_time,
                   function(varname, version_relative_time) {
                     target_training_data <- epix_target_evaluation_data(archive, varname, version_relative_time, time_until_semistable, anchor_versions, out_name, nomatch)
                     if (drop_na) {
                       # FIXME not operating properly
                       target_training_data <- vctrs::vec_slice(target_training_data, vctrs::vec_detect_complete(target_training_data[[length(target_training_data)]]))
                     }
                     target_training_data <- target_training_data %>%
                       tidyr::pack(key = all_of(c(key_colnames(archive, exclude = c("time_value", "version")), "anchor_version"))) %>%
                       tidyr::pack(values = !key)
                     # no weights
                     if (is.null(training)) {
                       # TODO weights?
                       training <<- target_training_data
                     } else {
                       # FIXME not working properly w/ `values` col
                       training <<- dplyr::inner_join(training, target_training_data, by = "key")
                     }
                   })
      tibble::lst(targets, features, training, testing, inferences, archive)
    }
  )
}

# TODO target & feature specs should probably give them names, and map to an abstraction more general than a shift.

transform_target_segment <- function(target, f, finv, inner_pipeline) {
  new_pipeline_segment(
    "transform_target_segment",
    function(targets, features, training, testing, inferences, archive) {
      stop("TODO")
    }
  )
}

engine_segment <- function(engine) {
  new_pipeline_segment(
    "engine_segment",
    function(targets, features, training, testing, inferences, archive) {
      stop("TODO")
    }
  )
}

pipeline <- function(...) {
  segments <- list(...)
  assert_list(segments, "pipeline_segment")
  # XXX the pipeline execution could handle the re-expansion of the
  # output to multiple input args for the next segment; don't need the
  # segment wrapper function.  Just want to be sure to be rigid about
  # accepting pipelines not segments.
  #
  # XXX or consider just having the base segment functions take `input`
  # to begin with?  maybe less typing.  pipeline could do validation
  #
  # for now, hack it like a segment
  new_pipeline_segment(
    "pipeline",
    function(targets, features, training, testing, inferences, archive) {
      state <- list(targets, features, training, testing, inferences, archive)
      for(segment in segments) {
        state <- segment(state)
      }
      state
    }
  )
}

# TODO should archive / data sources actually be part of the pipeline?

# TODO inferences vs. info

# TODO might need col/multicol role and extra info
# tracking... `targets`, `features`, and packed `key` handles part.
# But then there is recipes/workflows interop.  And non-`targets`,
# non-`features`.  Is there a better way?


# Pipeline runners: targets/tar-based, future-based, vanilla, etc.

# Have each step only return its own info, and handle combination with
# others in a uniform way in runners, assigning names according to
# segment names.

# pipeline info extractors: allows switching between keeping default
# info and more info for detailed views (e.g., powering dashboards);
# default default just keeps all returned info

# Optional pipelines segments and wrappers:
#
# * Segments: `c` with `NULL` natural, but won't show up in pipeline
#   as an omitted/skipped step, which might be desirable.
#
# * Segments, wrappers: could have wrappers and meta-wrappers to
#   enable/disable segments and wrappers, though perhaps awkward

# todo consider some sort of checker for training weight ignorance in
# fitters, etc.

# todo is there a way to avoid repeating target configuration for
# training and evaluation?  seems not a great idea to be forced into,
# because changing eval would change underlying models, and might want
# to make different choices and keep models static

# todo should models be functions from configs to pipelines where
# features are specified in args, or should possible features be
# communicated through something like roles?  Similar to target
# configs, seems less arcane to just put in function args, even though
# we may be repeating some information.  If really don't like
# repeating, likely have some similar issues when trying to
# standardize arg lists vs. standardizing role/col-metadata/... stored
# in pipeline.
#
# ... but want way to be able to transform variables, and that seems
# to require the role part in some approaches, at least after getting
# things out of archives (though some things may want archive col info
# as well... hope to omit that at first).  Thought transformations and
# imputations would be required to create new columns and mutate
# roles, rather than mutate columns, so we won't combine fit/etc. info
# with cols that have been mutated underneath us.  But if wrappers are
# well-behaved, e.g., inverting transformations, then would
# mutate-cols be okay for wrappers at least?  But this would
# invalidate some col info, e.g., whether something is a count or not.
#
# pop normalization transformation... do we need to know count-like
# vs. rate-like?  but originals should have been on same scale as
# target to have made sense, so... maybe not? but at least need to
# avoid scaling training weights
#
# parallel between training&test col info and archive col info... can
# we somehow change to same-ish interface, perhaps switching in some
# middle/wrapped part of pipeline from archives to tibbles and back?
# seems like that would mean making training and test
# archives... doesn't immediately seem ridiculous
#
# TODO consider (train_archive, test_archive, col_info, fit_info)
# pipeline... might fit better into recipes, but still want/need
# wrappers for archive cv, calibration, ensembles, etc. to feel
# natural

# todo consider also smoothing and Mercer kernel methods... train x
# test dimensions... perhaps can just provide indices into matrix held
# in fit_info, but perhaps not..
#
# current training weight scheme idea would be to force into col in
# test, and the same for all test instances

# xxx rule of either adding NAs to existing columns or adding new
# columns, not mutating, doesn't quite seem to work if want to allow
# adding additional fake training data without first putting into
# archive.  Though maybe this can be handled via another segment that
# combines several pipelines for prepping training and test data
# (could also track other info via some source id column, which might
# need to have anyway to have a ukey if need one).

# may also need test target treatments... ensure missing, ignore,
# ignore if too fresh?
