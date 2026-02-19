
# pipeline(
#   by_group(
#     flu_hosp_archive groups,
#     pipeline(
#       define_targets(flu_hosp, target_aheads),
#       add_semistable_target_training_data(..., flu_hosp_archive), # vs. combine with above?
#       by_target(
#         pipeline(
#           select_nonmissing_features( # adds training and test features as byproduct
#             pipeline(

#             )
#           )
#         )
#       )
#     )
#   )
# )

# by_group(
#   flu_hosp_archive groups,
#   add_semistable_targets(lagged(flu_hosp, target_aheads), ...),
#   by_target(
#     search_split_nonmissing_lag_features(
#       settings, settings, settings,
#       rest,
#       of,
#       pipeline
#       # here is where omitting pipeline call starts to potentially get awkward... except if we have pipeline at end and then add settings to interface, we'd need them after the pipeline arg or name anyway, so maybe not that different besides the potential name conflicts... unless require a single /pipeline/ arg by name.  But other way requiring params by name seems like may be better for readability anyway
#     )
#   )
# )

# named pipeline segments for referring back to... guess can still divvy out params vs segments with non-dot-prefixed vs dot-prefixed

# allow pipelines and segments to be used interchangeably, and use `c`
# impls to combine pipeline+{pipeline,segment},
# segment+{pipeline,segment} into pipelines?  maybe tweak terminology
# here and come up with common class for segments and pipelines.

# basic_pipeline <-
#   by_group(
#     flu_hosp_archive groups,
#     add_semistable_targets(lagged(flu_hosp, target_aheads), ...),
#     by_target(
#       search_split_nonmissing_lag_features(
#         # here is where omitting pipeline call starts to potentially get awkward...
#         mandatory_setting,
#         mandatory_setting,
#         flu_hosp_archive, # need to re-split by group in case it's not matching... and have had to repeat it a few times... maybe do want to bake it into pipeline?
#         .setting1 = something,
#         .setting2 = something,
#         .setting3 = something
#         #
#         # c( # require or not?
#         parsnip_fit_predict(),
#         # )
#         )
#     )
#   )

# # baking in archive / data sources vs. not...

# # baked in: we can rely on same types, resolution, groups.  May make
# # segments that mutate archive / data sources a possibility.

# # not baked in: we have earlier access to the types, resolution,
# # groups, in the segment constructors.  We may have multiple pointers
# # to data sets stored; not sure if that's an issue for save/load
# # memory size.  And it might make validation wrappers not work.

# # prod forecasts

# run_pipeline(c(basic_pipeline, pivot_quantiles_longer_segment))
# # vs.
# run_pipeline(basic_pipeline)


# # validation:

# # should we be requiring by_group etc. to be doing some special logic
# # to recognize equivalent targets, or require it, or require any
# # targets whose predictions should be kept post-grouping to be defined
# # before by_group?  or move to some sort of nested (not good for
# # general validation) or long format for predictions? or require
# # defining validation targets & data used separately?

# c(
#   define_horizon_targets(flu_hosp, 1:4),
#   # v or should we be treating these as test versions rather than training versions?
#   define_test_version(c()), # set to 0 test versions... or should this
#                             # be epikey-versions or
#                             # epikey-time-versions?
#   version_map(
#     # we need to control the archive fed into the basic pipeline
#     basic_pipeline
#   ), # runs and attaches for training versions and test versions / ...
#   add_semistable_target_training_data(),
#   score_training_forecasts()
# )

# TODO ensemble, calibration, iterative forecasts

# ensemble gets potentially messy... we want to define targets both
# for evaluation and for models, but defining targets is also a
# natural place to encode assumptions about semistability, which we
# may want separate.  Perhaps methods can be encoded as assumptions
# about target semistability + a fixed-relt set of feats&covars,
# though seems awkward for it to be encoding info about a specific
# target while pretending it's about an arbitrary target; could maybe,
# similar to relt part, encode as some sort of mapping, but that may
# get awkward as well... Perhaps could decompose methods into
# target-feat encoding and the later parts...

# potential issues with single archive for multiple methods having
# epix_merge change the set of ukeys and causing methods on subset of
# signals to change behavior
