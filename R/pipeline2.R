
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
#       # here is where omitting pipeline call starts to potentially get awkward...
#       settings, settings, settings,
#       rest,
#       of,
#       pipeline
#     )
#   )
# )

# named pipeline segments for referring back to... guess can still divvy out params vs segments with non-dot-prefixed vs dot-prefixed
