# We use special features of data.table's `[`. The data.table package has a
# compatibility feature that disables some/all of these features if it thinks we
# might expect `data.frame`-compatible behavior instead. We can signal that we
# want the special behavior via `.datatable.aware = TRUE` or by importing any
# `data.table` package member. Do both to prevent surprises if we decide to use
# `data.table::` everywhere and not importing things.
.datatable.aware = TRUE

#' Archive (data version history) for an `epi_df` object
#'
#' Contains version history for an `epi_df` object, and enables fast querying of
#' snapshots of the `epi_df` object as of certain "issues" (versions). See the
#' [data versioning 
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/archive.html) for
#' examples. 
#'
#' @details Version history can be input as a data frame combining full
#'   snapshots of the `epi_df` as of several issue times, or using only the
#'   newly added or revised rows for each issue, or using some combination of
#'   these two (including "updates" for things that didn't actually
#'   change). Last-observation-carried-forward (LOCF) is used to data in between
#'   recorded updates. Currently, deletions must be represented as revising a
#'   row to a special state (e.g., making the entries `NA` or including a
#'   special column that flags the data as removed and performing
#'   post-processing), and the archive is unaware of what this state is.
#'
#' @importFrom assertthat assert_that
#' @importFrom data.table as.data.table
#' @importFrom dplyr filter rename 
#' @importFrom pipeR %>>%
#' @importFrom rlang !! is_named is_character is_scalar_atomic is_scalar_character warn
#' @importFrom tibble as_tibble
#' @export
epi_archive =
  R6::R6Class("epi_archive",
              private = list(
                update.DT = NULL,
                max.issue = NULL,
                issue.colname = NULL,
                geo.colname = NULL,
                time.colname = NULL,
                other.key.colnames = NULL
              ),
              public = list(
#' @description
#' Create a new \code{epi_archive} with the given update data
#' @param update.df the update data
                #' @param issue.colname name of the column with the issue time of the corresponding updates; operations such as \code{sort}, \code{<=}, and \code{max} must make sense on this column, with earlier issues "less than" later issues
                #' @param geo.colname the name of the column that will become \code{geo_value} in the \code{epi_df}
                #' @param time.colname the name of the column that will become \code{time_value} in the \code{epi_df}
                #' @param other.key.colnames the names of any other columns that would be used to index a single measurement in this data set, such as the age group the measurement corresponds to (if the data set includes an age group breakdown); there should only be a single row per issue-geo-time-other-key-cols combination.
                #' @param max.issue the latest issue for which update data was available; defaults to the maximum issue time in the \code{update.df}, but if there were no additions or revisions in subsequent issues, it could be later.  However, due to details regarding database replica syncing times in upstream APIs, using the default might be safer than whatever we think the max issue should be.
                #' @return an \code{epi_archive} object
                initialize = function(update.df,
                                      issue.colname = "issue",
                                      geo.colname = "geo_value",
                                      time.colname = "time_value",
                                      other.key.colnames = character(0L),
                                      max.issue = max(update.df[[issue.colname]])) {
                  assert_that (is.data.frame(update.df))
                  assert_that (is_scalar_character(issue.colname) && !is_named(issue.colname))
                  assert_that (is_scalar_character(geo.colname) && !is_named(geo.colname))
                  assert_that (is_scalar_character(time.colname) && !is_named(time.colname))
                  assert_that (is_character(other.key.colnames) && !is_named(other.key.colnames))
                  assert_that (issue.colname %in% names(update.df))
                  assert_that (geo.colname %in% names(update.df))
                  assert_that (time.colname %in% names(update.df))
                  assert_that (all(other.key.colnames %in% names(update.df)))
                  assert_that (identical(class(update.df[[issue.colname]]), class(max.issue)))
                  assert_that(length(unique(c(issue.colname, geo.colname, time.colname, other.key.colnames))) ==
                              3L + length(other.key.colnames))
                  assert_that(max.issue >= max(update.df[[issue.colname]]))
                  ## -- end of input validation --
                  update.DT = as.data.table(update.df, key=c(geo.colname, time.colname, other.key.colnames, issue.colname))
                  private[["update.DT"]] <- update.DT
                  private[["max.issue"]] <- max.issue
                  private[["issue.colname"]] <- issue.colname
                  private[["geo.colname"]] <- geo.colname
                  private[["time.colname"]] <- time.colname
                  private[["other.key.colnames"]] <- other.key.colnames
                },
                #' @description
                #' Get the \code{epi_df} as of some issue time
                #' @param issue the desired as-of issue time
                #' @return an \code{epi_df} with data as of the specified issue time, \code{issue} recorded in the metadata, the geo column renamed to \code{geo_value} and time column to \code{time_value}, and the other key colnames recorded in the metadata
                epi_df_as_of = function(issue) {
                  assert_that(is_scalar_atomic(issue) && identical(class(issue), class(private[["max.issue"]])))
                  assert_that(issue <= private[["max.issue"]])
                  if (issue == max(private[["update.DT"]][[private[["issue.colname"]]]])) {
                    ## (really, this should be the last issue with an actual
                    ## addition or revision; it's the same as what's checked
                    ## here only if we didn't include redundant "updates" in
                    ## this max issue; alternatively, we should follow the
                    ## user's indication and use `private$max.issue` and let
                    ## them deal with potential strange cases with replicas
                    ## being out of date)
                    warn('Getting epi_df as of the latest issue with recorded change data; it is possible that we have a preliminary version of this issue, the upstream source has updated it, and we have not seen those updates yet due to them not being published yet, or potentially due to latency in synchronization of upstream database replicas.  Thus, the epi_df snapshot that we produce here might not be reproducible at later times when we use an archive with fresher data.')
                  }
                  ## -- end of input validation --
                  private[["update.DT"]] %>>%
                    ## {.[, .SD[.[[private[["issue.colname"]]]] <= ..issue]]} %>>%
                    filter(.[[private[["issue.colname"]]]] <= .env[["issue"]]) %>>%
                    unique(by=c(private[["geo.colname"]], private[["time.colname"]], private[["other.key.colnames"]]), fromLast=TRUE) %>>%
                    as_tibble() %>>%
                    select(-!!private[["issue.colname"]]) %>>%
                    ## rename(issue_with_last_update = !!private[["issue.colname"]]) %>>%
                    rename(
                      geo_value = !!private[["geo.colname"]],
                      time_value = !!private[["time.colname"]],
                      ) %>>%
                    as_epi_df(issue = issue,
                                  additional_metadata = list(other.key.colnames = private[["other.key.colnames"]])) %>>%
                    return()
                },
                #' @description
                #' Return the name settings in a list
                naming_info = function() {
                  list(
                    issue.colname = private[["issue.colname"]],
                    geo.colname = private[["geo.colname"]],
                    time.colname = private[["time.colname"]],
                    other.key.colnames = private[["other.key.colnames"]]
                  )
                },
                #' @description
                #' Return the max issue value recorded by this archive (whether it had updates or not)
                max_issue = function() {
                  private[["max.issue"]]
                },
                #' @description
                #' Return the issue values for which updates are
                #' recorded in this archive (that is, whether they had updates in
                #' the data frame used to form this archive, regardless of whether
                #' those "updates" actually added or revised any data)
                issues_with_updates = function() {
                  return (unique(private[["update.DT"]][[private[["issue.colname"]]]]))
                },
                #' @description
                #'
                #' Return the recorded update data up through the given issue
                #' value, inside a \code{data.table} object which is fine to
                #' modify without copying.
                #'
                #' @param issue the max issue value that should appear in the result
                update_DT_as_of = function(issue) {
                  assert_that(is_scalar_atomic(issue) && identical(class(issue), class(private[["max.issue"]])))
                  assert_that(issue <= private[["max.issue"]])
                  if (issue == max(private[["update.DT"]][[private[["issue.colname"]]]])) {
                    ## (really, this should be the last issue with an actual
                    ## addition or revision; it's the same as what's checked
                    ## here only if we didn't include redundant "updates" in
                    ## this max issue; alternatively, we should follow the
                    ## user's indication and use `private$max.issue` and let
                    ## them deal with potential strange cases with replicas
                    ## being out of date)
                    warn('Getting epi_df as of the latest issue with recorded change data; it is possible that we have a preliminary version of this issue, the upstream source has updated it, and we have not seen those updates yet due to them not being published yet, or potentially due to latency in synchronization of upstream database replicas.  Thus, the epi_df snapshot that we produce here might not be reproducible at later times when we use an archive with fresher data.')
                  }
                  private[["update.DT"]] %>>%
                    ## {.[, .SD[.[[private[["issue.colname"]]]] <= ..issue]]} %>>%
                    filter(.[[private[["issue.colname"]]]] <= .env[["issue"]]) %>>%
                    return()
                }
              )
              )
