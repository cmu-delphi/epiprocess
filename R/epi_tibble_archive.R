## We use special features of data.table's `[`. The data.table package has a
## compatibility feature that disables some/all of these features if it thinks
## we might expect `data.frame`-compatible behavior instead. We can signal that
## we want the special behavior via `.datatable.aware = TRUE` or by importing
## any `data.table` package member. Do both to prevent surprises if we decide to
## use `data.table::` everywhere and not importing things.
.datatable.aware = TRUE

#' Archive (data version history) for an \code{epi_tibble}
#'
#' Contains version history for an \code{epi_tibble}, and enables fast querying
#' of snapshots of the \code{epi_tibble} as of certain "issues" (versions).
#' Version history can be input as a data frame combining full snapshots of the
#' `epi_tibble` as of several issue times, or using only the newly added or
#' revised rows for each issue, or using some combination of these two
#' (including "updates" for things that didn't actually change).
#' Last-observation-carried-forward (LOCF) is used to data in between recorded
#' updates. Currently, deletions must be represented as revising a row to a
#' special state (e.g., making the entries \code{NA} or including a special
#' column that flags the data as removed and performing post-processing), and
#' the archive is unaware of what this state is.
#'
#' @examples
#'
#' update.df =
#'   tibble::tribble(
#'     ~geo_value, ~time_value, ~issue, ~value,
#'     ## update history of geo1 for reference time 2021-01-01:
#'     ##   (1 day of latency in initial report)
#'     "geo1", as.Date("2021-01-01"), as.Date("2021-01-02"), 5.0,
#'     ##   (revised upward)
#'     "geo1", as.Date("2021-01-01"), as.Date("2021-01-03"), 9.0,
#'     ##   (revised upward)
#'     "geo1", as.Date("2021-01-01"), as.Date("2021-01-10"), 9.2,
#'     ## update history of geo1 for reference time 2021-01-02:
#'     ##   (1 day of latency in initial report)
#'     "geo1", as.Date("2021-01-02"), as.Date("2021-01-03"), 8.0,
#'     ##   (redundant "update" row; we will already be using LOCF to fill in)
#'     "geo1", as.Date("2021-01-02"), as.Date("2021-01-04"), 8.0,
#'     ##   (replaced with NA)
#'     "geo1", as.Date("2021-01-02"), as.Date("2021-01-10"), NA_real_,
#'     ## update history of geo1 for reference time 2021-01-05 (suppose data set skips the 3rd and 4th)
#'     ##   (1 day of latency in initial report)
#'     "geo1", as.Date("2021-01-05"), as.Date("2021-01-06"), 13.0,
#'   )
#'
#' ## update.df actually contains update data through issue 2021-01-11, but the
#' ## data set was not reported to change from 2021-01-10 to 2021-01-11
#' epi.tibble.archive = epi_tibble_archive$new(update.df, max.issue=as.Date("2021-01-11"))
#'
#' ## The snapshot as of issue 2021-01-03 just looks like the updates in issue
#' ## 2021-01-03, because all past measurements were updated in this issue (we
#' ## don't need to do any LOCF to obtain the snapshot).
#' epi.tibble.archive$epi_tibble_as_of(as.Date("2021-01-03"))
#'
#' ## The snapshot as of issue 2021-01-05 uses LOCF on the first two geo-time
#' ## combinations. Note that there is no entry for `time_value` 2021-01-05, as the
#' ## initial version of this data was not available until issue 2021-01-06.
#' epi.tibble.archive$epi_tibble_as_of(as.Date("2021-01-05"))
#'
#' ## The snapshot as of issue 2021-01-06 does include the measurement for
#' ## `time_value` 2021-01-05.
#' epi.tibble.archive$epi_tibble_as_of(as.Date("2021-01-06"))
#'
#' ## (Don't automatically run this example as it involves network access and querying the API)
#' if (FALSE) {
#'   library(dplyr)
#'   ## (delphi.epidata package is on GitHub in cmu-delphi/delphi-epidata-r)
#'   update.df.2 =
#'     delphi.epidata::covidcast("jhu-csse", "confirmed_incidence_num",
#'                               "day", "state",
#'                               delphi.epidata::epirange(12340101,34560101), c("ak","al"),
#'                               issues = delphi.epidata::epirange(12340101,34560101)) %>%
#'     delphi.epidata::fetch_tbl()
#'   snapshot.df.2a =
#'     delphi.epidata::covidcast("jhu-csse", "confirmed_incidence_num",
#'                               "day", "state",
#'                               delphi.epidata::epirange(12340101,34560101), c("ak","al"),
#'                               as_of = 20201014) %>%
#'     delphi.epidata::fetch_tbl()
#'   snapshot.df.2b =
#'     delphi.epidata::covidcast("jhu-csse", "confirmed_incidence_num",
#'                               "day", "state",
#'                               delphi.epidata::epirange(12340101,34560101), c("ak","al"),
#'                               as_of = 20201028) %>%
#'     delphi.epidata::fetch_tbl()
#'
#'   epi.tibble.archive.2 = epi_tibble_archive$new(update.df.2)
#'   all.equal(
#'     as_tibble(epi.tibble.archive.2$epi_tibble_as_of(as.Date("2020-10-14"))),
#'     as_tibble(as.epi_tibble(snapshot.df.2a)),
#'     check.attributes = FALSE)
#'   all.equal(
#'     as_tibble(epi.tibble.archive.2$epi_tibble_as_of(as.Date("2020-10-28"))),
#'     as_tibble(as.epi_tibble(snapshot.df.2b)),
#'     check.attributes = FALSE)
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom rlang is_scalar_character is_named is_character is_scalar_atomic !! warn
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename filter
#' @importFrom data.table as.data.table
#' @importFrom pipeR %>>%
#' @export
epi_tibble_archive =
  R6::R6Class("epi_tibble_archive",
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
                #' Create a new \code{epi_tibble_archive} with the given update data.
                #' @param update.df the update data
                #' @param issue.colname name of the column with the issue time of the corresponding updates; operations such as \code{sort}, \code{<=}, and \code{max} must make sense on this column, with earlier issues "less than" later issues
                #' @param geo.colname the name of the column that will become \code{geo_value} in the \code{epi_tibble}
                #' @param time.colname the name of the column that will become \code{time_value} in the \code{epi_tibble}
                #' @param other.key.colnames the names of any other columns that would be used to index a single measurement in this data set, such as the age group the measurement corresponds to (if the data set includes an age group breakdown); there should only be a single row per issue-geo-time-other-key-cols combination.
                #' @param max.issue the latest issue for which update data was available; defaults to the maximum issue time in the \code{update.df}, but if there were no additions or revisions in subsequent issues, it could be later.  However, due to details regarding database replica syncing times in upstream APIs, using the default might be safer than whatever we think the max issue should be.
                #' @return an \code{epi_tibble_archive} object
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
                #' Get the \code{epi_tibble} as of some issue time
                #' @param issue the desired as-of issue time
                #' @return an \code{epi_tibble} with data as of the specified issue time, \code{issue} recorded in the metadata, the geo column renamed to \code{geo_value} and time column to \code{time_value}, and the other key colnames recorded in the metadata
                epi_tibble_as_of = function(issue) {
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
                    warn('Getting epi_tibble as of the latest issue with recorded change data; it is possible that we have a preliminary version of this issue, the upstream source has updated it, and we have not seen those updates yet due to them not being published yet, or potentially due to latency in synchronization of upstream database replicas.  Thus, the epi_tibble snapshot that we produce here might not be reproducible at later times when we use an archive with fresher data.')
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
                    as.epi_tibble(issue = issue,
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
                    warn('Getting epi_tibble as of the latest issue with recorded change data; it is possible that we have a preliminary version of this issue, the upstream source has updated it, and we have not seen those updates yet due to them not being published yet, or potentially due to latency in synchronization of upstream database replicas.  Thus, the epi_tibble snapshot that we produce here might not be reproducible at later times when we use an archive with fresher data.')
                  }
                  private[["update.DT"]] %>>%
                    ## {.[, .SD[.[[private[["issue.colname"]]]] <= ..issue]]} %>>%
                    filter(.[[private[["issue.colname"]]]] <= .env[["issue"]]) %>>%
                    return()
                }
              )
              )
