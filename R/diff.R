
## To make a compressed `epi_tibble_archive` with only the necessary rows and no
## redundant updates, to add snapshots to an existing archive, and provide other
## functionality, extra information, and checks, it may help to introduce the
## concept of a "diff" between two issues. A simple diff could simply note which
## measurements were removed and which were updated (that is, added or revised
## with no distinction between the two), with the new values. A more complex
## diff would note the issue value for the snapshot beforehand and afterward,
## whether the update was a removal, addition, or revision, plus the values
## beforehand.

## TODO make this more like an assert_that helper function
#' @importFrom assertthat assert_that
stop_if_not_rbind_coltype_compatible = function(df1, df2) {
  assert_that(inherits(df1, "data.frame"))
  assert_that(inherits(df2, "data.frame"))
  ## just try the rbind on zero row selection and let it stop if it detects a problem
  rbind(df1[integer(0L),], df2[integer(0L),])
  ## if we made it here, we are rbind coltype compatible; just return (NULL) rather than the rbind result
  return ()
}

## XXX consider class Snapshot that attaches the version id?  or S3 functions with implementations for epi_tibble

## NOTE on column prefixes in diff DTs
##
## Since the user could be using any column names in their DTs, we should (i)
## apply a prefix to all user data, and (ii) avoid using column names that share
## a prefix used for user data for non-user data.

#' Select columns named with a given prefix and remove the prefix
#'
#' @param x an object with a structure like a data frame
#' @param prefix the column name prefix to select on and to remove
#' @return the same type of structure with the selected renamed columns
#'
#' @examples
#'
#' select_substr_prefix_columns(data.table::data.table(prefix.a = 1:5, prefix.b=11:15, nonprefixed=letters[1:5]), "prefix.")
#'
#' @export
select_substr_prefix_columns = function(x, prefix, ...) {
  UseMethod("select_substr_prefix_columns", x)
}

#' @importFrom data.table setnames
#'
#' @method select_substr_prefix_columns data.table
#' @export
select_substr_prefix_columns.data.table = function(x, prefix, ...) {
  x[, which(startsWith(names(x), prefix)), with=FALSE] %>>%
    setnames(function(nonprefixed.names) substr(nonprefixed.names, nchar(prefix)+1L, nchar(nonprefixed.names))) %>>%
    {.[]} # remove internal invisibility flag
}

#' @importFrom data.table is.data.table set
#' @importFrom rlang !!!
#' @importFrom dplyr select starts_with
#' @importFrom assertthat assert_that
#' @keywords internal
TransitionDTDiff = R6::R6Class("TransitionDTDiff",
                               private = list(
                                 nonprefixed.key = NULL,
                                 from.version.id = NULL,
                                 to.version.id = NULL,
                                 diff.DT = NULL
                               ),
                               public = list(
                                 initialize = function(key, from.version.id, to.version.id, diff.DT) {
                                   assert_that (is.data.table(diff.DT))
                                   ## ---
                                   private[["nonprefixed.key"]] <- key
                                   private[["from.version.id"]] <- from.version.id
                                   private[["to.version.id"]] <- to.version.id
                                   private[["diff.DT"]] <- diff.DT
                                 },
                                 print = function(...) {
                                   cat(paste0("<TransitionDTDiff> (R6 object)"))
                                   cat("\nTurns snapshot version ")
                                   cat(deparse(private[["from.version.id"]]))
                                   cat(" into snapshot version ")
                                   cat(deparse(private[["to.version.id"]]))
                                   cat("\nKey: ")
                                   cat(deparse(private[["nonprefixed.key"]]))
                                   cat("\nDiff entries:\n")
                                   print(private[["diff.DT"]], ...)
                                 },
                                 from_version_id = function() {
                                   private[["from.version.id"]]
                                 },
                                 to_version_id = function() {
                                   private[["to.version.id"]]
                                 },
                                 key = function() {
                                   private[["nonprefixed.key"]]
                                 },
                                 patch_snapshot = function(from.version.id, from.DT) {
                                   nonprefixed.key = private[["nonprefixed.key"]]
                                   prefixed.key = paste0("key.",nonprefixed.key)
                                   diff.DT = private[["diff.DT"]]
                                   ## ---
                                   assert_that (identical(private[["from.version.id"]], from.version.id))
                                   assert_that (is.data.table(from.DT))
                                   assert_that (
                                     private[["diff.DT"]][status=="added"] %>>%
                                     select(starts_with("key.")) %>>%
                                     setnames(which(startsWith(names(.), "key.")),
                                              function(prefixed.names) substring(prefixed.names, nchar("key.")+1L, nchar(prefixed.names))) %>>%
                                     {from.DT[., on=nonprefixed.key, nomatch=NULL]} %>>%
                                     nrow() %>>%
                                     `==`(0L),
                                     msg = 'diff does not apply to from.DT; there were already values for a key to be added'
                                   )
                                   assert_that (
                                     private[["diff.DT"]][status!="added"] %>>%
                                     select(starts_with("key."), starts_with("from.")) %>>%
                                     setnames(which(startsWith(names(.), "key.")),
                                              function(prefixed.names) substring(prefixed.names, nchar("key.")+1L, nchar(prefixed.names))) %>>%
                                     setnames(which(startsWith(names(.), "from.")),
                                              function(prefixed.names) substring(prefixed.names, nchar("from.")+1L, nchar(prefixed.names))) %>>%
                                     {.[!from.DT, on=names(from.DT)]} %>>%
                                     nrow() %>>%
                                     `==`(0L),
                                     msg = 'diff does not apply to from.DT; contents to be removed or changed did not match'
                                   )
                                   ## ---
                                   to.DT = from.DT %>>%
                                     `[`(
                                       !diff.DT,
                                       on=setNames(nm=nonprefixed.key, object=prefixed.key)
                                     ) %>>%
                                     rbind(
                                       diff.DT[status!="removed"] %>>%
                                       {cbind(
                                          select_substr_prefix_columns(., "key."),
                                          select_substr_prefix_columns(., "to.")
                                       )}
                                     )
                                   return (to.DT)
                                 },
                                 compose_with = function(later.diff) {
                                   assert_that (inherits(later.diff, "TransitionDTDiff"))
                                   assert_that (identical(private[["to.version.id"]], later.diff[["from_version_id"]]()))
                                   assert_that (identical(private[["nonprefixed.key"]], later.diff[["key"]]()))
                                   ## ---
                                   ## - add then add = invalid
                                   ## - add then change = add
                                   ## - add then remove = nothing
                                   ## - add the nothing = add
                                   ## - change then add = invalid
                                   ## - change then change = change or nothing
                                   ## - change then remove = remove
                                   ## - change then nothing = change
                                   ## - remove then add = change or nothing
                                   ## - remove then change = invalid
                                   ## - remove then remove = invalid
                                   ## - remove then nothing = remove
                                   ## - nothing then add = add
                                   ## - nothing then change = change
                                   ## - nothing then remove = remove
                                   ## - (nothing then nothing = nothing)
                                   ##
                                   ## FIXME finish
                                   ##
                                   ## NOTE that this cannot be achieve for a minimal removed/changed diff without the rest of the history.  Suggests tighter integration with the archiver.
                                   stop ('to implement')
                                 }
                                 ## TODO diff diffs
                               )
                               )

#' Object used to produce diffs between `data.table`s with certain columns treated similar to keys
#' @examples
#'
#' library("pipeR")
#'
#' DT1 = data.table::data.table(t = 1:5, v = 1:5)
#' DT2 = data.table::data.table(t = 2:6, v = c(2:4, 5.1, 6))
#'
#' TransitionDTDiffer$new("t")$diff("ver1", DT1, "ver2", DT2)
#'
#' TransitionDTDiffer$new("t")$diff("ver1", DT1, "ver2", DT2)$patch_snapshot("ver1", DT1) %>>% all.equal(DT2)
#'
#' @importFrom data.table is.data.table rbindlist setnames set setcolorder copy :=
#' @importFrom stats setNames
#' @importFrom assertthat assert_that
#' @importFrom rlang is_character is_named
#' @importFrom pipeR %>>%
#' @keywords internal
TransitionDTDiffer =
  R6::R6Class("TransitionDTDiffer",
              private = list(
                ## We need a key to identify which rows in a pair
                ## of DTs correspond to each other.
                nonprefixed.key = NULL
              ),
              public = list(
                initialize = function(key) {
                  assert_that (is_character(key))
                  assert_that (!is_named(key))
                  private[["nonprefixed.key"]] <- key
                },
                diff = function(from.version.id, from.DT, to.version.id, to.DT) {
                  ## input validation
                  assert_that (is.data.table(from.DT))
                  assert_that (is.data.table(to.DT))
                  ## TODO want stricter than rbind compatibility... check colname set.  maybe check for same order?
                  stop_if_not_rbind_coltype_compatible (from.DT, to.DT)
                  ## -----
                  nonprefixed.key = private[["nonprefixed.key"]]
                  diff.DT = rbindlist(
                    list(
                      ## Removed rows, as they appeared in from.DT:
                      from.DT[!to.DT, on=private[["nonprefixed.key"]]] %>>%
                      ##   `setnames` modifies by reference, but is still
                      ##   chain-friendly (it'd be easier to just `setnames` the
                      ##   original `from.DT` and `to.DT` but that would modify
                      ##   the inputs)
                      setnames(nonprefixed.key,
                               function(nonprefixed.colnames) paste0("key.",nonprefixed.colnames)) %>>%
                      setnames(setdiff(colnames(from.DT), nonprefixed.key),
                               function(nonprefixed.colnames) paste0("from.",nonprefixed.colnames)) %>>%
                      ##   fill in to.* columns to show the transition; special
                      ##   treatment here is to add NAs of appropriate types
                      ##   without having to perform another join:
                      set(, paste0("to.",setdiff(colnames(to.DT), nonprefixed.key)),
                          to.DT
                          [integer(0L), setdiff(colnames(to.DT), nonprefixed.key), with=FALSE]
                          [seq_len(nrow(.))]) %>>%
                      set(, "status", "removed"),
                      ## Added or changed rows, as they appeared in to.DT:
                      to.DT[!from.DT, on=colnames(to.DT)] %>>%
                      setnames(nonprefixed.key,
                               function(nonprefixed.colnames) paste0("key.",nonprefixed.colnames)) %>>%
                      setnames(setdiff(colnames(from.DT), nonprefixed.key),
                               function(nonprefixed.colnames) paste0("to.",nonprefixed.colnames)) %>>%
                      ##   fill in from.* columns to show the transition:
                      (added.or.changed.DT ~
                        ## inefficiently copy `from.DT` so we can tag all the
                        ## things to have "changed" status before joining and
                        ## losing the membership-in-`from.DT` information
                        from.DT %>>%
                        copy() %>>%
                        setnames(nonprefixed.key,
                                 function(nonprefixed.colnames) paste0("key.",nonprefixed.colnames)) %>>%
                        setnames(setdiff(names(from.DT), nonprefixed.key),
                                 function(nonprefixed.names) paste0("from.",nonprefixed.names)) %>>%
                        set(, "status", "changed") %>>%
                        {( # <-- bracket for pipe, parenthesis for "bracket chain"
                          .
                          [added.or.changed.DT, on=paste0("key.",nonprefixed.key), nomatch=NA]
                          [is.na(status), status := "added"]
                        )}
                      )
                    ),
                    fill=TRUE,
                    use.names=TRUE
                  ) %>>%
                    ## place status and (prefixed) key columns first:
                    setcolorder(c("status", paste0("key.",nonprefixed.key)))
                  return (TransitionDTDiff[["new"]](nonprefixed.key, from.version.id, to.version.id, diff.DT))
                }
              ))
