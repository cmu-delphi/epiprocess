#' Subset of JHU daily state cases and deaths 
#'
#' This data source of confirmed COVID-19 cases and deaths
#' is based on reports made available by the Center for
#' Systems Science and Engineering at Johns Hopkins University.
#' This example data ranges from Mar 1, 2020 to Dec 31, 2021, and is limited to 
#' California, Florida, Texas, New York, Georgia, and Pennsylvania.
#'
#' @format A tibble with 4026 rows and 6 variables:
#' \describe{
#'   \item{geo_value}{the geographic value associated with each row 
#'       of measurements.}
#'   \item{time_value}{the time value associated with each row of measurements.}
#'   \item{case_rate_7d_av}{7-day average signal of number of new 
#'       confirmed COVID-19 cases per 100,000 population, daily}
#'   \item{death_rate_7d_av}{7-day average signal of number of new confirmed 
#'       deaths due to COVID-19 per 100,000 population, daily}
#'   \item{cases}{Number of new confirmed COVID-19 cases, daily}
#'   \item{cases_7d_av}{7-day average signal of number of new confirmed 
#'       COVID-19 cases, daily}
#' }
#' @source This object contains a modified part of the 
#'   \href{https://github.com/CSSEGISandData/COVID-19}{COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University}
#'   as \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html}{republished in the COVIDcast Epidata API}. 
#'   This data set is licensed under the terms of the
#'   \href{https://creativecommons.org/licenses/by/4.0/}{Creative Commons Attribution 4.0 International license}
#'   by the Johns Hopkins University on behalf of its Center for Systems Science
#'   in Engineering. Copyright Johns Hopkins University 2020.
#'
#' Modifications:
#' * \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html}{From the COVIDcast Epidata API}: 
#'   These signals are taken directly from the JHU CSSE 
#'   \href{https://github.com/CSSEGISandData/COVID-19}{COVID-19 GitHub repository} 
#'   without changes. The 7-day average signals are computed by Delphi by 
#'   calculating moving averages of the preceding 7 days, so the signal for 
#'   June 7 is the average of the underlying data for June 1 through 7, 
#'   inclusive.
#' * Furthermore, the data has been limited to a very small number of rows, 
#'   the signal names slightly altered, and formatted into a tibble.
"jhu_csse_daily_subset"


#' Subset of daily doctor visits and cases in archive format
#'
#' This data source is based on information about outpatient visits,
#' provided to us by health system partners, and also contains confirmed
#' COVID-19 cases based on reports made available by the Center for
#' Systems Science and Engineering at Johns Hopkins University.
#' This example data ranges from June 1, 2020 to Dec 1, 2021, and 
#' is also limited to California, Florida, Texas, and New York.
#'
#' @format An `epi_archive` data format. The data table DT has 129,638 rows and 5 columns:
#' \describe{
#'   \item{geo_value}{the geographic value associated with each row of measurements.}
#'   \item{time_value}{the time value associated with each row of measurements.}
#'   \item{version}{the time value specifying the version for each row of measurements. }
#'   \item{percent_cli}{percentage of doctor’s visits with CLI (COVID-like illness) computed from medical insurance claims}
#'   \item{case_rate_7d_av}{7-day average signal of number of new confirmed deaths due to COVID-19 per 100,000 population, daily}
#' }
#' @source
#' This object contains a modified part of the \href{https://github.com/CSSEGISandData/COVID-19}{COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University} as \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html}{republished in the COVIDcast Epidata API}. This data set is licensed under the terms of the
#' \href{https://creativecommons.org/licenses/by/4.0/}{Creative Commons Attribution 4.0 International license}
#' by Johns Hopkins University on behalf of its Center for Systems Science in Engineering.
#' Copyright Johns Hopkins University 2020.
#'
#' Modifications:
#' * \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/doctor-visits.html}{From the COVIDcast Doctor Visits API}: The signal `percent_cli` is taken directly from the API without changes.
#' * \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html}{From the COVIDcast Epidata API}: `case_rate_7d_av` signal was computed by Delphi from the original JHU-CSSE data by calculating moving averages of the preceding 7 days, so the signal for June 7 is the average of the underlying data for June 1 through 7, inclusive.
#' * Furthermore, the data is a subset of the full dataset, the signal names slightly altered, and formatted into a tibble.
#'
#' @export
"archive_cases_dv_subset"

#' Detect whether `pkgload` is unregistering a package (with some unlikely false positives)
#'
#' More precisely, detects the presence of a call to an `unregister` or
#' `unregister_namespace` function from any package in the indicated part of the
#' function call stack.
#'
#' @param parent_n optional, single non-`NA` non-negative integer; how many
#'   "parent"/"ancestor" calls should we skip inspecting? Default of `0L` will
#'   check everything up to, but not including the call to this function. If
#'   building wrappers or utilities around this function it may be useful to use
#'   this default to ignore those wrappers, especially if they might trigger
#'   false positives now or in some future version of this function with a looser
#'   function name test.
#'
#' @return Boolean
#'
#' @noRd
some_package_is_being_unregistered = function(parent_n = 0L) {
  calls = sys.calls()
  # `calls` will include the call to this function; strip out this call plus
  # `parent_n` additional requested calls to make it like we're reasoning about
  # the desired call. This could prevent potential false positives from
  # triggering if, in a later version, we decide to loosen the `call_name`
  # checks below to something that would be `TRUE` for the name of this function
  # or one of the undesired call ancestors.
  calls_to_inspect = utils::head(calls, n = -(parent_n + 1L))
  # Note that `utils::head(sys.calls(), n=-1L)` isn't equivalent, due to lazy
  # argument evaluation. Note that copy-pasting the body of this function
  # without this `utils::head` operation isn't always equivalent to calling it;
  # e.g., within the `value` argument of a package-level `delayedAssign`,
  # `sys.calls()` will return `NULL` is some or all cases, including when its
  # evaluation has been triggered via `unregister`.
  simple_call_names = purrr::map_chr(calls_to_inspect, function(call) {
    maybe_simple_call_name = rlang::call_name(call)
    if (is.null(maybe_simple_call_name)) NA_character_ else maybe_simple_call_name
  })
  # `pkgload::unregister` is an (the?) exported function that forces
  # package-level promises, while `pkgload:::unregister_namespace` is the
  # internal function that does this package-level promise. Check for both just
  # in case there's another exported function that calls `unregister_namespace`
  # or other `pkgload` versions don't use the `unregister_namespace` internal.
  # (Note that `NA_character_ %in% <table not containing NA>` is `FALSE` rather
  # than `NA`, giving the desired semantics and avoiding potential `NA`s in the
  # argument to `any`.)
  any(simple_call_names %in% c("unregister", "unregister_namespace"))
}

#' [`base::delayedAssign`] with [`pkgload::unregister`] awareness, injection support
#'
#' Provides better feedback on errors during promise evaluation while a package
#' is being unregistered, to help package developers escape from a situation
#' where a buggy promise prevents package reloading. Also provide `rlang`
#' injection support (like [`rlang::env_bind_lazy`]). The call stack will look
#' different than when using `delayedAssign` directly.
#'
#' @noRd
delayed_assign_with_unregister_awareness = function(x, value,
                                                    eval.env = rlang::caller_env(),
                                                    assign.env = rlang::caller_env()) {
  value_quosure = rlang::as_quosure(rlang::enexpr(value), eval.env)
  this_env = environment()
  delayedAssign(x, eval.env = this_env, assign.env = assign.env, value = {
    if (some_package_is_being_unregistered()) {
      withCallingHandlers(
        # `rlang::eval_tidy(value_quosure)` is shorter and would sort of work,
        # but doesn't give the same `ls`, `rm`, and top-level `<-` behavior as
        # we'd have with `delayedAssign`; it doesn't seem to actually evaluate
        # quosure's expr in the quosure's env. Using `rlang::eval_bare` instead
        # seems to do the trick. (We also could have just used a `value_expr`
        # and `eval.env` together rather than introducing `value_quosure` at
        # all.)
        rlang::eval_bare(rlang::quo_get_expr(value_quosure), rlang::quo_get_env(value_quosure)),
        error = function(err) {
          Abort(paste("An error was raised while attempting to evaluate a promise",
                      "(prepared with `delayed_assign_with_unregister_awareness`)",
                      "while an `unregister` or `unregister_namespace` call",
                      "was being evaluated.",
                      "This can happen, for example, when `devtools::load_all`",
                      "reloads a package that contains a buggy promise,",
                      "because reloading can cause old package-level promises to",
                      "be forced via `pkgload::unregister` and",
                      "`pkgload:::unregister_namespace`, due to",
                      "https://github.com/r-lib/pkgload/pull/157.",
                      "If this is the current situation, you might be able to",
                      "be successfully reload the package again after",
                      "`unloadNamespace`-ing it (but this situation will",
                      "keep re-occurring every other `devtools::load`",
                      "and every `devtools:document` until the bug or situation",
                      "generating the promise's error has been resolved)."
                      ),
                class = "epiprocess__promise_evaluation_error_during_unregister",
                parent = err)
        })
    } else {
      rlang::eval_bare(rlang::quo_get_expr(value_quosure), rlang::quo_get_env(value_quosure))
    }
  })
}

# Like normal data objects, set `archive_cases_dv_subset` up as a promise, so it
# doesn't take unnecessary space before it's evaluated. This also avoids a need
# for @include tags. However, this pattern will use unnecessary space after this
# promise is evaluated, because `as_epi_archive` clones `archive_cases_dv_subset_dt`
# and `archive_cases_dv_subset_dt` will stick around along with `archive_cases_dv_subset`
# after they have been evaluated. We may want to add an option to avoid cloning
# in `as_epi_archive` and make use of it here. But we may also want to change
# this into an active binding that clones every time, unless we can hide the
# `DT` field from the user (make it non-`public` in general) or make it
# read-only (in this specific case), so that the user cannot modify the `DT`
# here and potentially mess up examples that they refer to later on.
#
# During development, note that reloading the package and re-evaluating this
# promise should prepare the archive from the DT using any changes that have
# been made to `as_epi_archive`; however, if earlier, any field of
# `archive_cases_dv_subset` was modified using `<-`, a global environment
# binding may have been created with the same name as the package promise, and
# this binding will stick around even when the package is reloaded, and will
# need to be `rm`-d to easily access the refreshed package promise.
delayed_assign_with_unregister_awareness("archive_cases_dv_subset", as_epi_archive(archive_cases_dv_subset_dt, compactify=FALSE))

#' Subset of JHU daily cases from California and Florida
#'
#' This data source of confirmed COVID-19 cases
#' is based on reports made available by the Center for
#' Systems Science and Engineering at Johns Hopkins University.
#' This example data is a snapshot as of Oct 28, 2021 and captures the cases 
#' from June 1, 2020 to May 31, 2021
#' and is limited to California and Florida.
#'
#' @format A tibble with 730 rows and 3 variables:
#' \describe{
#'   \item{geo_value}{the geographic value associated with each row of measurements.}
#'   \item{time_value}{the time value associated with each row of measurements.}
#'   \item{cases}{Number of new confirmed COVID-19 cases, daily}
#' }
#' @source This object contains a modified part of the \href{https://github.com/CSSEGISandData/COVID-19}{COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University} as \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html}{republished in the COVIDcast Epidata API}. This data set is licensed under the terms of the
#' \href{https://creativecommons.org/licenses/by/4.0/}{Creative Commons Attribution 4.0 International license}
#' by the Johns Hopkins University on behalf of its Center for Systems Science in Engineering.
#' Copyright Johns Hopkins University 2020.
#'
#' Modifications:
#' * \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html}{From the COVIDcast Epidata API}:
#' These signals are taken directly from the JHU CSSE \href{https://github.com/CSSEGISandData/COVID-19}{COVID-19 GitHub repository} without changes.
#' * Furthermore, the data has been limited to a very small number of rows, the signal names slightly altered, and formatted into a tibble.
"incidence_num_outlier_example"

#' Subset of JHU daily cases from counties in Massachusetts and Vermont
#'
#' This data source of confirmed COVID-19 cases and deaths
#' is based on reports made available by the Center for
#' Systems Science and Engineering at Johns Hopkins University.
#' This example data ranges from Mar 1, 2020 to Dec 31, 2021, 
#' and is limited to Massachusetts and Vermont.
#'
#' @format A tibble with 16,212 rows and 5 variables:
#' \describe{
#'   \item{geo_value}{the geographic value associated with each row of measurements.}
#'   \item{time_value}{the time value associated with each row of measurements.}
#'   \item{cases}{Number of new confirmed COVID-19 cases, daily}
#'   \item{county_name}{the name of the county}
#'   \item{state_name}{the full name of the state}
#' }
#' @source This object contains a modified part of the \href{https://github.com/CSSEGISandData/COVID-19}{COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University} as \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html}{republished in the COVIDcast Epidata API}. This data set is licensed under the terms of the
#' \href{https://creativecommons.org/licenses/by/4.0/}{Creative Commons Attribution 4.0 International license}
#' by the Johns Hopkins University on behalf of its Center for Systems Science in Engineering.
#' Copyright Johns Hopkins University 2020.
#'
#' Modifications:
#' * \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html}{From the COVIDcast Epidata API}: These signals are taken directly from the JHU CSSE \href{https://github.com/CSSEGISandData/COVID-19}{COVID-19 GitHub repository} without changes. The 7-day average signals are computed by Delphi by calculating moving averages of the preceding 7 days, so the signal for June 7 is the average of the underlying data for June 1 through 7, inclusive.
#' * Furthermore, the data has been limited to a very small number of rows, the signal names slightly altered, and formatted into a tibble.
"jhu_csse_county_level_subset"
