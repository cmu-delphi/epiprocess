# Epitooling Development Guide

This guide is more of a collection of what we've learned while building our toolset than a formal manual. While we follow the conventions of the [posit/tidyverse](https://www.tidyverse.org/) and [tidymodels](https://www.tidymodels.org/) ecosystems for programming in R, we've also adopted some specific practices and shared wisdom from our conversations and comments during PR reviews. As a result, we've compiled these practices and sources in one place.

## Table of Contents
- [Epitooling Development Guide](#epitooling-development-guide)
  - [Table of Contents](#table-of-contents)
  - [R Development](#r-development)
    - [Important References](#important-references)
    - [R Epitooling Data Structures](#r-epitooling-data-structures)
    - [Data Standardization \& Column Ordering](#data-standardization--column-ordering)
    - [Panel Data Implementation Patterns](#panel-data-implementation-patterns)
    - [Infrastructure \& Supporting Packages](#infrastructure--supporting-packages)
      - [rlang](#rlang)
      - [vctrs](#vctrs)
      - [tidyselect](#tidyselect)
      - [cli](#cli)
      - [checkmate](#checkmate)
      - [withr](#withr)
      - [testthat](#testthat)
      - [lifecycle](#lifecycle)
      - [roxygen2](#roxygen2)
        - [Documentation Resilience \& Reuse](#documentation-resilience--reuse)
        - [Organizing Method Pages](#organizing-method-pages)
        - [Complex Formatting Advice](#complex-formatting-advice)
      - [lintr](#lintr)
    - [Developing the Code \& Local Environment](#developing-the-code--local-environment)
      - [Validating the Package Locally](#validating-the-package-locally)
      - [Developing the Documentation Site](#developing-the-documentation-site)
    - [CI/CD \& Local Debugging](#cicd--local-debugging)
    - [Versioning \& Releases](#versioning--releases)
      - [Versioning](#versioning)
      - [The CRAN Submission Checklist](#the-cran-submission-checklist)
  - [Python Development](#python-development)
  - [Common Workflows](#common-workflows)
    - [Code Review Guidelines](#code-review-guidelines)
      - [Review Philosophy](#review-philosophy)
      - [Review Process](#review-process)
        - [For Authors](#for-authors)
        - [For Reviewers](#for-reviewers)
      - [Integration with Conventional Comments](#integration-with-conventional-comments)
        - [Decorations](#decorations)

---

## R Development

### Important References

Here are some resources that help to improve the quality of our code and better understand how to build a robust package. 

- [R Package Tricks](https://dajmcdon.github.io/other/rpkg-tricks.html). Practical collection of functions we frequently use when building a package.
- [Advanced R](https://adv-r.hadley.nz). Guide to understanding how R operates under the hood.
- [R Packages](https://r-pkgs.org). Manual for building, testing, and maintaining R packages.
- [Tidyverse Design Guide](https://design.tidyverse.org/). The principles we lean on for consistent package development.
  - We specifically recommend paying attention to the [dots prefix guide](https://design.tidyverse.org/dots-prefix.html) if you're writing functions that take `...`.
- [Version Control](https://git-scm.com/book/en/v2). Official guide on version control. 
  - Also, it is available this refresher [course from Dr. McDonald](https://ubc-stat.github.io/stat-550/schedule/slides/git.html#/section) that covers the essentials.

### R Epitooling Data Structures
We use a couple of specific structures to deal with panel data.

- `epi_df`. A snapshot of epi data. It must contain `geo_value` and `time_value`.
- `epi_archive`. A full version history of data containing all updates to past values.

### Data Standardization & Column Ordering
To keep things consistent across all our APIs and internal code, we expect columns strictly flowing the following order:
1. `geo_value` (geo type)
2. `other_keys` (e.g., age bracket, race)
3. `time_value` (time type)
4. `version` (or `as_of` / `issue`)
5. Measured variables (indicators / values)

### Panel Data Implementation Patterns
- Ensure that data objects are backed by a `tibble` or `data.table` to prevent dimensions from being dropped unexpectedly. Don't use `drop = FALSE` in subsetting operations.
- Ensure windows are structured to be "complete" before the function is applied.
- Ensure data is properly grouped by `geo_value` and any `other_keys` before passing into sliding, scaling, or transformation functions.

### Infrastructure & Supporting Packages
Here some packages that we use frequently:

#### rlang
The `rlang` package is fundamental for [metaprogramming](https://rlang.r-lib.org/reference/topic-metaprogramming.html), safely [defusing arguments](https://rlang.r-lib.org/reference/topic-defuse.html), and [data masking](https://rlang.r-lib.org/reference/topic-data-mask-programming.html). Also, we use its [condition formatting](https://rlang.r-lib.org/reference/topic-condition-formatting.html) (e.g., `rlang::warn()`, `rlang::abort()`) instead of standard `message()` or `stop()` to give users structured, beautiful console output.

- We use `rlang::enquo()` / `rlang::enquos()` to capture user input as quosures and `!!` / `!!!` to inject them into later calls. Use `rlang::inject()` when you need to splash a list of parameters into a function call (like `stats::smooth.spline()`).
- Use `rlang::arg_match()` to validate string arguments against allowed values; it provides much better error messaging than base `match.arg()`.
- Use `rlang::caller_arg()` to capture the expression passed to a function, allowing us to quote the specific variable name in error messages.
- Use `rlang::sym()` / `rlang::syms()` to convert strings to symbols for programmatic call construction, and `rlang::as_name()` / `rlang::as_label()` to convert expressions to strings for display or indexing.
- Use `rlang::is_integerish()` for precise numeric checks (e.g., checking for whole numbers stored as doubles) and `rlang::check_dots_empty()` to future-proof functions that don't yet use `...`.
- Use `rlang::abort(..., class = "epiprocess__error_class")` if you need to throw a structured error that can be caught programmatically.
- Some of Delphi's packages include small "fragments" or standalone versions of `rlang` code internally. This helps us avoid heavy external dependencies for core logic and ensures that our internal tools (like special error handlers) remain stable even if the user has a different version of `rlang` installed.

#### vctrs
Provides vector operations and [custom S3 vector classes](https://vctrs.r-lib.org/articles/s3-vector.html) (using `vctrs::new_vctr()`) and formatting in tibbles via [pillar](https://vctrs.r-lib.org/articles/pillar.html). We rely on it for the following uses.

- `vec_data()` and `vec_restore()` are used to preserve `epi_df` metadata during manipulations.
- `vec_rbind()`, `vec_cast()`, and `vec_recycle_common()` ensure consistent data types, especially when padding rows in `epi_slide()`.
- `vec_order()`, `vec_slice()`, and `vec_rep()` provide fast vector operations.
- `vec_equal()` and `vec_in()` handle robust comparisons, including `NA` handling in compactification.
- `vec_set_intersect()`, `vec_set_union()`, and `vec_set_difference()` are used for robustly managing sets of column names or keys, ensuring we handle duplicate or missing values consistently.

#### tidyselect
If your function accepts column names, implement [tidy selection syntax](https://tidyselect.r-lib.org/articles/syntax.html) so it feels native to users. This allows users to use helpers like `starts_with()` or `any_of()`.

#### cli
The [cli package](https://cli.r-lib.org/index.html) is the preferred tool for the UI/UX of messages. Paired with `rlang`, it allows us to create rich formatting (e.g., using `{.var}`, `{.code}`, and `{.fn}`). Use `cli::cli_abort()`, `cli::cli_warn()`, and `cli::cli_inform()` to communicate with users.

When dealing with pluralization (e.g., "Deleted 1 file" vs "Deleted 2 files"), follow the [cli pluralization guide](https://cli.r-lib.org/articles/pluralization.html) to keep messages grammatically correct without complex logic in your code.

#### checkmate
The `checkmate` package is used throughout our toolset to provide informative input validation.

- Almost every user-facing function starts with a series of `assert_*` calls to validate that arguments meet the expected criteria (type, class, length, range).
  - Example: `checkmate::assert_integerish(k, lower = 0, len = 1)`
- `test_*` functions are used within `if` statements to branch logic based on the properties of an object without triggering an immediate error.
  - Example: `if (checkmate::test_numeric(params$lambda, min.len = 2L)) { ... }`
- For specialized epi-logic, we implement custom `check_` and `test_` functions (often in `utils.R`) that follow the `checkmate` API style.
  - Example: `check_ukey_unique()` validates that a combination of columns forms a unique key, providing a high-quality error message via `cli` if duplicates are found.
- Large-scale validations often use `assert(check_...)` to combine custom logic with standard `checkmate` error triggering.
  - Example: `assert(check_ukey_unique(x, c("geo_value", other_keys, "time_value")))`
- We use it to validate internal object states and essential metadata.
  - Example: `assert_class(x, "epi_df")` or `assert_subset(names, expected_names)`

#### withr
Used to manage temporary state changes cleanly. In tests, we use `withr::local_options()` or `withr::local_envvar()` to ensure state doesn't leak between test blocks. It is also useful in package code to temporarily change options or environment variables. Always control env/state changes explicitly during `testthat` blocks using `withr::local_*` (like `local_options` or `local_envvar`).

#### testthat
For testing purposes, we use `testthat`. It is used to validate that our code is working as expected.

- We use [`testthat` snapshots](https://testthat.r-lib.org/articles/snapshotting.html) (stored in `tests/testthat/_snaps/`) to ensure stability across our packages in the following cases:
  - We snapshot `cli` messages and layouts to catch errors in complex outputs.
  - We use `expect_snapshot_value(style = "json2")` to guard the boundary between R and the Epidata API, ensuring request configurations are correctly serialized.
  - We use `expect_snapshot_value(style = "deparse")` for numerical and structural stability of forecasters (prediction tibbles and summaries).
- Use `expect_s3_class()`, `expect_length()`, and `expect_named()` instead of over-relying on generic `expect_equal()`.
- If you're hit an external API, mock it. We use `local_mocked_bindings()` to keep our testing pipelines isolated and deterministic. More information can be found [here](https://testthat.r-lib.org/articles/mocking.html).

#### lifecycle
Used to manage the [lifecycle](https://lifecycle.r-lib.org/articles/lifecycle.html) of functions and arguments. 

Here's a deprecation workflow:
1. First, check `DESCRIPTION` for our upcoming version.
2. Add `lifecycle::deprecate_warn()` in the function body.
3. Keep our CI green by silencing that warning in legacy tests using `withr::local_options(lifecycle_verbosity = "quiet")`.
4. Write a new specific test that verifies your exact deprecation warning snapshot.
5. In `roxygen2`, add the `` `r lifecycle::badge("deprecated")` `` tag and provide clear `# Old:` vs `# New:` examples. It saves users a lot of grief.

#### roxygen2
We make extensive use of `roxygen2` features across the packages. Here is important to point out that we try to reduce the maintenance burden of our documentation by avoiding repeateing parameters and other features in the documentation.

##### Documentation Resilience & Reuse
- Use `@inheritParams` to copy documentation from another function.
  - Use a hidden documentation anchor (like `.epidatr_shared_params` in `epidatr`) to centralize common parameters.
  - Inherit from dependencies using the `@inheritParams pkg::fun` syntax (e.g., `epipredict` inherits from `recipes`).
  - Use `@inheritSection` to reuse complex methodology or data caveat explanations.
- Use inline R expressions to inject documentation fragments. We store common descriptions (like `tidyselect` semantics) as strings in `R/inline-roxygen.R`.
  - *Example*: `#' @param sum_cols `r tidyselect_arg_roxygen``

##### Organizing Method Pages
- Use `@rdname` to send multiple related functions or methods to the same `.Rd` file (e.g., `plot.epi_df` and `plot.epi_archive`).
- Use `@describeIn` to document methods for a generic, keeping the main generic page as the focus.
  - *Example*: `#' @describeIn epi_df Lower-level constructor for epi_df object`

##### Complex Formatting Advice
- Always ensure `Roxygen: list(markdown = TRUE)` is in your `DESCRIPTION`.
- Use `[pkg::fun()]` for automatic function linking and `{.cls epi_df}` for consistent class styling.
- Ensure there is a blank line before starting nested lists to prevent layout issues in the `.Rd` output.
- You can use inline R to conditionally add content: `r if (is_pkg_installed("plotly")) "..."`.

For more details, check the articles on [Cross-referencing](https://roxygen2.r-lib.org/articles/index-crossref.html) and [Reusing Documentation](https://roxygen2.r-lib.org/articles/reuse.html).

#### lintr
We use the [lintr package](https://lintr.r-lib.org) to maintain our style consistency.

- Each package includes a [`.lintr` file](https://lintr.r-lib.org/articles/lintr.html) that defines the specific linters we use. If the linter yells at your PR, check this file to understand the rules.
- Some standard linting checks are not relevant for Delphi packages and are disabled or modified in our configuration:
  - `return_linter`: Often disabled to allow for more flexible function exits.
  - `pipe_consistency_linter`: Disabled to avoid enforcing a specific pipe style.
  - `line_length_linter(...)`: We allow lines up to a certain number of characters to accommodate long data-processing chains.
  - `object_length_linter(length = 40L)`: We allow longer object names to ensure they are descriptive.

### Developing the Code & Local Environment

Use the following R code to set up your environment.
```r
install.packages(c('devtools', 'pkgdown', 'styler', 'lintr', 'pak')) # install dev dependencies
pak::pkg_install(".") # install package and dependencies
```

#### Validating the Package Locally
Before you open a PR, make sure to run these locally. It saves a lot of back-and-forth on GitHub Actions:

```r
styler::style_pkg() # format code
lintr::lint_package() # lint code

devtools::check() # run R CMD check, which runs everything below
devtools::document() # generate package meta data and man files
devtools::test() # test package
devtools::build_vignettes() # build vignettes only
devtools::run_examples() # run doc examples
devtools::check(vignettes = FALSE) # check package without vignettes
```

#### Developing the Documentation Site
Commands for developing the documentation site:

```sh
# Basic build and preview
R -e 'pkgdown::clean_site()'
R -e 'devtools::document()'
R -e 'pkgdown::build_site()'

# A smart rebuild workflow for non-RStudio users.
# You may need to first build the site.
R -e 'pkgdown::build_site(".", examples = FALSE, devel = TRUE, preview = FALSE)'
R -e 'renv::install("servr")'
# Will start a local docs server and monitor for changes.
Rscript inst/pkgdown-watch.R
```

### CI/CD & Local Debugging
 `dev` branch checks are pretty comprehensive and exist purely to safeguard the main branch.

- A PR to `dev` will automatically trigger testing, linting, and docs checks via GitHub Actions.
- When things break, look closely at the Action logs—they'll almost always point to the specific step that crashed.
- To debug R package issues, `devtools::check()` is your best friend. If it passes there, it usually passes in CI.
- If it’s a style issue, run `lintr::lint_package()` locally. 
- If a vignette fails on the build site, make sure `devtools::document()` and `pkgdown::build_site()` are executing cleanly on your machine.

### Versioning & Releases

#### Versioning
Please follow the guidelines in the [PR template document](.github/pull_request_template.md).

#### The CRAN Submission Checklist

Open a release issue and then copy and follow this checklist in the issue (modified from the checklist generated by `usethis::use_release_issue(version = "1.0.2")`):

- [ ] `git pull` on `dev` branch.
- [ ] Make sure all changes are committed and pushed.
- [ ] Check [current CRAN check results](https://cran.rstudio.org/web/checks/check_results_epiprocess.html).
- [ ] `devtools::check(".", manual = TRUE, env_vars = c(NOT_CRAN = "false"))`.
  - Aim for 10/10, no notes.
- [ ] If check works well enough, merge to main. Otherwise open a PR to fix up.
- [ ] [Polish NEWS](https://github.com/cmu-delphi/epiprocess/blob/dev/NEWS.md).
  - Some [guidelines](https://style.tidyverse.org/news.html#news-release).
- [ ] `git checkout main`
- [ ] `git pull`
- [ ] `urlchecker::url_check()`.
  - This may choke on the MIT license url, and that's ok.
- [ ] `devtools::build_readme()`
- [ ] `devtools::check_win_devel()`
- [ ] Have maintainer ("cre" in description) check email for problems.
- [ ] `revdepcheck::revdep_check(num_workers = 4)`.
  - This may choke, it is very sensitive to the binary versions of packages on a given system. Either bypass or ask someone else to run it if you're concerned.
- [ ] Update `cran-comments.md`
- [ ] PR with any changes (and go through the list again) into `dev` and run through the list again.

Submit to CRAN:

- [ ] `devtools::submit_cran()`.
- [ ] Maintainer approves email.

Wait for CRAN...

- [ ] If accepted :tada:, move to next steps. If rejected, fix and resubmit.
- [ ] Open and merge a PR containing any updates made to `main` back to `dev`.
- [ ] `usethis::use_github_release(publish = FALSE)` (publish off, otherwise it won't push) will create a draft release based on the commit hash in CRAN-SUBMISSION and push a tag to the GitHub repo.
- [ ] Go to the repo, verify the release notes, and publish when ready.
---

## Python Development

When working on Python packages, here are some resources. Although some focus on pipeline development, they are still useful for general Python development in Delphi.

- [Indicator Dev Guide](https://github.com/cmu-delphi/covidcast-indicators/blob/main/_template_python/INDICATOR_DEV_GUIDE.md). If you're building indicators, start with this guide. It lays out the exact template and structure we expect.
- [Delphi Epidata Development](https://github.com/cmu-delphi/delphi-epidata/blob/dev/docs/epidata_development.md). For backend contributions or core client work, check here.
- `EpiDataContext`. Treat this as the central nervous system for `epidatpy` data retrieval and context management.

---

## Common Workflows

### Code Review Guidelines
A general guideline for code review is available in our [Internal Google Doc](https://docs.google.com/document/d/1ZQXFHKlDrAeXwjwIq0jM0y24a_wZqgiFOjI-c_PMGts/edit?tab=t.0). Some relevant points below. 

#### Review Philosophy

- A reviewer should ask questions about code that isn't clear
- Compliment things done well to build a common understanding of standards.
- Ensure code and comments make it easy for future readers to understand the "why" and "how".
- Code reviews ensure that at least two people understand every part of the codebase.

#### Review Process

##### For Authors
- Explain the problem solved and link to relevant GitHub Issues.
- Include sample output if the PR changes any user-facing or data output.
- Choose at least one experienced individual.

##### For Reviewers
- Verify that the code actually works as intended.
- Ensure the PR matches its description and doesn't include out-of-scope changes.
- Aim for uniformity and clarity.
- Approve: Mergable state that improves overall code health, even if not perfect.
- Reject: Rare; only for major misunderstandings or shifts in priority.

#### Integration with Conventional Comments

Delphi uses [Conventional Comments](https://conventionalcomments.org/) to clarify intent. Standard labels include:

- `question`: Clarifications.
- `praise`: Positive reinforcement.
- `issue`/`todo`: Requested changes (typically blocking).
- `suggestion`/`nitpick`: Requested changes (typically non-blocking).

##### Decorations
- `(blocking)`: Must be addressed.
- `(non-blocking)`: Optional suggestion.
- `(if-minor)`: Address if simple.
