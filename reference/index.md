# Package index

## `epi_df` basics

Constructors and information for `epi_df` objects.

- [`as_epi_df()`](https://cmu-delphi.github.io/epiprocess/reference/epi_df.md)
  [`is_epi_df()`](https://cmu-delphi.github.io/epiprocess/reference/epi_df.md)
  [`new_epi_df()`](https://cmu-delphi.github.io/epiprocess/reference/epi_df.md)
  :

  Test for `epi_df` format

- [`group_epi_df()`](https://cmu-delphi.github.io/epiprocess/reference/group_epi_df.md)
  :

  Group an `epi_df` object by default keys

## `epi_df` manipulation

Functions operating on `epi_df` objects.

- [`complete(`*`<epi_df>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/complete.epi_df.md)
  :

  "Complete" an `epi_df`, adding missing rows and/or replacing `NA`s

- [`epi_slide()`](https://cmu-delphi.github.io/epiprocess/reference/epi_slide.md)
  :

  More general form of `epi_slide_opt` for rolling/running computations

- [`epi_slide_opt()`](https://cmu-delphi.github.io/epiprocess/reference/epi_slide_opt.md)
  [`epi_slide_mean()`](https://cmu-delphi.github.io/epiprocess/reference/epi_slide_opt.md)
  [`epi_slide_sum()`](https://cmu-delphi.github.io/epiprocess/reference/epi_slide_opt.md)
  : Calculate rolling or running means, sums, etc., or custom
  calculations

- [`sum_groups_epi_df()`](https://cmu-delphi.github.io/epiprocess/reference/sum_groups_epi_df.md)
  :

  Aggregate an `epi_df` object

- [`epi_cor()`](https://cmu-delphi.github.io/epiprocess/reference/epi_cor.md)
  :

  Compute correlations between variables in an `epi_df` object

- [`detect_outlr()`](https://cmu-delphi.github.io/epiprocess/reference/detect_outlr.md)
  [`detect_outlr_rm()`](https://cmu-delphi.github.io/epiprocess/reference/detect_outlr.md)
  [`detect_outlr_stl()`](https://cmu-delphi.github.io/epiprocess/reference/detect_outlr.md)
  : Detect outliers

- [`growth_rate()`](https://cmu-delphi.github.io/epiprocess/reference/growth_rate.md)
  : Estimate growth rate

- [`growth_rate_params()`](https://cmu-delphi.github.io/epiprocess/reference/growth_rate_params.md)
  : Optional parameters for growth rate methods

- [`as_tibble(`*`<epi_df>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/as_tibble.epi_df.md)
  : Convert to tibble

- [`as_tsibble(`*`<epi_df>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/as_tsibble.epi_df.md)
  : Convert to tsibble format

## `epi_archive` basics

Constructors and information for `epi_archive` objects.

- [`as_epi_archive()`](https://cmu-delphi.github.io/epiprocess/reference/epi_archive.md)
  [`is_epi_archive()`](https://cmu-delphi.github.io/epiprocess/reference/epi_archive.md)
  [`new_epi_archive()`](https://cmu-delphi.github.io/epiprocess/reference/epi_archive.md)
  [`validate_epi_archive()`](https://cmu-delphi.github.io/epiprocess/reference/epi_archive.md)
  :

  `as_epi_archive` converts a data frame, data table, or tibble into an
  `epi_archive` object.

- [`linelist_to_archive()`](https://cmu-delphi.github.io/epiprocess/reference/linelist_to_archive.md)
  :

  Convert a line list to an `epi_archive` object

- [`clone()`](https://cmu-delphi.github.io/epiprocess/reference/clone.md)
  :

  Clone an `epi_archive` object.

- [`group_by(`*`<epi_archive>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/group_by.epi_archive.md)
  [`group_by(`*`<grouped_epi_archive>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/group_by.epi_archive.md)
  [`group_by_drop_default(`*`<grouped_epi_archive>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/group_by.epi_archive.md)
  [`group_vars(`*`<grouped_epi_archive>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/group_by.epi_archive.md)
  [`groups(`*`<grouped_epi_archive>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/group_by.epi_archive.md)
  [`ungroup(`*`<grouped_epi_archive>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/group_by.epi_archive.md)
  [`is_grouped_epi_archive()`](https://cmu-delphi.github.io/epiprocess/reference/group_by.epi_archive.md)
  :

  `group_by` and related methods for `epi_archive`,
  `grouped_epi_archive`

## `epi_archive` manipulation

Functions operating on `epi_archive` objects.

- [`epix_as_of()`](https://cmu-delphi.github.io/epiprocess/reference/epix_as_of.md)
  :

  Generate a snapshot from an `epi_archive` object

- [`epix_as_of_latest()`](https://cmu-delphi.github.io/epiprocess/reference/epix_as_of_latest.md)
  :

  Get the latest snapshot from an `epi_archive` object

- [`epix_as_of_current()`](https://cmu-delphi.github.io/epiprocess/reference/epix_as_of_current.md)
  **\[deprecated\]** :

  Get the latest snapshot from an `epi_archive` object

- [`epix_slide()`](https://cmu-delphi.github.io/epiprocess/reference/epix_slide.md)
  : Take each requested (group and) version in an archive, run a
  computation (e.g., forecast)

- [`revision_analysis()`](https://cmu-delphi.github.io/epiprocess/reference/revision_analysis.md)
  [`print(`*`<revision_analysis>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/revision_analysis.md)
  [`revision_summary()`](https://cmu-delphi.github.io/epiprocess/reference/revision_analysis.md)
  : A function to describe revision behavior for an archive.

- [`epix_merge()`](https://cmu-delphi.github.io/epiprocess/reference/epix_merge.md)
  :

  Merge two `epi_archive` objects

- [`filter(`*`<epi_archive>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/filter.epi_archive.md)
  :

  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
  for `epi_archive`s

- [`epix_fill_through_version()`](https://cmu-delphi.github.io/epiprocess/reference/epix_fill_through_version.md)
  :

  Fill `epi_archive` unobserved history

- [`epix_truncate_versions_after()`](https://cmu-delphi.github.io/epiprocess/reference/epix_truncate_versions_after.md)
  :

  Filter an `epi_archive` object to keep only older versions

- [`set_versions_end()`](https://cmu-delphi.github.io/epiprocess/reference/set_versions_end.md)
  :

  Set the `versions_end` attribute of an `epi_archive` object

## Basic analysis and visualization

- [`autoplot(`*`<epi_df>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/autoplot-epi.md)
  [`autoplot(`*`<epi_archive>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/autoplot-epi.md)
  [`plot(`*`<epi_df>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/autoplot-epi.md)
  [`plot(`*`<epi_archive>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/autoplot-epi.md)
  : Automatically plot an epi_df or epi_archive

- [`plot_heatmap()`](https://cmu-delphi.github.io/epiprocess/reference/plot_heatmap.md)
  : Plot a heatmap for an epi_df

- [`print(`*`<epi_archive>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/print.epi_archive.md)
  :

  Print information about an `epi_archive` object

- [`print(`*`<epi_df>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/print.epi_df.md)
  [`summary(`*`<epi_df>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/print.epi_df.md)
  [`group_by(`*`<epi_df>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/print.epi_df.md)
  [`ungroup(`*`<epi_df>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/print.epi_df.md)
  [`group_modify(`*`<epi_df>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/print.epi_df.md)
  [`unnest(`*`<epi_df>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/print.epi_df.md)
  :

  Base S3 methods for an `epi_df` object

- [`revision_analysis()`](https://cmu-delphi.github.io/epiprocess/reference/revision_analysis.md)
  [`print(`*`<revision_analysis>`*`)`](https://cmu-delphi.github.io/epiprocess/reference/revision_analysis.md)
  [`revision_summary()`](https://cmu-delphi.github.io/epiprocess/reference/revision_analysis.md)
  : A function to describe revision behavior for an archive.

## Example data

- [`cases_deaths_subset`](https://cmu-delphi.github.io/epidatasets/reference/cases_deaths_subset.html)
  : Subset of JHU daily state COVID-19 cases and deaths from 6 states
  (from epidatasets)
- [`archive_cases_dv_subset`](https://cmu-delphi.github.io/epidatasets/reference/archive_cases_dv_subset.html)
  : Subset of daily COVID-19 doctor visits and cases from 6 states in
  archive format (from epidatasets)
- [`covid_incidence_county_subset`](https://cmu-delphi.github.io/epidatasets/reference/covid_incidence_county_subset.html)
  : Subset of JHU daily COVID-19 cases from counties in Massachusetts
  and Vermont (from epidatasets)
- [`covid_incidence_outliers`](https://cmu-delphi.github.io/epidatasets/reference/covid_incidence_outliers.html)
  : Subset of JHU daily COVID-19 cases from New Jersey and Florida (from
  epidatasets)
- [`covid_case_death_rates_extended`](https://cmu-delphi.github.io/epidatasets/reference/covid_case_death_rates_extended.html)
  : JHU daily COVID-19 cases and deaths rates from all states (from
  epidatasets)
