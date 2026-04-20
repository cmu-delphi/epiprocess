# Plot a heatmap for an epi_df

Plot a heatmap for an epi_df

## Usage

``` r
plot_heatmap(x, ..., .max_keys = 60)
```

## Arguments

- x:

  An `epi_df` object.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  One or more unquoted expressions separated by commas. Variable names
  can be used as if they were positions in the data frame, so
  expressions like `x:y` can be used to select a range of variables.

- .max_keys:

  The maximum number of key combinations to display.

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Examples

``` r
# Use it on an `epi_df`
plot_heatmap(cases_deaths_subset, case_rate_7d_av)


# Plotting multiple variables
plot_heatmap(cases_deaths_subset, case_rate_7d_av, death_rate_7d_av)
```
