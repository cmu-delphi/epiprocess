# epiprocess

This package introduces a common data structure for epidemiological data sets
measured over space and time, and offers associated utilities to perform basic
signal processing tasks. 

The main data structure in the `epiprocess` package is called `epi_df`, which is
simply a tibble with a couple of required columns, `geo_value` and
`time_value`. It can have any other number of columns, which are considered 
measured variables. (An `epi_df` object also has some special metadata.) The
function `as.epi_df()`can be used to convert a data frame or tibble into
`epi_df` format. 

The functions that operate on `epi_df` objects in the `epiprocess` package all
begin with `epi`, namely:

- `epi_slide()`, for iteratively applying a custom computation to a variable in
  an `epi_df` object over sliding windows in time; 
  
- `epi_cor()`, for computing lagged correlations between variables in an
  `epi_df` object, (allowing for grouping by geo value, time value, or any other
  variables); 
  
- `epi_detect_outlr()`, for detecting and correcting outliers in a variable in
  an `epi_df` object, using either built-in or custom methodologies.  

Other notable functions include `pct_change()`and `estimate_deriv()`, which,
when used within a call to `epi_slide()`, allow for fluid computation of
percentage change values or estimation of derivatives over time.

See the getting started guide and vignettes for examples. 
