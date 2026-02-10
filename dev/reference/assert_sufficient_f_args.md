# Assert that a sliding computation function takes enough args

Assert that a sliding computation function takes enough args

## Usage

``` r
assert_sufficient_f_args(.f, ..., .ref_time_value_label)
```

## Arguments

- ...:

  Dots that will be forwarded to `f` from the dots of `epi_slide` or
  `epix_slide`.

- .ref_time_value_label:

  String; how to describe/label the `ref_time_value` in error messages;
  e.g., "reference time value" or "version".

- f:

  Function; specifies a computation to slide over an `epi_df` or
  `epi_archive` in `epi_slide` or `epix_slide`.
