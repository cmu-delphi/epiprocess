# dplyr_reconstruct

dplyr_reconstruct

## Usage

``` r
# S3 method for class 'epi_df'
dplyr_reconstruct(data, template)
```

## Arguments

- data:

  tibble or `epi_df` (`dplyr` feeds in former, but we may directly feed
  in latter from our other methods)

- template:

  `epi_df` template to use to restore

## Value

`epi_df` or degrade into `tbl_df`
