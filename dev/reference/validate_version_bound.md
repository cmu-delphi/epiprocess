# Validate a version bound arg

Expected to be used on `clobberable_versions_start`, `versions_end`, and
similar arguments. Some additional context-specific checks may be
needed. Side effects: raises an error if version bound appears invalid.

## Usage

``` r
validate_version_bound(
  version_bound,
  x,
  na_ok = FALSE,
  version_bound_arg = rlang::caller_arg(version_bound),
  x_arg = rlang::caller_arg(x)
)
```

## Arguments

- version_bound:

  the version bound to validate

- x:

  a data frame containing a version column with which to check
  compatibility

- na_ok:

  Boolean; is `NA` an acceptable "bound"? (If so, `NA` will have a
  special context-dependent meaning.)

- version_bound_arg:

  optional string; what to call the version bound in error messages
