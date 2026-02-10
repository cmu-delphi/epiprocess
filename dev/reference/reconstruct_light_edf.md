# Like `dplyr_reconstruct.epi_df` but not recomputing any grouping

In the move to our current not-quite-proper/effective "implementation"
of
[`dplyr::dplyr_extending`](https://dplyr.tidyverse.org/reference/dplyr_extending.html)
for `epi_df`s, we moved a lot of checks in `dplyr_reconstruct` and used
it instead of `reclass()` in various operations to prevent operations
from outputting invalid metadata/classes, instead of more careful
tailored and relevant checks. However, this actually introduced extra
overhead due to
[`dplyr_reconstruct.epi_df()`](https://cmu-delphi.github.io/epiprocess/dev/reference/dplyr_reconstruct.epi_df.md)
passing off to `dplyr_reconstruct.grouped_df()` when grouped, which
assumes that it will need to / should for safety recompute the groups,
even when it'd be safe for it not to do so. In many operations, we're
using [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) to
dispatch to `grouped_df` behavior if needed, and it should output
something with valid groupings.

## Usage

``` r
reconstruct_light_edf(data, template)
```

## Details

This function serves the original purpose of performing `epi_df`-centric
checks rather than just throwing on potentially-incorrect metadata like
`reclass()`, but without unnecessary
[`dplyr_reconstruct()`](https://dplyr.tidyverse.org/reference/dplyr_extending.html)
delegation.
