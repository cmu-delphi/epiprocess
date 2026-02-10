# return the first value in values_from from the last string of trues in bool_vec

the point of this operation is to get the value in values_from which
occurs at the same index as the start of the last run of true values in
bool_vec. for example, in c(1,1,0,1,1), we want the 4th entry, since
there's a 0 breaking the run

## Usage

``` r
get_last_run(bool_vec, values_from)
```
