# revision_summary works for a dummy dataset

    Code
      dummy_ex %>% revision_summary() %>% print(n = 10, width = 300)
    Message
      Min lag (time to first version):
    Output
           min median     mean    max
        0 days 1 days 1.6 days 4 days
    Message
      Fraction of epi_key+time_values with
      No revisions:
      * 3 out of 7 (42.86%)
      Quick revisions (last revision within 3 days of the `time_value`):
      * 4 out of 7 (57.14%)
      Few revisions (At most 3 revisions for that `time_value`):
      * 6 out of 7 (85.71%)
      Fraction of revised epi_key+time_values which have:
      Less than 0.1 spread in relative value:
      * 1 out of 4 (25%)
      Spread of more than 5.1 in actual value (when revised):
      * 3 out of 4 (75%)
      days until within 20% of the latest value:
    Output
           min median     mean     max
        0 days 3 days 6.9 days 19 days
      # A tibble: 7 x 11
        time_value geo_value n_revisions min_lag max_lag time_near_latest spread
        <date>     <chr>           <dbl> <drtn>  <drtn>  <drtn>            <dbl>
      1 2020-01-01 ak                  4 2 days  19 days 19 days             101
      2 2020-01-01 al                  1 0 days  19 days 19 days              99
      3 2020-01-02 ak                  1 4 days   5 days  4 days               9
      4 2020-01-02 al                  0 0 days   0 days  0 days               0
      5 2020-01-03 ak                  0 3 days   3 days  3 days               0
      6 2020-01-03 al                  1 1 days   2 days  2 days               3
      7 2020-01-04 al                  0 1 days   1 days  1 days               0
        rel_spread min_value max_value median_value
             <dbl>     <dbl>     <dbl>        <dbl>
      1      0.990         1       102          6  
      2      0.99          1       100         50.5
      3      0.09         91       100         95.5
      4      0             1         1          1  
      5    NaN             0         0          0  
      6      0.75          1         4          2.5
      7      0             9         9          9  

---

    Code
      dummy_ex %>% revision_summary(drop_nas = FALSE) %>% print(n = 10, width = 300)
    Message
      Min lag (time to first version):
    Output
           min median     mean    max
        0 days 1 days 1.4 days 4 days
    Message
      Fraction of all versions that are `NA`:
      * 2 out of 19 (10.53%)
      Fraction of epi_key+time_values with
      No revisions:
      * 2 out of 7 (28.57%)
      Quick revisions (last revision within 3 days of the `time_value`):
      * 4 out of 7 (57.14%)
      Few revisions (At most 3 revisions for that `time_value`):
      * 6 out of 7 (85.71%)
      Fraction of revised epi_key+time_values which have:
      Less than 0.1 spread in relative value:
      * 2 out of 5 (40%)
      Spread of more than 5.1 in actual value (when revised):
      * 3 out of 5 (60%)
      days until within 20% of the latest value:
    Output
           min median     mean     max
        0 days 3 days 6.9 days 19 days
      # A tibble: 7 x 11
        time_value geo_value n_revisions min_lag max_lag time_near_latest spread
        <date>     <chr>           <dbl> <drtn>  <drtn>  <drtn>            <dbl>
      1 2020-01-01 ak                  6 2 days  19 days 19 days             101
      2 2020-01-01 al                  1 0 days  19 days 19 days              99
      3 2020-01-02 ak                  1 4 days   5 days  4 days               9
      4 2020-01-02 al                  0 0 days   0 days  0 days               0
      5 2020-01-03 ak                  0 3 days   3 days  3 days               0
      6 2020-01-03 al                  1 1 days   2 days  2 days               3
      7 2020-01-04 al                  1 0 days   1 days  1 days               0
        rel_spread min_value max_value median_value
             <dbl>     <dbl>     <dbl>        <dbl>
      1      0.990         1       102          5.5
      2      0.99          1       100         50.5
      3      0.09         91       100         95.5
      4      0             1         1          1  
      5    NaN             0         0          0  
      6      0.75          1         4          2.5
      7      0             9         9          9  

