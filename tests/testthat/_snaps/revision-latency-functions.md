# revision_summary works for dummy datasets

    Code
      rs1
    Message
      
      -- Revision analysis for archive spanning time values 2020-01-01 to 2020-01-04. --
      
      -- Across epi_key + versions that add new time values: 
      Freshest new time value's lag/latency:
    Output
           min median     mean    max
        0 days 1 days 1.2 days 3 days
    Message
      Farthest-back new time value's lag/latency:
    Output
           min median     mean    max
        0 days 1 days 1.3 days 4 days
    Message
      
      -- Across epi_key + time_value + versions: 
      Fraction of all versions that are `NA`:
      * 1 out of 16 (6.25%)
      
      -- Bulk reporting of older epikey + time values: none detected 
      Initial lags above 5 days would have been counted as bulk reporting.
      
      -- Remaining information is for non-bulk-reported epikey + time values
         with semi-stable versions past the waiting period available. 
      
      -- Fraction of epi_key + time_values with 
      No revisions:
      * 3 out of 7 (42.86%)
      Quick revisions (last revision within 3 days of the `time_value`):
      * 4 out of 7 (57.14%)
      Few revisions (At most 3 revisions for that `time_value`):
      * 6 out of 7 (85.71%)
      
      -- Fraction of revised epi_key + time_values which have: 
      Less than 0.1 spread in relative value:
      * 1 out of 4 (25%)
      Spread of more than 5.1 in actual value (when revised):
      * 3 out of 4 (75%)
      
      -- Days until within 20% of the latest value: 
    Output
           min median     mean     max
        0 days 3 days 6.9 days 19 days
    Message
      
      -- Days until at the latest lag: 
    Output
           min median   mean     max
        0 days 3 days 7 days 19 days

---

    Code
      rs1$revision_behavior %>% print(n = 10, width = 300)
    Output
      # A tibble: 7 x 11
        time_value geo_value n_revisions min_lag max_lag lag_near_latest spread
        <date>     <chr>           <int> <drtn>  <drtn>  <drtn>           <dbl>
      1 2020-01-01 ak                  6 2 days  19 days 19 days            101
      2 2020-01-02 ak                  1 4 days   5 days  4 days              9
      3 2020-01-03 ak                  0 3 days   3 days  3 days              0
      4 2020-01-01 al                  1 0 days  19 days 19 days             99
      5 2020-01-02 al                  0 0 days   0 days  0 days              0
      6 2020-01-03 al                  1 1 days   2 days  2 days              3
      7 2020-01-04 al                  0 1 days   1 days  1 days              0
        rel_spread min_value max_value median_value
             <dbl>     <dbl>     <dbl>        <dbl>
      1      0.990         1       102          5.5
      2      0.09         91       100         95.5
      3    NaN             0         0          0  
      4      0.99          1       100         50.5
      5      0             1         1          1  
      6      0.75          1         4          2.5
      7      0             9         9          9  

---

    Code
      rs2
    Message
      
      -- Revision analysis for archive spanning time values 2020-01-01 to 2020-01-04. --
      
      -- Across epi_key + versions that add new time values: 
      Freshest new time value's lag/latency:
    Output
           min median     mean    max
        0 days 1 days 1.2 days 3 days
    Message
      Farthest-back new time value's lag/latency:
    Output
           min median     mean    max
        0 days 1 days 1.3 days 4 days
    Message
      
      -- Across epi_key + time_value + versions: 
      Fraction of all versions that are `NA`:
      * 1 out of 16 (6.25%)
      
      -- Bulk reporting of older epikey + time values: none detected 
      Initial lags above 5 days would have been counted as bulk reporting.
      
      -- Remaining information is for non-bulk-reported epikey + time values
         with semi-stable versions past the waiting period available. 
      
      -- Fraction of epi_key + time_values with 
      No revisions:
      * 3 out of 7 (42.86%)
      Quick revisions (last revision within 3 days of the `time_value`):
      * 4 out of 7 (57.14%)
      Few revisions (At most 3 revisions for that `time_value`):
      * 6 out of 7 (85.71%)
      
      -- Fraction of revised epi_key + time_values which have: 
      Less than 0.1 spread in relative value:
      * 1 out of 4 (25%)
      Spread of more than 5.1 in actual value (when revised):
      * 3 out of 4 (75%)
      
      -- Days until within 20% of the latest value: 
    Output
           min median     mean     max
        0 days 3 days 6.9 days 19 days
    Message
      
      -- Days until at the latest lag: 
    Output
           min median   mean     max
        0 days 3 days 7 days 19 days

---

    Code
      rs2$revision_behavior %>% print(n = 10, width = 300)
    Output
      # A tibble: 7 x 11
        time_value geo_value n_revisions min_lag max_lag lag_near_latest spread
        <date>     <chr>           <int> <drtn>  <drtn>  <drtn>           <dbl>
      1 2020-01-01 ak                  6 2 days  19 days 19 days            101
      2 2020-01-02 ak                  1 4 days   5 days  4 days              9
      3 2020-01-03 ak                  0 3 days   3 days  3 days              0
      4 2020-01-01 al                  1 0 days  19 days 19 days             99
      5 2020-01-02 al                  0 0 days   0 days  0 days              0
      6 2020-01-03 al                  1 1 days   2 days  2 days              3
      7 2020-01-04 al                  0 1 days   1 days  1 days              0
        rel_spread min_value max_value median_value
             <dbl>     <dbl>     <dbl>        <dbl>
      1      0.990         1       102          5.5
      2      0.09         91       100         95.5
      3    NaN             0         0          0  
      4      0.99          1       100         50.5
      5      0             1         1          1  
      6      0.75          1         4          2.5
      7      0             9         9          9  

---

    Code
      rs3
    Message
      
      -- Revision analysis for archive spanning time values 2020-01-01 to 2020-01-22. --
      
      -- Across epi_key + versions that add new time values: 
      Freshest new time value's lag/latency:
    Output
            min  median      mean     max
        0 weeks 1 weeks 1.2 weeks 3 weeks
    Message
      Farthest-back new time value's lag/latency:
    Output
            min  median      mean     max
        0 weeks 1 weeks 1.3 weeks 4 weeks
    Message
      
      -- Across epi_key + time_value + versions: 
      Fraction of all versions that are `NA`:
      * 1 out of 16 (6.25%)
      
      -- Bulk reporting of older epikey + time values: none detected 
      Initial lags above 5 weeks would have been counted as bulk reporting.
      
      -- Remaining information is for non-bulk-reported epikey + time values
         with semi-stable versions past the waiting period available. 
      
      -- Fraction of epi_key + time_values with 
      No revisions:
      * 3 out of 7 (42.86%)
      Quick revisions (last revision within 1 week of the `time_value`):
      * 2 out of 7 (28.57%)
      Few revisions (At most 3 revisions for that `time_value`):
      * 6 out of 7 (85.71%)
      
      -- Fraction of revised epi_key + time_values which have: 
      Less than 0.1 spread in relative value:
      * 1 out of 4 (25%)
      Spread of more than 5.1 in actual value (when revised):
      * 3 out of 4 (75%)
      
      -- Weeks until within 20% of the latest value: 
    Output
            min  median      mean      max
        0 weeks 3 weeks 6.9 weeks 19 weeks
    Message
      
      -- Weeks until at the latest lag: 
    Output
            min  median    mean      max
        0 weeks 3 weeks 7 weeks 19 weeks

---

    Code
      rs3$revision_behavior %>% print(n = 10, width = 300)
    Output
      # A tibble: 7 x 11
        time_value geo_value n_revisions min_lag max_lag  lag_near_latest spread
        <date>     <chr>           <int> <drtn>  <drtn>   <drtn>           <dbl>
      1 2020-01-01 ak                  6 2 weeks 19 weeks 19 weeks           101
      2 2020-01-08 ak                  1 4 weeks  5 weeks  4 weeks             9
      3 2020-01-15 ak                  0 3 weeks  3 weeks  3 weeks             0
      4 2020-01-01 al                  1 0 weeks 19 weeks 19 weeks            99
      5 2020-01-08 al                  0 0 weeks  0 weeks  0 weeks             0
      6 2020-01-15 al                  1 1 weeks  2 weeks  2 weeks             3
      7 2020-01-22 al                  0 1 weeks  1 weeks  1 weeks             0
        rel_spread min_value max_value median_value
             <dbl>     <dbl>     <dbl>        <dbl>
      1      0.990         1       102          5.5
      2      0.09         91       100         95.5
      3    NaN             0         0          0  
      4      0.99          1       100         50.5
      5      0             1         1          1  
      6      0.75          1         4          2.5
      7      0             9         9          9  

---

    Code
      rs4
    Message
      
      -- Revision analysis for archive spanning time values 2020 Jan to 2020 Apr. --
      
      -- Across epi_key + versions that add new time values: 
      Freshest new time value's lag/latency:
    Output
        min median mean max
          0      1  1.2   3
    Message
      Farthest-back new time value's lag/latency:
    Output
        min median mean max
          0      1  1.3   4
    Message
      
      -- Across epi_key + time_value + versions: 
      Fraction of all versions that are `NA`:
      * 1 out of 16 (6.25%)
      
      -- Bulk reporting of older epikey + time values: none detected 
      Initial lags above 5 months would have been counted as bulk reporting.
      
      -- Remaining information is for non-bulk-reported epikey + time values
         with semi-stable versions past the waiting period available. 
      
      -- Fraction of epi_key + time_values with 
      No revisions:
      * 3 out of 7 (42.86%)
      Quick revisions (last revision within 1 month of the `time_value`):
      * 2 out of 7 (28.57%)
      Few revisions (At most 3 revisions for that `time_value`):
      * 6 out of 7 (85.71%)
      
      -- Fraction of revised epi_key + time_values which have: 
      Less than 0.1 spread in relative value:
      * 1 out of 4 (25%)
      Spread of more than 5.1 in actual value (when revised):
      * 3 out of 4 (75%)
      
      -- Months until within 20% of the latest value: 
    Output
        min median mean max
          0      3  6.9  19
    Message
      
      -- Months until at the latest lag: 
    Output
        min median mean max
          0      3    7  19

---

    Code
      rs4$revision_behavior %>% print(n = 10, width = 300)
    Output
      # A tibble: 7 x 11
        time_value geo_value n_revisions min_lag max_lag lag_near_latest spread
             <mth> <chr>           <int>   <dbl>   <dbl>           <dbl>  <dbl>
      1   2020 Jan ak                  6       2      19              19    101
      2   2020 Feb ak                  1       4       5               4      9
      3   2020 Mar ak                  0       3       3               3      0
      4   2020 Jan al                  1       0      19              19     99
      5   2020 Feb al                  0       0       0               0      0
      6   2020 Mar al                  1       1       2               2      3
      7   2020 Apr al                  0       1       1               1      0
        rel_spread min_value max_value median_value
             <dbl>     <dbl>     <dbl>        <dbl>
      1      0.990         1       102          5.5
      2      0.09         91       100         95.5
      3    NaN             0         0          0  
      4      0.99          1       100         50.5
      5      0             1         1          1  
      6      0.75          1         4          2.5
      7      0             9         9          9  

---

    Code
      print(rs5, quick_revision = 3)
    Message
      
      -- Revision analysis for archive spanning time values 1 to 4. --
      
      -- Across epi_key + versions that add new time values: 
      Freshest new time value's lag/latency:
    Output
        min median mean max
          0      1  1.2   3
    Message
      Farthest-back new time value's lag/latency:
    Output
        min median mean max
          0      1  1.3   4
    Message
      
      -- Across epi_key + time_value + versions: 
      Fraction of all versions that are `NA`:
      * 1 out of 16 (6.25%)
      
      -- Bulk reporting of older epikey + time values: none detected 
      Initial lags above 5 time steps would have been counted as bulk reporting.
      
      -- Remaining information is for non-bulk-reported epikey + time values
         with semi-stable versions past the waiting period available. 
      
      -- Fraction of epi_key + time_values with 
      No revisions:
      * 3 out of 7 (42.86%)
      Quick revisions (last revision within 3 time steps of the `time_value`):
      * 4 out of 7 (57.14%)
      Few revisions (At most 3 revisions for that `time_value`):
      * 6 out of 7 (85.71%)
      
      -- Fraction of revised epi_key + time_values which have: 
      Less than 0.1 spread in relative value:
      * 1 out of 4 (25%)
      Spread of more than 5.1 in actual value (when revised):
      * 3 out of 4 (75%)
      
      -- Time Steps until within 20% of the latest value: 
    Output
        min median mean max
          0      3  6.9  19
    Message
      
      -- Time Steps until at the latest lag: 
    Output
        min median mean max
          0      3    7  19

---

    Code
      rs5$revision_behavior %>% print(n = 10, width = 300)
    Output
      # A tibble: 7 x 11
        time_value geo_value n_revisions min_lag max_lag lag_near_latest spread
             <dbl> <chr>           <int>   <dbl>   <dbl>           <dbl>  <dbl>
      1          1 ak                  6       2      19              19    101
      2          2 ak                  1       4       5               4      9
      3          3 ak                  0       3       3               3      0
      4          1 al                  1       0      19              19     99
      5          2 al                  0       0       0               0      0
      6          3 al                  1       1       2               2      3
      7          4 al                  0       1       1               1      0
        rel_spread min_value max_value median_value
             <dbl>     <dbl>     <dbl>        <dbl>
      1      0.990         1       102          5.5
      2      0.09         91       100         95.5
      3    NaN             0         0          0  
      4      0.99          1       100         50.5
      5      0             1         1          1  
      6      0.75          1         4          2.5
      7      0             9         9          9  

# revision_summary bulk reporting summary works as expected

    Code
      bind_rows(tibble(geo_value = 1, time_value = 1:100, version = (100 + 9) %/% 7 *
        7, value = 1:100), tibble(geo_value = 1, time_value = 101:200, version = (
      time_value + 9) %/% 7 * 7, value = 1:100)) %>% mutate(across(c(time_value,
        version), ~ as.Date("2020-01-01") + .x - 1)) %>% as_epi_archive() %>%
        revision_summary()
    Message
      
      -- Revision analysis for archive spanning time values 2020-01-01 to 2020-07-18. --
      
      -- Across epi_key + versions that add new time values: 
      Freshest new time value's lag/latency:
    Output
           min median     mean    max
        3 days 3 days 3.1 days 4 days
    Message
      Farthest-back new time value's lag/latency:
    Output
           min median      mean      max
        9 days 9 days 22.6 days 104 days
    Message
      
      -- Across epi_key + time_value + versions: 
      Fraction of all versions that are `NA`:
      * 0 out of 143 (0%)
      
      -- Bulk reporting adding initial observations for older epikey + time values: 
      Initial lags above 11 days were counted as bulk reporting.
      Fraction of epi_key + time_values initially added by bulk reporting:
      * 93 out of 143 (65.03%)
      Versions containing bulk reporting: 1
      * (2020-04-14)
      Versions adding epikey + time values but no bulk reporting: 6
      Revision-only versions: 0
      
      -- Remaining information is for non-bulk-reported epikey + time values
         with semi-stable versions past the waiting period available. 
      
      -- Fraction of epi_key + time_values with 
      No revisions:
      * 50 out of 50 (100%)
      Quick revisions (last revision within 3 days of the `time_value`):
      * 6 out of 50 (12%)
      Few revisions (At most 3 revisions for that `time_value`):
      * 50 out of 50 (100%)
      
      -- Fraction of revised epi_key + time_values which have: 
      Less than 0.1 spread in relative value:
      * 0 out of 0 (NaN%)
      Spread of more than 5 in actual value (when revised):
      * 0 out of 0 (NaN%)
      
      -- Days until within 20% of the latest value: 
    Output
           min median     mean     max
        3 days 6 days 6.2 days 11 days
    Message
      
      -- Days until at the latest lag: 
    Output
           min median     mean     max
        3 days 6 days 6.2 days 11 days

