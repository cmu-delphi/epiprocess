# revision_summary works for dummy datasets

    Code
      rs1
    Message
      
      -- An epi_archive spanning 2020-01-01 to 2020-01-01. --
      
      -- Min lag (time to first version): 
    Output
           min median     mean    max
        0 days 1 days 1.6 days 4 days
    Message
      
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

---

    Code
      rs1$revision_behavior %>% print(n = 10, width = 300)
    Output
      # A tibble: 7 x 11
        time_value geo_value n_revisions min_lag max_lag lag_near_latest spread
        <date>     <chr>           <dbl> <drtn>  <drtn>  <drtn>           <dbl>
      1 2020-01-01 ak                  4 2 days  19 days 19 days            101
      2 2020-01-02 ak                  1 4 days   5 days  4 days              9
      3 2020-01-03 ak                  0 3 days   3 days  3 days              0
      4 2020-01-01 al                  1 0 days  19 days 19 days             99
      5 2020-01-02 al                  0 0 days   0 days  0 days              0
      6 2020-01-03 al                  1 1 days   2 days  2 days              3
      7 2020-01-04 al                  0 1 days   1 days  1 days              0
        rel_spread min_value max_value median_value
             <dbl>     <dbl>     <dbl>        <dbl>
      1      0.990         1       102          6  
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
      
      -- An epi_archive spanning 2020-01-01 to 2020-01-01. --
      
      -- Min lag (time to first version): 
    Output
           min median     mean    max
        0 days 1 days 1.4 days 4 days
    Message
      Fraction of all versions that are `NA`:
      * 2 out of 19 (10.53%)
      
      -- Fraction of epi_key + time_values with 
      No revisions:
      * 2 out of 7 (28.57%)
      Quick revisions (last revision within 3 days of the `time_value`):
      * 4 out of 7 (57.14%)
      Few revisions (At most 3 revisions for that `time_value`):
      * 6 out of 7 (85.71%)
      
      -- Fraction of revised epi_key + time_values which have: 
      Less than 0.1 spread in relative value:
      * 2 out of 5 (40%)
      Spread of more than 5.1 in actual value (when revised):
      * 3 out of 5 (60%)
      
      -- Days until within 20% of the latest value: 
    Output
           min median     mean     max
        0 days 3 days 6.9 days 19 days

---

    Code
      rs2$revision_behavior %>% print(n = 10, width = 300)
    Output
      # A tibble: 7 x 11
        time_value geo_value n_revisions min_lag max_lag lag_near_latest spread
        <date>     <chr>           <dbl> <drtn>  <drtn>  <drtn>           <dbl>
      1 2020-01-01 ak                  6 2 days  19 days 19 days            101
      2 2020-01-02 ak                  1 4 days   5 days  4 days              9
      3 2020-01-03 ak                  0 3 days   3 days  3 days              0
      4 2020-01-01 al                  1 0 days  19 days 19 days             99
      5 2020-01-02 al                  0 0 days   0 days  0 days              0
      6 2020-01-03 al                  1 1 days   2 days  2 days              3
      7 2020-01-04 al                  1 0 days   1 days  1 days              0
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
      
      -- An epi_archive spanning 2020-01-01 to 2020-01-01. --
      
      -- Min lag (time to first version): 
    Output
            min  median      mean     max
        0 weeks 1 weeks 1.4 weeks 4 weeks
    Message
      Fraction of all versions that are `NA`:
      * 2 out of 19 (10.53%)
      
      -- Fraction of epi_key + time_values with 
      No revisions:
      * 2 out of 7 (28.57%)
      Quick revisions (last revision within 1 week of the `time_value`):
      * 2 out of 7 (28.57%)
      Few revisions (At most 3 revisions for that `time_value`):
      * 6 out of 7 (85.71%)
      
      -- Fraction of revised epi_key + time_values which have: 
      Less than 0.1 spread in relative value:
      * 2 out of 5 (40%)
      Spread of more than 5.1 in actual value (when revised):
      * 3 out of 5 (60%)
      
      -- Weeks until within 20% of the latest value: 
    Output
            min  median      mean      max
        0 weeks 3 weeks 6.9 weeks 19 weeks

---

    Code
      rs3$revision_behavior %>% print(n = 10, width = 300)
    Output
      # A tibble: 7 x 11
        time_value geo_value n_revisions min_lag max_lag  lag_near_latest spread
        <date>     <chr>           <dbl> <drtn>  <drtn>   <drtn>           <dbl>
      1 2020-01-01 ak                  6 2 weeks 19 weeks 19 weeks           101
      2 2020-01-08 ak                  1 4 weeks  5 weeks  4 weeks             9
      3 2020-01-15 ak                  0 3 weeks  3 weeks  3 weeks             0
      4 2020-01-01 al                  1 0 weeks 19 weeks 19 weeks            99
      5 2020-01-08 al                  0 0 weeks  0 weeks  0 weeks             0
      6 2020-01-15 al                  1 1 weeks  2 weeks  2 weeks             3
      7 2020-01-22 al                  1 0 weeks  1 weeks  1 weeks             0
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
      
      -- An epi_archive spanning 2020 Jan to 2020 Jan. --
      
      -- Min lag (time to first version): 
    Output
        min median mean max
          0      1  1.4   4
    Message
      Fraction of all versions that are `NA`:
      * 2 out of 19 (10.53%)
      
      -- Fraction of epi_key + time_values with 
      No revisions:
      * 2 out of 7 (28.57%)
      Quick revisions (last revision within 1 month of the `time_value`):
      * 2 out of 7 (28.57%)
      Few revisions (At most 3 revisions for that `time_value`):
      * 6 out of 7 (85.71%)
      
      -- Fraction of revised epi_key + time_values which have: 
      Less than 0.1 spread in relative value:
      * 2 out of 5 (40%)
      Spread of more than 5.1 in actual value (when revised):
      * 3 out of 5 (60%)
      
      -- Months until within 20% of the latest value: 
    Output
        min median mean max
          0      3  6.9  19

---

    Code
      rs4$revision_behavior %>% print(n = 10, width = 300)
    Output
      # A tibble: 7 x 11
        time_value geo_value n_revisions min_lag max_lag lag_near_latest spread
             <mth> <chr>           <dbl>   <dbl>   <dbl>           <dbl>  <dbl>
      1   2020 Jan ak                  6       2      19              19    101
      2   2020 Feb ak                  1       4       5               4      9
      3   2020 Mar ak                  0       3       3               3      0
      4   2020 Jan al                  1       0      19              19     99
      5   2020 Feb al                  0       0       0               0      0
      6   2020 Mar al                  1       1       2               2      3
      7   2020 Apr al                  1       0       1               1      0
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
      
      -- An epi_archive spanning 1 to 1. --
      
      -- Min lag (time to first version): 
    Output
        min median mean max
          0      1  1.4   4
    Message
      Fraction of all versions that are `NA`:
      * 2 out of 19 (10.53%)
      
      -- Fraction of epi_key + time_values with 
      No revisions:
      * 2 out of 7 (28.57%)
      Quick revisions (last revision within 3 time steps of the `time_value`):
      * 4 out of 7 (57.14%)
      Few revisions (At most 3 revisions for that `time_value`):
      * 6 out of 7 (85.71%)
      
      -- Fraction of revised epi_key + time_values which have: 
      Less than 0.1 spread in relative value:
      * 2 out of 5 (40%)
      Spread of more than 5.1 in actual value (when revised):
      * 3 out of 5 (60%)
      
      -- Time Steps until within 20% of the latest value: 
    Output
        min median mean max
          0      3  6.9  19

---

    Code
      rs5$revision_behavior %>% print(n = 10, width = 300)
    Output
      # A tibble: 7 x 11
        time_value geo_value n_revisions min_lag max_lag lag_near_latest spread
             <dbl> <chr>           <dbl>   <dbl>   <dbl>           <dbl>  <dbl>
      1          1 ak                  6       2      19              19    101
      2          2 ak                  1       4       5               4      9
      3          3 ak                  0       3       3               3      0
      4          1 al                  1       0      19              19     99
      5          2 al                  0       0       0               0      0
      6          3 al                  1       1       2               2      3
      7          4 al                  1       0       1               1      0
        rel_spread min_value max_value median_value
             <dbl>     <dbl>     <dbl>        <dbl>
      1      0.990         1       102          5.5
      2      0.09         91       100         95.5
      3    NaN             0         0          0  
      4      0.99          1       100         50.5
      5      0             1         1          1  
      6      0.75          1         4          2.5
      7      0             9         9          9  

