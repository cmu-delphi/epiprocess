# revision_summary works for a dummy dataset

    Code
      dummy_ex %>% revision_summary() %>% print(n = 10, width = 300)
    Message
      Number of revisions:
      Min lag (time to first version):
    Output
           min median   mean    max
        0 days 1 days 2 days 4 days
    Message
      No revisions:
      * 3 out of 7 (42.86%)
      Quick revisions (last revision within 3 days of the `time_value`):
      * 4 out of 7 (57.14%)
      Few revisions (At most 3 revisions for that `time_value`):
      * 6 out of 7 (85.71%)
      Changes in Value:
      Less than 0.1 change in relative value (only from the revised subset):
      * 1 out of 4 (25%)
      days until over 0.8 percent of the final value:
    Output
           min median   mean     max
        0 days 3 days 7 days 19 days
    Message
      Change by more than 5.1 in actual value (when revised):
      * 3 out of 4 (75%)
    Output
      # A tibble: 7 x 8
        time_value geo_value n_revisions min_lag max_lag max_change max_rel_change
        <date>     <chr>           <dbl> <drtn>  <drtn>       <dbl>          <dbl>
      1 2020-01-01 ak                  4 2 days  19 days        101          0.990
      2 2020-01-01 al                  1 0 days  19 days         99          0.99 
      3 2020-01-02 ak                  1 4 days   5 days          9          0.09 
      4 2020-01-02 al                  0 0 days   0 days          0          0    
      5 2020-01-03 ak                  0 3 days   3 days          0        NaN    
      6 2020-01-03 al                  1 1 days   2 days          3          0.75 
      7 2020-01-04 al                  0 1 days   1 days          0          0    
        time_to_pct_final
        <drtn>           
      1 19 days          
      2 19 days          
      3  4 days          
      4  0 days          
      5  3 days          
      6  2 days          
      7  1 days          

---

    Code
      dummy_ex %>% revision_summary(drop_nas = FALSE) %>% print(n = 10, width = 300)
    Message
      Number of revisions:
      Min lag (time to first version):
    Output
           min median   mean    max
        0 days 1 days 1 days 4 days
    Message
      Fraction of all versions that are `NA`:
      * 2 out of 19 (10.53%)
      No revisions:
      * 1 out of 7 (14.29%)
      Quick revisions (last revision within 3 days of the `time_value`):
      * 3 out of 7 (42.86%)
      Few revisions (At most 3 revisions for that `time_value`):
      * 6 out of 7 (85.71%)
      Changes in Value:
      Less than 0.1 change in relative value (only from the revised subset):
      * 3 out of 6 (50%)
      days until over 0.8 percent of the final value:
    Output
           min median   mean     max
        0 days 3 days 7 days 19 days
    Message
      Change by more than 5.1 in actual value (when revised):
      * 3 out of 6 (50%)
    Output
      # A tibble: 7 x 8
        time_value geo_value n_revisions min_lag max_lag max_change max_rel_change
        <date>     <chr>           <dbl> <drtn>  <drtn>       <dbl>          <dbl>
      1 2020-01-01 ak                  6 2 days  19 days        101          0.990
      2 2020-01-01 al                  2 0 days  19 days         99          0.99 
      3 2020-01-02 ak                  1 4 days   5 days          9          0.09 
      4 2020-01-02 al                  0 0 days   0 days          0          0    
      5 2020-01-03 ak                  1 3 days   4 days          0        NaN    
      6 2020-01-03 al                  1 1 days   2 days          3          0.75 
      7 2020-01-04 al                  1 0 days   1 days          0          0    
        time_to_pct_final
        <drtn>           
      1 19 days          
      2 19 days          
      3  4 days          
      4  0 days          
      5  3 days          
      6  2 days          
      7  1 days          

