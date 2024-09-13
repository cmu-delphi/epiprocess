# Forbidden epi_df methods have decent error messages

    Code
      edf %>% epi_slide(.window_size = 7L, ~ mean(.x))
    Condition
      Error in `mean()`:
      ! `mean` shouldn't be used on entire `epi_df`s
      x .x was an `epi_df`
      i If you encountered this while trying to take a rolling mean of a column using `epi_slide`, you probably forgot to specify the column name (e.g., ~ mean(.x$colname)). You may also prefer to use the specialized `epi_slide_mean` method.

---

    Code
      edf %>% epi_slide(.window_size = 7L, ~ sum(.x))
    Condition
      Error in `.slide_comp()`:
      ! `sum` shouldn't be used on entire `epi_df`s
      x `sum`'s first argument was an `epi_df`
      i If you encountered this while trying to take a rolling sum of a column using `epi_slide`, you probably forgot to specify the column name (e.g., ~ sum(.x$colname)). You may also prefer to use the specialized `epi_slide_sum` method.

---

    Code
      edf %>% epi_slide(.window_size = 7L, ~ min(.x))
    Condition
      Error in `.slide_comp()`:
      ! `min` shouldn't be used on entire `epi_df`s
      x `min`'s first argument was an `epi_df`
      i If you encountered this while trying to take a rolling min of a column using `epi_slide`, you probably forgot to specify the column name (e.g., ~ min(.x$colname)). You may also prefer to use the specialized `epi_slide_opt` method.

---

    Code
      edf %>% epi_slide(.window_size = 7L, ~ range(.x))
    Condition
      Error in `.slide_comp()`:
      ! `range` shouldn't be used on entire `epi_df`s
      x `range`'s first argument was an `epi_df`
      i If you encountered this while trying to take a rolling range of a column using `epi_slide`, you probably forgot to specify the column name (e.g., ~ range(.x$colname)).

