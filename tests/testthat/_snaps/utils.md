# as_slide_computation raises errors as expected

    Code
      toy_edf %>% group_by(geo_value) %>% epi_slide(.window_size = 7, mean,
        .col_names = "value")
    Condition
      Error in `epi_slide()`:
      ! `epi_slide` and `epix_slide` do not support `.col_names`; consider:
      * using `epi_slide_mean`, `epi_slide_sum`, or `epi_slide_opt`, if applicable
      * using `.f = ~ .x %>% dplyr::reframe(across(your_col_names, list(your_func_name = your_func)))`

---

    Code
      toy_edf %>% group_by(geo_value) %>% epi_slide(.window_size = 7, tibble(
        slide_value = mean(.x$value)))
    Condition
      Error in `epi_slide()`:
      ! Failed to convert `tibble(slide_value = mean(.x$value))` to a slide computation.
      * If you were trying to use the formula interface, maybe you forgot a tilde at the beginning.
      * If you were trying to use the tidyeval interface, maybe you forgot to specify the name, e.g.: `my_output_col_name =`.  Note that `.col_names` is not supported.
      * If you were trying to use advanced features of the tidyeval interface such as `!! name_variable :=`, maybe you forgot the required leading comma.
      * Something else could have gone wrong; see below.
      Caused by error:
      ! object '.x' not found

---

    Code
      toy_archive %>% epix_slide(tibble(slide_value = mean(.x$value)))
    Condition
      Error in `epix_slide()`:
      ! Failed to convert `.f` to a slide computation.
      * If you were trying to use the formula interface, maybe you forgot a tilde at the beginning.
      * If you were trying to use the tidyeval interface, maybe you forgot to specify the name, e.g.: `my_output_col_name =`.  Note that `.col_names` is not supported.
      * If you were trying to use advanced features of the tidyeval interface such as `!! name_variable :=`, maybe you forgot the required leading comma.
      * Something else could have gone wrong; see below.
      Caused by error:
      ! object '.x' not found

