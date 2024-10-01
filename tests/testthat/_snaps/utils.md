# as_slide_computation raises errors as expected

    Code
      toy_edf %>% group_by(geo_value) %>% epi_slide(.window_size = 6, tibble(
        slide_value = mean(.x$value)))
    Condition
      Error in `as_slide_computation()`:
      ! Failed to convert `tibble(slide_value = mean(.x$value))` to a slide computation.
      * If you were trying to use the formula interface, maybe you forgot a tilde at the beginning.
      * If you were trying to use the tidyeval interface, maybe you forgot to specify the name, e.g.: `my_output_col_name =`.
      * If you were trying to use advanced features of the tidyeval interface such as `!! name_variable :=`, you might have forgotten the required leading comma.
      Caused by error:
      ! object '.x' not found

---

    Code
      toy_archive %>% epix_slide(tibble(slide_value = mean(.x$value)))
    Condition
      Error in `as_slide_computation()`:
      ! Failed to convert `.f` to a slide computation.
      * If you were trying to use the formula interface, maybe you forgot a tilde at the beginning.
      * If you were trying to use the tidyeval interface, maybe you forgot to specify the name, e.g.: `my_output_col_name =`.
      * If you were trying to use advanced features of the tidyeval interface such as `!! name_variable :=`, you might have forgotten the required leading comma.
      Caused by error:
      ! object '.x' not found

