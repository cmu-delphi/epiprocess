handle_arg_list <- function(..., tests) {
  values <- list(...)
  names <- eval(substitute(alist(...)))
  names <- purrr::map(names, deparse)

  purrr::walk2(names, values, tests)
}

arg_is_scalar <- function(..., allow_null = FALSE, allow_na = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (length(value) > 1 | (!allow_null & length(value) == 0)) {
        cli::cli_abort("Argument {.val {name}} must be of length 1.",
          class = "epiprocess__value_not_length_1"
        )
      }
      if (!is.null(value)) {
        if (is.na(value) & !allow_na) {
          cli::cli_abort(
            "Argument {.val {name}} must not be a missing value ({.val {NA}}).",
            class = "epiprocess__value_is_na"
          )
        }
      }
    }
  )
}

arg_is_numeric <- function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is.numeric(value) | (is.null(value) & allow_null))) {
        cli::cli_abort("All {.val {name}} must be numeric.",
          class = "epiprocess__value_is_null_or_not_numeric"
        )
      }
    }
  )
}

arg_is_int <- function(..., allow_null = FALSE) {
  arg_is_numeric(..., allow_null = allow_null)
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!( (all(value %% 1 == 0) && all(value > 0)) | (is.null(value) & allow_null))) {
        cli::cli_abort("All {.val {name}} must be whole positive number(s).",
          class = "epiprocess__some_decimal_or_negative_elements"
        )
      }
    }
  )
}

arg_is_chr <- function(..., allow_null = FALSE, allow_na = FALSE, allow_empty = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (is.null(value) & !allow_null) {
        cli::cli_abort("Argument {.val {name}} may not be `NULL`.",
          class = "epiprocess__value_is_null"
        )
      }
      if (any(is.na(value)) & !allow_na) {
        cli::cli_abort("Argument {.val {name}} must not contain any missing values ({.val {NA}}).",
          class = "epiprocess__some_na_elements"
        )
      }
      if (!is.null(value) & (length(value) == 0L & !allow_empty)) {
        cli::cli_abort("Argument {.val {name}} must have length > 0.",
          class = "epiprocess__value_length_0"
        )
      }
      if (!(is.character(value) | is.null(value) | all(is.na(value)))) {
        cli::cli_abort("Argument {.val {name}} must be of character type.",
          class = "epiprocess__not_character_type"
        )
      }
    }
  )
}

arg_is_chr_scalar <- function(..., allow_null = FALSE, allow_na = FALSE) {
  arg_is_chr(..., allow_null = allow_null, allow_na = allow_na)
  arg_is_scalar(..., allow_null = allow_null, allow_na = allow_na)
}
