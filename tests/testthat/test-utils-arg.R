test_that("arg_is_scalar basic behavior", {
  expect_no_error(arg_is_scalar(d = 1, "a", 2, c = c("1"), a = list(2)))

  expect_error(arg_is_scalar(c(3, 5, 5)),
    class = "epiprocess__value_not_length_1"
  )

  expect_no_error(arg_is_scalar(NULL, allow_null = TRUE))
  expect_error(arg_is_scalar(NULL),
    class = "epiprocess__value_not_length_1"
  )

  expect_no_error(arg_is_scalar(NA, allow_na = TRUE))
  expect_error(arg_is_scalar(NA),
    class = "epiprocess__value_is_na"
  )
})

test_that("arg_is_numeric basic behavior", {
  expect_no_error(arg_is_numeric(c = 1.25, b = 2:5, 1, c(2.22, 2.12)))

  for (val in list(list(1), "a", list(NULL))) {
    expect_error(arg_is_numeric(val),
      class = "epiprocess__value_is_null_or_not_numeric"
    )
  }

  expect_no_error(arg_is_numeric(1, c(1.255, 2.33, 3), NULL, allow_null = TRUE))
  expect_error(arg_is_numeric(1, c(1.255, 2.33, 3), NULL),
    class = "epiprocess__value_is_null_or_not_numeric"
  )
})

test_that("arg_is_int basic behavior", {
  expect_no_error(arg_is_int(c = 1, 1, 3, b = 2:5))
  expect_no_error(arg_is_int(NULL, 1, allow_null = TRUE))

  for (val in list(1.25, -(1:3))) {
    expect_error(arg_is_int(val),
      class = "epiprocess__some_decimal_or_negative_elements"
    )
  }
})

test_that("arg_is_chr basic behavior", {
  expect_no_error(arg_is_chr(c = c("a", "value"), d = "a", "d"))

  expect_no_error(arg_is_chr(NULL, allow_null = TRUE)) #
  for (val in list(NULL)) {
    expect_error(arg_is_chr(val), #
      class = "epiprocess__value_is_null"
    )
  }

  expect_no_error(arg_is_chr(NA, c(NA, NA, NA), c(NA, "a"), allow_na = TRUE))
  for (val in list(NA, c(NA, NA, NA), c(NA, "a"))) {
    expect_error(arg_is_chr(val),
      class = "epiprocess__some_na_elements"
    )
  }

  expect_no_error(arg_is_chr(c("a", "value"), character(0), list(), allow_empty = TRUE))
  for (val in list(character(0), list())) {
    expect_error(arg_is_chr(val),
      class = "epiprocess__value_length_0"
    )
  }

  for (val in list(c(5, 4), list(5, 4), 5)) {
    expect_error(arg_is_chr(val),
      class = "epiprocess__not_character_type"
    )
  }
})

test_that("arg_is_chr_scalar basic behavior", {
  expect_no_error(arg_is_chr_scalar("a", "b", c = "c"))
  expect_no_error(arg_is_chr_scalar(c = "c"))
})

