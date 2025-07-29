sample_geos <- list(
  chr = letters#,
  # df = data.frame(
  #   state = "Rhode Island",
  #   county = c("Bristol County", "Kent County")
  # ),
  # tbl = tibble::tibble(
  #   state = "Rhode Island",
  #   county = c("Bristol County", "Kent County")
  # )
)
sample_times <- list(
  date = as.Date("2020-01-01") + 1:5 - 1,
  ts_yearmonth = tsibble::yearmonth(1:5),
  # cl_ymd = clock::year_month_day(2000, 1, 1:5),
  int = 1:5
)

for (col in c(sample_geos, sample_times)) {
  test_that(glue::glue("Can mark&unmark {class(col)[[1L]]} as ukey col"), {
    expect_identical(ukey_col_heavyprefix_get_data(as_ukey_col_heavyprefix(col)),
                     col)
  })
}

for (col in sample_times) {
  if (!inherits(try(col + 1, silent = TRUE), "try-error")) { # exclude, e.g., clock ymd
    test_that(glue::glue("Can perform ukey(<{class(col)[[1L]]}>) + <integerish numeric>"), {
      # expect_equal makes this a bit forgiving about numeric vs. integer
      expect_equal(ukey_col_heavyprefix_get_data(as_ukey_col_heavyprefix(col) + 1),
                   col + 1)
    })
  }
}

test_that(glue::glue("Can perform ukey(<Date>) + <difftime>"), {
  # Eliminating abort/warning/mistake may require some tricks to maintain an S4 bit.
  col <- sample_times$date
  expect_identical(
    ukey_col_heavyprefix_get_data(as_ukey_col_heavyprefix(col) + as.difftime(1, units = "days")),
    col + as.difftime(1, units = "days")
  )
})

for (col in c(sample_geos, sample_times)) {
  test_that(glue::glue("Can perform c(ukey(<{class(col)[[1L]]}>), ukey(<{class(col)[[1L]]}>))"), {
    expect_identical(c(as_ukey_col_heavyprefix(col), as_ukey_col_heavyprefix(col)),
                     as_ukey_col_heavyprefix(c(col, col)))
  })
}

test_that(glue::glue("Can perform c(ukey(<Date>), ukey(<chr>))"), {
  col <- sample_times$date
  col2 <- as.character(sample_times$date)
  expect_identical(c(as_ukey_col_heavyprefix(col), as_ukey_col_heavyprefix(col2)),
                   as_ukey_col_heavyprefix(c(col, col2)))
})

test_that(glue::glue("Can perform c(ukey(<chr>), ukey(<Date>))"), {
  col <- as.character(sample_times$date)
  col2 <- sample_times$date
  expect_identical(c(as_ukey_col_heavyprefix(col), as_ukey_col_heavyprefix(col2)),
                   as_ukey_col_heavyprefix(c(col2[0], col, col2)))
})

for (col in c(sample_geos, sample_times)) {
  test_that(glue::glue("Can perform c(ukey(<{class(col)[[1L]]}>), <{class(col)[[1L]]}>)"), {
    expect_identical(c(as_ukey_col_heavyprefix(col), col),
                     as_ukey_col_heavyprefix(c(col, col)))
  })
}

for (col in c(sample_geos, sample_times)) {
  test_that(glue::glue("Can perform c(<{class(col)[[1L]]}>, ukey(<{class(col)[[1L]]}>))"), {
    expect_identical(c(col, as_ukey_col_heavyprefix(col)),
                     as_ukey_col_heavyprefix(c(col, col)))
  })
}

for (col in c(sample_geos, sample_times)) {
  # For some downstream tidyverse functions, we may be fine if
  # `vec_c(other, ukey)` works even if `c(other, ukey)` is bugged.
  test_that(glue::glue("Can perform vec_c(<{class(col)[[1L]]}>, ukey(<{class(col)[[1L]]}>))"), {
    expect_identical(vctrs::vec_c(col, as_ukey_col_heavyprefix(col)),
                     as_ukey_col_heavyprefix(c(col, col)))
  })
}

for (do_ukey_date in c(TRUE, FALSE)) {
  date_col <- if (do_ukey_date) {
    as_ukey_col_heavyprefix(sample_times$date)
  } else {
    sample_times$date
  }
  for (do_ukey_chr in c(TRUE, FALSE)) {
    if (!do_ukey_date && !do_ukey_chr) {
      next
    }
    chr_col <- if (do_ukey_chr) {
      as_ukey_col_heavyprefix(as.character(sample_times$date))
    } else {
      as.character(sample_times$date)
    }
    for (transpose in c(TRUE, FALSE)) {
      if (transpose) {
        col <- chr_col
        col2 <- date_col
      } else {
        col <- date_col
        col2 <- chr_col
      }
      test_that(glue::glue("Can perform c({vctrs::vec_ptype_abbr(col)}, {vctrs::vec_ptype_abbr(col2)})"), {
        expect_identical(c(col, col2),
                         as_ukey_col_heavyprefix(c(date_col, date_col)))
      })
    }
  }
}

# TODO is.numeric, inherits, etc.
