sample_geos <- list(
  chr = letters,
  fct = factor(letters[c(1,1,2)], letters),
  df = data.frame(
    state = "Rhode Island",
    county = c("Bristol County", "Kent County")
  ),
  tbl = tibble::tibble(
    state = "Rhode Island",
    county = c("Bristol County", "Kent County")
  )
)
sample_times <- list(
  date = as.Date("2020-01-01") + 1:5 - 1,
  ts_yearmonth = tsibble::yearmonth(1:5),
  cl_ymd = clock::year_month_day(2000, 1, 1:5),
  lt = as.POSIXlt(as.Date("2020-01-01") + 1:5 - 1),
  ct = as.POSIXct(as.Date("2020-01-01") + 1:5 - 1),
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

test_that(glue::glue("Can perform ukey(<Date>) + <difftime>, possibly with incompatible methods warning"), {
  col <- sample_times$date
  lhs <- ukey_col_heavyprefix_get_data(as_ukey_col_heavyprefix(col))
  rhs <- as.difftime(1, units = "days")
  expect_identical(
    withCallingHandlers(
      lhs + rhs,
      warning = function(w) {
        if (any(grepl("^Incompatible methods.*for.*\\+.*", conditionMessage(w)))) {
          invokeRestart("muffleWarning")
        }
      }
    ),
    col + as.difftime(1, units = "days")
  )
})

test_that(glue::glue("ukey(<Date>) + <difftime> doesn't generate a warning"), {
  skip("Avoiding warnings here is likely possible, but would require some tricks to maintain an S4 bit")
  col <- sample_times$date
  expect_no_warning(ukey_col_heavyprefix_get_data(as_ukey_col_heavyprefix(col) + as.difftime(1, units = "days")))
})

# TODO more vec_c tests

for (col in c(sample_geos, sample_times)) {
  if (!is.data.frame(col)) { # don't test if `c` isn't `vec_c`-like for underlying `col`
    test_that(glue::glue("Can perform c(ukey(<{class(col)[[1L]]}>), ukey(<{class(col)[[1L]]}>))"), {
      expect_identical(c(as_ukey_col_heavyprefix(col), as_ukey_col_heavyprefix(col)),
                       as_ukey_col_heavyprefix(c(col, col)))
    })
  }
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
  if (!is.data.frame(col)) { # don't test if `c` isn't `vec_c`-like for underlying `col`
    test_that(glue::glue("Can perform c(ukey(<{class(col)[[1L]]}>), <{class(col)[[1L]]}>)"), {
      expect_identical(c(as_ukey_col_heavyprefix(col), col),
                       as_ukey_col_heavyprefix(c(col, col)))
    })
  }
}

for (col in c(sample_geos, sample_times)) {
  # Wrong results may be unavoidable here.  We can trigger S4 dispatch
  # from non-first arguments by setting the S4 bit, but it doesn't
  # help, as its S4 generic's args are `x, ...`, and it does not seem
  # to be possible to define methods that dispatch on elements of
  # `...`, contrary to what ?setMethod claims.  We can't even use rude
  # approaches like defining `c.default` or `c.integer`, as S3
  # dispatch is not attempted if the first argument does not have the
  # object bit set; nor can we define S4 analogues, S4 c "ANY" is
  # already defined and sealed, and S4 c "integer" is also sealed
  # somehow; nor can we redefine the S4 generic, which is sealed.
  if (!is.data.frame(col)) { # don't test if `c` isn't `vec_c`-like for underlying `col`
    test_that(glue::glue("Can perform c(<{class(col)[[1L]]}>, ukey(<{class(col)[[1L]]}>))"), {
      if (!inherits(col, "vctrs_vctr")) {
        skip("failure here is likely unavoidable")
      }
      expect_identical(c(col, as_ukey_col_heavyprefix(col)),
                       as_ukey_col_heavyprefix(c(col, col)))
    })
  }
}

for (col in c(sample_geos, sample_times)) {
  # For some downstream tidyverse functions, we may be fine if
  # `vec_c(other, ukey)` works even if `c(other, ukey)` is bugged.
  test_that(glue::glue("Can perform vec_c(<{class(col)[[1L]]}>, ukey(<{class(col)[[1L]]}>))"), {
    expect_identical(vctrs::vec_c(col, as_ukey_col_heavyprefix(col)),
                     as_ukey_col_heavyprefix(vctrs::vec_c(col, col)))
  })
}

for (do_ukey_date in c(TRUE, FALSE)) {
  for (do_ukey_chr in c(TRUE, FALSE)) {
    if (!do_ukey_date && !do_ukey_chr) {
      next
    }
    for (date_first in c(TRUE, FALSE)) {

      date_col <- if (do_ukey_date) {
        as_ukey_col_heavyprefix(sample_times$date)
      } else {
        sample_times$date
      }
      chr_col <- if (do_ukey_chr) {
        as_ukey_col_heavyprefix(as.character(sample_times$date))
      } else {
        as.character(sample_times$date)
      }
      if (date_first) {
        col <- date_col
        col2 <- chr_col
      } else {
        col <- chr_col
        col2 <- date_col
      }
      # c(chr, ukcol<date>) is likely impossible for the same reasons
      # as c(chr, ukcol<chr>), c(int, ukcol<int>), etc.
      test_that(glue::glue("Can perform c({vctrs::vec_ptype_abbr(col)}, {vctrs::vec_ptype_abbr(col2)})"), {
        if (date_first && !do_ukey_date && do_ukey_chr) {
          skip("c(date, ukcol<chr>) likely doomed to fail")
        }
        if (!date_first && !do_ukey_chr && do_ukey_date) {
          skip("c(chr, ukcol<date>) likely doomed to fail")
        }
        expect_identical(c(col, col2),
                         as_ukey_col_heavyprefix(c(date_col, date_col)))
      })
      test_that(glue::glue("Can perform vec_c({vctrs::vec_ptype_abbr(col)}, {vctrs::vec_ptype_abbr(col2)})"), {
        expect_identical(vec_c(col, col2),
                         as_ukey_col_heavyprefix(c(date_col, date_col)))
      })

    }
  }
}

# TODO is.numeric, inherits, etc.
