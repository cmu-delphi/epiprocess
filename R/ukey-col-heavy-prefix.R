#' @export
new_ukey_col_heavyprefix <- function(data) {
  # TODO NOTE

  `class<-`(vctrs::new_vctr(data), c("hardhat_ukey_col_heavyprefix", "vctrs_vctr", class(data)))
}

ukey_col_heavyprefix_get_data <- function(x) {
  heavyprefixed_class <- class(x)
  class(x) <- heavyprefixed_class[-match(c("hardhat_ukey_col_heavyprefix", "vctrs_vctr"), heavyprefixed_class)]
  x
}


# FIXME marker dropping

# vctrs::vec_c(ukey_col_heavyprefix(tibble(g = 1:3) %>% group_by(g)), ukey_col_heavyprefix(tibble(g = 1:3) %>% group_by(g)))

#' @export
is_ukey_col_heavyprefix <- function(x) {
  inherits(x, "hardhat_ukey_col_heavyprefix")
}

#' @export
as_ukey_col_heavyprefix <- function(col) {
  if (is_ukey_col_heavyprefix(col)) {
    col
  } else {
    vctrs::obj_check_vector(col)
    new_ukey_col_heavyprefix(col)
  }
}
# TODO as_ukey_col_heavyprefix, and maybe get rid of ukey_col_heavyprefix

#' @export
as_non_ukey_col_heavyprefix <- function(x) {
  if (is_ukey_col_heavyprefix(x)) {
    ukey_col_heavyprefix_get_data(x)
  } else {
    x
  }
}

# TODO proxy & restore?

#' @export
print.hardhat_ukey_col_heavyprefix <- function(x, ...) {
  vctrs::obj_print(x, ...)
  invisible(x)
}

#' @export
format.hardhat_ukey_col_heavyprefix <- function(x, ...) {
  format(ukey_col_heavyprefix_get_data(x))
}

#' @importFrom vctrs obj_print_data
#' @export
obj_print_data.hardhat_ukey_col_heavyprefix <- function(x, ...) {
  obj_print_data(ukey_col_heavyprefix_get_data(x), ...)
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.hardhat_ukey_col_heavyprefix <- function(x, ...) {
  glue::glue('ukcol<{vec_ptype_abbr(ukey_col_heavyprefix_get_data(x))}>')
}

#' @importFrom vctrs vec_ptype_full
#' @export
vec_ptype_full.hardhat_ukey_col_heavyprefix <- function(x, ...) {
  glue::glue('ukey_col<{vec_ptype_full(ukey_col_heavyprefix_get_data(x))}>')
}

#' @importFrom vctrs vec_ptype2
#' @export
vec_ptype2.hardhat_ukey_col_heavyprefix.hardhat_ukey_col_heavyprefix <- function(x, y, ..., x_arg = "", y_arg = "", call = caller_env()) {
  x_data <- ukey_col_heavyprefix_get_data(x)
  y_data <- ukey_col_heavyprefix_get_data(y)
  if (identical(class(x_data), "Date") && identical(class(y_data), "character")) {
    vctrs::vec_ptype(x) # ukey_col<Date>
  } else if (identical(class(x_data), "character") && identical(class(y_data), "Date")) {
    vctrs::vec_ptype(y) # ukey_col<Date>
  } else {
    new_ukey_col_heavyprefix(vec_ptype2(
      x_data,
      y_data,
      ...,
      x_arg = glue::glue('ukey_col_heavyprefix_get_data({x_arg})'),
      y_arg = glue::glue('ukey_col_heavyprefix_get_data({y_arg})'),
      call = call
    ))
  }
}

# NOTE We can provide blanket implementations for the RHS of `vec_arith`, but
# not for the LHS, and not for other double-dispatch methods like `vec_ptype2`
# and `vec_cast` that are dispatched in vctrs internals rather than using S3. To
# approximate a blanket implementation, we'll provide implementations for
# several common classes.

# Potential auto-conversions from common data types to ukey_col wrappers, e.g.,
# to enable some conveniences with `vec_c`, `bind_rows`, etc.

vec_ptype2_hardhat_ukey_col_heavyprefix_other <- function(x, y, ..., x_arg = "", y_arg = "", call = caller_env()) {
  new_ukey_col_heavyprefix(vec_ptype2(
    ukey_col_heavyprefix_get_data(x),
    y,
    ...,
    x_arg = glue::glue('ukey_col_heavyprefix_get_data({x_arg})'),
    y_arg = glue::glue('{y_arg}'),
    call = call
  ))
}
#' @export
vec_ptype2.hardhat_ukey_col_heavyprefix.integer <- vec_ptype2_hardhat_ukey_col_heavyprefix_other
#' @export
vec_ptype2.hardhat_ukey_col_heavyprefix.double <- vec_ptype2_hardhat_ukey_col_heavyprefix_other
#' @export
vec_ptype2.hardhat_ukey_col_heavyprefix.character <- function(x, y, ..., x_arg = "", y_arg = "", call = caller_env()) {
  # Partial workaround for https://github.com/r-lib/vctrs/issues/967 so `c`
  # behaves more like didn't have marker; not doing for POSIXts due to
  # complexity and potential base/vctrs disagreement.
  x_data <- ukey_col_heavyprefix_get_data(x)
  if (identical(class(x_data), "Date")) {
    vctrs::vec_ptype(x) # ukey_col<Date>
  } else {
    vec_ptype2_hardhat_ukey_col_heavyprefix_other(x, y, ..., x_arg = x_arg, y_arg = y_arg, call = call)
  }
}
#' @export
vec_ptype2.hardhat_ukey_col_heavyprefix.Date <- function(x, y, ..., x_arg = "", y_arg = "", call = caller_env()) {
  # Partial workaround for https://github.com/r-lib/vctrs/issues/967 so `c`
  # behaves more like didn't have marker; not doing for POSIXts due to
  # complexity and potential base/vctrs disagreement.
  x_data <- ukey_col_heavyprefix_get_data(x)
  if (identical(class(x_data), "character")) {
    new_ukey_col_heavyprefix(vctrs::vec_ptype(y)) # ukey_col<Date>
  } else {
    vec_ptype2_hardhat_ukey_col_heavyprefix_other(x, y, ..., x_arg = x_arg, y_arg = y_arg, call = call)
  }
}
#' @export
vec_ptype2.hardhat_ukey_col_heavyprefix.POSIXt <- vec_ptype2_hardhat_ukey_col_heavyprefix_other
#' @export
vec_ptype2.hardhat_ukey_col_heavyprefix.list <- vec_ptype2_hardhat_ukey_col_heavyprefix_other


vec_ptype2_other_hardhat_ukey_col_heavyprefix <- function(x, y, ..., x_arg = "", y_arg = "", call = caller_env()) {
  new_ukey_col_heavyprefix(vec_ptype2(
    x,
    ukey_col_heavyprefix_get_data(y),
    ...,
    x_arg = glue::glue('{x_arg}'),
    y_arg = glue::glue('ukey_col_heavyprefix_get_data({y_arg})'),
    call = call
  ))
}
#' @export
vec_ptype2.integer.hardhat_ukey_col_heavyprefix <- vec_ptype2_other_hardhat_ukey_col_heavyprefix
#' @export
vec_ptype2.double.hardhat_ukey_col_heavyprefix <- vec_ptype2_other_hardhat_ukey_col_heavyprefix
#' @export
vec_ptype2.character.hardhat_ukey_col_heavyprefix <- function(x, y, ..., x_arg = "", y_arg = "", call = caller_env()) {
  y_data <- ukey_col_heavyprefix_get_data(y)
  if (identical(class(y_data), "Date")) {
    vctrs::vec_ptype(y) # ukey_col<Date>
  } else {
    vec_ptype2_other_hardhat_ukey_col_heavyprefix(x, y, ..., x_arg = x_arg, y_arg = y_arg, call = call)
  }
}
#' @export
vec_ptype2.Date.hardhat_ukey_col_heavyprefix <- function(x, y, ..., x_arg = "", y_arg = "", call = caller_env()) {
  y_data <- ukey_col_heavyprefix_get_data(y)
  if (identical(class(y_data), "character")) {
    new_ukey_col_heavyprefix(vctrs::vec_ptype(x)) # ukey_col<Date>
  } else {
    vec_ptype2_other_hardhat_ukey_col_heavyprefix(x, y, ..., x_arg = x_arg, y_arg = y_arg, call = call)
  }
}
#' @export
vec_ptype2.POSIXt.hardhat_ukey_col_heavyprefix <- vec_ptype2_other_hardhat_ukey_col_heavyprefix
#' @export
vec_ptype2.list.hardhat_ukey_col_heavyprefix <- vec_ptype2_other_hardhat_ukey_col_heavyprefix


# Converting between ukey_cols:

#' @importFrom vctrs vec_cast
#' @export
vec_cast.hardhat_ukey_col_heavyprefix.hardhat_ukey_col_heavyprefix <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  x_data <- ukey_col_heavyprefix_get_data(x)
  to_data <- ukey_col_heavyprefix_get_data(to)
  if (identical(class(x_data), "Date") && identical(class(to_data), "character")) {
    result_data <- as.character(x_data)
  } else if (identical(class(x_data), "character") && identical(class(to_data), "Date")) {
    result_data <- as.Date(x_data)
  } else {
    result_data <- vec_cast(
      x_data,
      to_data,
      ...,
      x_arg = glue::glue('ukey_col_heavyprefix_get_data({x_arg})'),
      to_arg = glue::glue('ukey_col_heavyprefix_get_data({to_arg})'),
      call = call
    )
  }
  new_ukey_col_heavyprefix(result_data)
}

# Converting other things to ukey_cols:

vec_cast_hardhat_ukey_col_heavyprefix_other <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  new_ukey_col_heavyprefix(vec_cast(
    x,
    ukey_col_heavyprefix_get_data(to),
    ...,
    x_arg = glue::glue('{x_arg}'),
    to_arg = glue::glue('ukey_col_heavyprefix_get_data({to_arg})'),
    call = call
  ))
}

#' @export
vec_cast.hardhat_ukey_col_heavyprefix.integer <- vec_cast_hardhat_ukey_col_heavyprefix_other
#' @export
vec_cast.hardhat_ukey_col_heavyprefix.double <- vec_cast_hardhat_ukey_col_heavyprefix_other
#' @export
vec_cast.hardhat_ukey_col_heavyprefix.character <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  # Partial workaround for https://github.com/r-lib/vctrs/issues/967 so `c`
  # behaves more like didn't have marker; not doing for POSIXts due to
  # complexity and potential base/vctrs disagreement.
  to_data <- ukey_col_heavyprefix_get_data(to)
  if (identical(class(to_data), "Date")) {
    new_ukey_col_heavyprefix(as.Date(x))
  } else {
    vec_cast_hardhat_ukey_col_heavyprefix_other(x, to, ..., x_arg = x_arg, to_arg = to_arg, call = call)
  }
}
#' @export
vec_cast.hardhat_ukey_col_heavyprefix.Date <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  to_data <- ukey_col_heavyprefix_get_data(to)
  if (identical(class(to_data), "character")) {
    new_ukey_col_heavyprefix(as.character(x))
  } else {
    vec_cast_hardhat_ukey_col_heavyprefix_other(x, to, ..., x_arg = x_arg, to_arg = to_arg, call = call)
  }
}
#' @export
vec_cast.hardhat_ukey_col_heavyprefix.POSIXt <- vec_cast_hardhat_ukey_col_heavyprefix_other
#' @export
vec_cast.hardhat_ukey_col_heavyprefix.list <- vec_cast_hardhat_ukey_col_heavyprefix_other

# Converting ukey_cols to other things:

# NOTE This also makes `as.character` work to drop this wrapper class, which
# makes methods like `toupper` "work", but they will drop the ukey_col wrapper.
# We could override this behavior, but it might make some users unclear on how
# to remove the ukey_col class if they encounter it.

vec_cast_other_hardhat_ukey_col_heavyprefix <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  vec_cast(
    ukey_col_heavyprefix_get_data(x),
    to,
    ...,
    x_arg = glue::glue('ukey_col_heavyprefix_get_data({x_arg})'),
    to_arg = glue::glue('{to_arg}'),
    call = call
  )
}

#' @export
vec_cast.integer.hardhat_ukey_col_heavyprefix <- vec_cast_other_hardhat_ukey_col_heavyprefix
#' @export
vec_cast.double.hardhat_ukey_col_heavyprefix <- vec_cast_other_hardhat_ukey_col_heavyprefix
#' @export
vec_cast.character.hardhat_ukey_col_heavyprefix <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  x_data <- ukey_col_heavyprefix_get_data(x)
  if (identical(class(x_data), "Date")) {
    as.character(x_data)
  } else {
    vec_cast_other_hardhat_ukey_col_heavyprefix(x, to, ..., x_arg = x_arg, to_arg = to_arg, call = call)
  }
}
#' @export
vec_cast.Date.hardhat_ukey_col_heavyprefix <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  x_data <- ukey_col_heavyprefix_get_data(x)
  if (identical(class(x_data), "character")) {
    as.Date(x_data)
  } else {
    vec_cast_other_hardhat_ukey_col_heavyprefix(x, to, ..., x_arg = x_arg, to_arg = to_arg, call = call)
  }
}
#' @export
vec_cast.POSIXt.hardhat_ukey_col_heavyprefix <- vec_cast_other_hardhat_ukey_col_heavyprefix
#' @export
vec_cast.list.hardhat_ukey_col_heavyprefix <- vec_cast_other_hardhat_ukey_col_heavyprefix



# XXX {lubridate} adds `==.Date` impl(!?!) which conflicts with this
# and triggers warnings.  But if we leave base/lubridate Ops as-is,
# then `+` will not re-wrap as a ukey_col.




#' @importFrom vctrs vec_arith
#' @method vec_arith hardhat_ukey_col_heavyprefix
#' @export
vec_arith.hardhat_ukey_col_heavyprefix <- function(op, x, y, ...) {
  UseMethod("vec_arith.hardhat_ukey_col_heavyprefix", y)
}

#' @method vec_arith.hardhat_ukey_col_heavyprefix hardhat_ukey_col_heavyprefix
#' @export
vec_arith.hardhat_ukey_col_heavyprefix.hardhat_ukey_col_heavyprefix <- function(op, x, y, ...) {
  # In some cases, this is nonsensical or buggy. But it could legitimately be
  # used, e.g., to transform ukey columns a and b into a and a+b, or to combine
  # ukey<glue> columns a and b into a single ukey column ab. We can't detect
  # nonsensical or buggy vs. legitimate when working on individual ukey columns
  # without the others.
  new_ukey_col_heavyprefix(vec_arith(op, ukey_col_heavyprefix_get_data(x), ukey_col_heavyprefix_get_data(y), ...))
}

#' @method vec_arith.hardhat_ukey_col_heavyprefix default
#' @export
vec_arith.hardhat_ukey_col_heavyprefix.default <- function(op, x, y, ...) {
  new_ukey_col_heavyprefix(vec_arith(op, ukey_col_heavyprefix_get_data(x), y, ...))
}
