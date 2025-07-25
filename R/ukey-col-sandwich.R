
#' @export
new_ukey_col_sandwich <- function(data) {
  # TODO NOTE

  # TODO remove existing vctrs_vctr?

  # disguised_data_class <- vctrs::vec_set_difference(
  #   class(data),
  #   "data.frame"
  # )
  # disguised_data <- data
  # class(disguised_data) <- disguised_data_class
  vctrs::new_vctr(
    # disguised_data,
    data,
    # XXX feeding length>1 to class arg doesn't seem supported
    # according to docs, but effect seems similar to what new_rcrd
    # does
    class = c("hardhat_ukey_col_sandwich", class(data)),
    "hardhat:::data_class" = class(data)
  )
}

# FIXME data can't be a data frame... doesn't seem like there's an
# easy way around this.

#' @export
ukey_col_sandwich_get_data <- function(x) {
  # FIXME don't remove all vctrs_vctr if base was vctrs_vctr
  class(x) <- attr(x, "hardhat:::data_class")
  attr(x, "hardhat:::data_class") <- NULL
  x
}

#' @export
is_ukey_col_sandwich <- function(x) {
  inherits(x, "hardhat_ukey_col_sandwich")
}

#' @export
as_ukey_col_sandwich <- function(col) {
  vctrs::obj_check_vector(col)
  if (is_ukey_col_sandwich(col)) {
    col
  } else {
    new_ukey_col_sandwich(col)
  }
}


# TODO proxy & restore?

#' @importFrom vctrs vec_proxy
#' @export
vec_proxy.ukey_col_sandwich <- function(x, ...) {
  vec_proxy(ukey_col_sandwich_get_data(x), ...)
}

#' @importFrom vctrs vec_restore
#' @export
vec_restore.ukey_col_sandwich <- function(x, to, ...) {
  to_data <- ukey_col_sandwich_get_data(to)
  if (is.data.frame(to_data)) {
    new_ukey_col_sandwich(x)
  } else {
    new_ukey_col_sandwich(vec_restore(x, to_data, ...))
  }
}

#' @export
print.hardhat_ukey_col_sandwich <- function(x, ...) {
  vctrs::obj_print(x, ...)
  invisible(x)
}

#' @export
format.hardhat_ukey_col_sandwich <- function(x, ...) {
  format(ukey_col_sandwich_get_data(x))
}

#' @importFrom vctrs obj_print_data
#' @export
obj_print_data.hardhat_ukey_col_sandwich <- function(x, ...) {
  obj_print_data(ukey_col_sandwich_get_data(x), ...)
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.hardhat_ukey_col_sandwich <- function(x, ...) {
  glue::glue('ukcol<{vec_ptype_abbr(ukey_col_sandwich_get_data(x))}>')
}

#' @importFrom vctrs vec_ptype_full
#' @export
vec_ptype_full.hardhat_ukey_col_sandwich <- function(x, ...) {
  glue::glue('ukey_col<{vec_ptype_full(ukey_col_sandwich_get_data(x))}>')
}





#' @importFrom vctrs vec_ptype2
#' @export
vec_ptype2.hardhat_ukey_col_sandwich.hardhat_ukey_col_sandwich <- function(x, y, ..., x_arg = "", y_arg = "", call = caller_env()) {
  new_ukey_col_sandwich(vec_ptype2(
    ukey_col_sandwich_get_data(x),
    ukey_col_sandwich_get_data(y),
    ...,
    x_arg = glue::glue('ukey_col_sandwich_get_data({x_arg})'),
    y_arg = glue::glue('ukey_col_sandwich_get_data({y_arg})'),
    call = call
  ))
}

# NOTE We can provide blanket implementations for the RHS of `vec_arith`, but
# not for the LHS, and not for other double-dispatch methods like `vec_ptype2`
# and `vec_cast` that are dispatched in vctrs internals rather than using S3. To
# approximate a blanket implementation, we'll provide implementations for
# several common classes.

# Potential auto-conversions from common data types to ukey_col wrappers, e.g.,
# to enable some conveniences with `vec_c`, `bind_rows`, etc.

vec_ptype2_hardhat_ukey_col_sandwich_other <- function(x, y, ..., x_arg = "", y_arg = "", call = caller_env()) {
  new_ukey_col_sandwich(vec_ptype2(
    ukey_col_sandwich_get_data(x),
    y,
    ...,
    x_arg = glue::glue('ukey_col_sandwich_get_data({x_arg})'),
    y_arg = glue::glue('{y_arg}'),
    call = call
  ))
}
#' @export
vec_ptype2.hardhat_ukey_col_sandwich.integer <- vec_ptype2_hardhat_ukey_col_sandwich_other
#' @export
vec_ptype2.hardhat_ukey_col_sandwich.double <- vec_ptype2_hardhat_ukey_col_sandwich_other
#' @export
vec_ptype2.hardhat_ukey_col_sandwich.character <- function(x, y, ..., x_arg = "", y_arg = "", call = caller_env()) {
  # Partial workaround for https://github.com/r-lib/vctrs/issues/967 so `c`
  # behaves more like didn't have marker; not doing for POSIXts due to
  # complexity and potential base/vctrs disagreement.
  x_data <- ukey_col_sandwich_get_data(x)
  if (identical(class(x_data), "Date")) {
    vctrs::vec_ptype(x) # ukey_col<Date>
  } else {
    vec_ptype2_hardhat_ukey_col_sandwich_other(x, y, ..., x_arg = x_arg, y_arg = y_arg, call = call)
  }
}
#' @export
vec_ptype2.hardhat_ukey_col_sandwich.Date <- function(x, y, ..., x_arg = "", y_arg = "", call = caller_env()) {
  # Partial workaround for https://github.com/r-lib/vctrs/issues/967 so `c`
  # behaves more like didn't have marker; not doing for POSIXts due to
  # complexity and potential base/vctrs disagreement.
  x_data <- ukey_col_sandwich_get_data(x)
  if (identical(class(x_data), "character")) {
    new_ukey_col_sandwich(vctrs::vec_ptype(y)) # ukey_col<Date>
  } else {
    vec_ptype2_hardhat_ukey_col_sandwich_other(x, y, ..., x_arg = x_arg, y_arg = y_arg, call = call)
  }
}
#' @export
vec_ptype2.hardhat_ukey_col_sandwich.POSIXt <- vec_ptype2_hardhat_ukey_col_sandwich_other
#' @export
vec_ptype2.hardhat_ukey_col_sandwich.list <- vec_ptype2_hardhat_ukey_col_sandwich_other
#' @export
vec_ptype2.hardhat_ukey_col_sandwich.data.frame <- vec_ptype2_hardhat_ukey_col_sandwich_other
#' @export
vec_ptype2.hardhat_ukey_col_sandwich.vctrs_vctr <- vec_ptype2_hardhat_ukey_col_sandwich_other


vec_ptype2_other_hardhat_ukey_col_sandwich <- function(x, y, ..., x_arg = "", y_arg = "", call = caller_env()) {
  new_ukey_col_sandwich(vec_ptype2(
    x,
    ukey_col_sandwich_get_data(y),
    ...,
    x_arg = glue::glue('{x_arg}'),
    y_arg = glue::glue('ukey_col_sandwich_get_data({y_arg})'),
    call = call
  ))
}
#' @export
vec_ptype2.integer.hardhat_ukey_col_sandwich <- vec_ptype2_other_hardhat_ukey_col_sandwich
#' @export
vec_ptype2.double.hardhat_ukey_col_sandwich <- vec_ptype2_other_hardhat_ukey_col_sandwich
#' @export
vec_ptype2.character.hardhat_ukey_col_sandwich <- function(x, y, ..., x_arg = "", y_arg = "", call = caller_env()) {
  y_data <- ukey_col_sandwich_get_data(y)
  if (identical(class(y_data), "Date")) {
    vctrs::vec_ptype(y) # ukey_col<Date>
  } else {
    vec_ptype2_other_hardhat_ukey_col_sandwich(x, y, ..., x_arg = x_arg, y_arg = y_arg, call = call)
  }
}
#' @export
vec_ptype2.Date.hardhat_ukey_col_sandwich <- function(x, y, ..., x_arg = "", y_arg = "", call = caller_env()) {
  y_data <- ukey_col_sandwich_get_data(y)
  if (identical(class(y_data), "character")) {
    new_ukey_col_sandwich(vctrs::vec_ptype(x)) # ukey_col<Date>
  } else {
    vec_ptype2_other_hardhat_ukey_col_sandwich(x, y, ..., x_arg = x_arg, y_arg = y_arg, call = call)
  }
}
#' @export
vec_ptype2.POSIXt.hardhat_ukey_col_sandwich <- vec_ptype2_other_hardhat_ukey_col_sandwich
#' @export
vec_ptype2.list.hardhat_ukey_col_sandwich <- vec_ptype2_other_hardhat_ukey_col_sandwich
#' @export
vec_ptype2.data.frame.hardhat_ukey_col_sandwich <- vec_ptype2_other_hardhat_ukey_col_sandwich
#' @export
vec_ptype2.vctrs_vctr.hardhat_ukey_col_sandwich <- vec_ptype2_other_hardhat_ukey_col_sandwich

# FIXME these vctrs_vctr impls aren't actually inherited with vctrs
# internal double dispatch; they seem pointless


# Converting between ukey_cols:

#' @importFrom vctrs vec_cast
#' @export
vec_cast.hardhat_ukey_col_sandwich.hardhat_ukey_col_sandwich <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  x_data <- ukey_col_sandwich_get_data(x)
  to_data <- ukey_col_sandwich_get_data(to)
  if (identical(class(x_data), "Date") && identical(class(to_data), "character")) {
    result_data <- as.character(x_data)
  } else if (identical(class(x_data), "character") && identical(class(to_data), "Date")) {
    result_data <- as.Date(x_data)
  } else {
    result_data <- vec_cast(
      x_data,
      to_data,
      ...,
      x_arg = glue::glue('ukey_col_sandwich_get_data({x_arg})'),
      to_arg = glue::glue('ukey_col_sandwich_get_data({to_arg})'),
      call = call
    )
  }
  new_ukey_col_sandwich(result_data)
}

# Converting other things to ukey_cols:

vec_cast_hardhat_ukey_col_sandwich_other <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  new_ukey_col_sandwich(vec_cast(
    x,
    ukey_col_sandwich_get_data(to),
    ...,
    x_arg = glue::glue('{x_arg}'),
    to_arg = glue::glue('ukey_col_sandwich_get_data({to_arg})'),
    call = call
  ))
}

#' @export
vec_cast.hardhat_ukey_col_sandwich.integer <- vec_cast_hardhat_ukey_col_sandwich_other
#' @export
vec_cast.hardhat_ukey_col_sandwich.double <- vec_cast_hardhat_ukey_col_sandwich_other
#' @export
vec_cast.hardhat_ukey_col_sandwich.character <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  # Partial workaround for https://github.com/r-lib/vctrs/issues/967 so `c`
  # behaves more like didn't have marker; not doing for POSIXts due to
  # complexity and potential base/vctrs disagreement.
  to_data <- ukey_col_sandwich_get_data(to)
  if (identical(class(to_data), "Date")) {
    new_ukey_col_sandwich(as.Date(x))
  } else {
    vec_cast_hardhat_ukey_col_sandwich_other(x, to, ..., x_arg = x_arg, to_arg = to_arg, call = call)
  }
}
#' @export
vec_cast.hardhat_ukey_col_sandwich.Date <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  to_data <- ukey_col_sandwich_get_data(to)
  if (identical(class(to_data), "character")) {
    new_ukey_col_sandwich(as.character(x))
  } else {
    vec_cast_hardhat_ukey_col_sandwich_other(x, to, ..., x_arg = x_arg, to_arg = to_arg, call = call)
  }
}
#' @export
vec_cast.hardhat_ukey_col_sandwich.POSIXt <- vec_cast_hardhat_ukey_col_sandwich_other
#' @export
vec_cast.hardhat_ukey_col_sandwich.list <- vec_cast_hardhat_ukey_col_sandwich_other
#' @export
vec_cast.hardhat_ukey_col_sandwich.data.frame <- vec_cast_hardhat_ukey_col_sandwich_other
#' @export
vec_cast.hardhat_ukey_col_sandwich.vctrs_vctr <- vec_cast_hardhat_ukey_col_sandwich_other

# Converting ukey_cols to other things:

# NOTE This also makes `as.character` work to drop this wrapper class, which
# makes methods like `toupper` "work", but they will drop the ukey_col wrapper.
# We could override this behavior, but it might make some users unclear on how
# to remove the ukey_col class if they encounter it.

vec_cast_other_hardhat_ukey_col_sandwich <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  vec_cast(
    ukey_col_sandwich_get_data(x),
    to,
    ...,
    x_arg = glue::glue('ukey_col_sandwich_get_data({x_arg})'),
    to_arg = glue::glue('{to_arg}'),
    call = call
  )
}

#' @export
vec_cast.integer.hardhat_ukey_col_sandwich <- vec_cast_other_hardhat_ukey_col_sandwich
#' @export
vec_cast.double.hardhat_ukey_col_sandwich <- vec_cast_other_hardhat_ukey_col_sandwich
#' @export
vec_cast.character.hardhat_ukey_col_sandwich <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  x_data <- ukey_col_sandwich_get_data(x)
  if (identical(class(x_data), "Date")) {
    as.character(x_data)
  } else {
    vec_cast_other_hardhat_ukey_col_sandwich(x, to, ..., x_arg = x_arg, to_arg = to_arg, call = call)
  }
}
#' @export
vec_cast.Date.hardhat_ukey_col_sandwich <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  x_data <- ukey_col_sandwich_get_data(x)
  if (identical(class(x_data), "character")) {
    as.Date(x_data)
  } else {
    vec_cast_other_hardhat_ukey_col_sandwich(x, to, ..., x_arg = x_arg, to_arg = to_arg, call = call)
  }
}
#' @export
vec_cast.POSIXt.hardhat_ukey_col_sandwich <- vec_cast_other_hardhat_ukey_col_sandwich
#' @export
vec_cast.list.hardhat_ukey_col_sandwich <- vec_cast_other_hardhat_ukey_col_sandwich
#' @export
vec_cast.data.frame.hardhat_ukey_col_sandwich <- vec_cast_other_hardhat_ukey_col_sandwich
#' @export
vec_cast.vctrs_vctr.hardhat_ukey_col_sandwich <- vec_cast_other_hardhat_ukey_col_sandwich





#' @importFrom vctrs vec_arith
#' @method vec_arith hardhat_ukey_col_sandwich
#' @export
vec_arith.hardhat_ukey_col_sandwich <- function(op, x, y, ...) {
  UseMethod("vec_arith.hardhat_ukey_col_sandwich", y)
}

#' @method vec_arith.hardhat_ukey_col_sandwich hardhat_ukey_col_sandwich
#' @export
vec_arith.hardhat_ukey_col_sandwich.hardhat_ukey_col_sandwich <- function(op, x, y, ...) {
  # In some cases, this is nonsensical or buggy. But it could legitimately be
  # used, e.g., to transform ukey columns a and b into a and a+b, or to combine
  # ukey<glue> columns a and b into a single ukey column ab. We can't detect
  # nonsensical or buggy vs. legitimate when working on individual ukey columns
  # without the others.
  new_ukey_col_sandwich(vec_arith(op, ukey_col_sandwich_get_data(x), ukey_col_sandwich_get_data(y), ...))
}

#' @method vec_arith.hardhat_ukey_col_sandwich default
#' @export
vec_arith.hardhat_ukey_col_sandwich.default <- function(op, x, y, ...) {
  new_ukey_col_sandwich(vec_arith(op, ukey_col_sandwich_get_data(x), y, ...))
}



# NOTE it might be nice to have a ptype2 for ukey_cols and the ptype they are
# matching; however, it would be tricky or impossible to properly implement
# ptype2s for everything up the coercion hierarchy, since even if doing
# something in vec_ptype2.hardhat_ukey_col_sandwich is possible, there are still the
# vec_ptype2.*.hardhat_ukey_col_sandwich methods to consider. It may be possible to
# register and dynamically update (but not in time? unless there is a way to add
# hooks for loading additional packages... or maybe with setGeneric, though then
# there may not be a need to actually go through with this registration) a set
# of such implementations using `methods` and `s3_register`. For now, rather
# than mess up the vctrs coercion hierarchy, just require things to be properly
# marked/not.

# #' @method vec_castq.hardhat_ukey_col_sandwich hardhat_ukey_col_sandwich
# #' @export
# vec_castq.hardhat_ukey_col_sandwich.hardhat_ukey_col_sandwich <- function(x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
#   new_ukey_col_sandwich(vec_castq(
#     ukey_col_sandwich_get_data(x),
#     ukey_col_sandwich_get_data(to),
#     ...,
#     x_arg = glue::glue('vctrs::field({x_arg}, "data")'),
#     to_arg = glue::glue('vctrs::field({to_arg}, "data")'),
#     call = call
#   ))
# }


# ukey_col_class_vec <- class(ukey_col(1:3))
# ukey_col_class_vec_with_package <- `attr<-`(ukey_col_class_vec, "package", "epiprocess")
# obj <- asS4(`attr<-`(`class<-`(ukey_col(1:3), ukey_col_class_vec_with_package), ".S3Class", ukey_col_class_vec))
# tbl <- tibble::tibble(obj = obj)
# saveRDS(tbl, "testtblcontanings4.rds")

# readRDS("testtblcontanings4.rds") # doesn't load

# readRDS("testtblcontanings4.rds")$obj # loads.... from `print` / `format`? not sure what.  also not sure if will be sufficient....  esp if not calling .onLoad every time... check

# #' @importFrom vctrs vec_cast
# #' @method vec_cast hardhat_ukey_col_sandwich
# #' @export
# vec_cast.hardhat_ukey_col_sandwich <- function(x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
#   print("AAAA")
#   UseMethod("vec_cast.hardhat_ukey_col_sandwich", x)
# }


# # #' @importFrom vctrs vec_cast
# # #' @method vec_cast.hardhat_ukey_col_sandwich default
# #' @method vec_cast hardhat_ukey_col_sandwich.default
# #' @export
# vec_cast.hardhat_ukey_col_sandwich.default <- function(x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
#   stop("BBBB")
# }

# # setOldClass(c("hardhat_ukey_col_sandwich", "vctrs_rcrd", "vctrs_vctr"))
# setOldClass(class(ukey_col(1:5)))
# # XXX setGeneric & setMethod will slow down unrelated vec_cast operations... potentially... try actually installing instead of dev_loading
# # --- actually doesn't slow down unrelated stuff when installed!  however, we do take a hit within the package; e.g., bench_date_cast below.
# #
# # ... but help(Methods_for_Nongenerics) suggests problems when others try to use ours... yes, when installed, vec_cast doesn't actually use our method...
# #
# # library(vctrs); date = Sys.Date(); bench::mark(vec_cast(date, date), min_time = 5, max_iterations = 1e9)
# setGeneric("vec_cast")
# setMethod(
#   "vec_cast", c("hardhat_ukey_col_sandwich", "hardhat_ukey_col_sandwich"),
#   function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
#     print("S4!!!")
#   }
# )

# #' @export
# bench_date_cast <- function() {
#   library(vctrs); date = Sys.Date(); bench::mark(vec_cast(date, date), min_time = 5, max_iterations = 1e9)
# }

# TODO setting S4 flag on hardhat_ukey_col_sandwich, even though apparently the above
# works for vec_cast... will be needed for

# TODO ptype stuff

# TODO S4 trick for difftime ...


# #' @export
# asdf <- function(x, to) {
#   UseMethod("asdf", to)
# }
# #' @method asdf hardhat_ukey_col_sandwich
# #' @export
# asdf.hardhat_ukey_col_sandwich <- function(x, to) UseMethod("asdf.hardhat_ukey_col_sandwich", x)
# #' @method asdf.hardhat_ukey_col_sandwich hardhat_ukey_col_sandwich
# #' @export
# asdf.hardhat_ukey_col_sandwich.hardhat_ukey_col_sandwich <- function(x, to) print("AAA")
# #' @method asdf.hardhat_ukey_col_sandwich default
# #' @export
# asdf.hardhat_ukey_col_sandwich.default <- function(x, to) print("BBB")

# #' @method str hardhat_ukey_col_sandwich
# #' @export
# str.hardhat_ukey_col_sandwich <- function(object, object2, ...) {
#   UseMethod("str.hardhat_ukey_col_sandwich", object2)
# }

# #' @method str hardhat_ukey_col_sandwich
# #' @export
# str.hardhat_ukey_col_sandwich <- function(object, object2, ...) {
#   UseMethod("str.hardhat_ukey_col_sandwich", object2)
# }

# #' @method str.hardhat_ukey_col_sandwich default
# #' @export
# str.hardhat_ukey_col_sandwich.default <- function(object, object2, ...) {
#   print("EEE")
# }

# FIXME issues with Date arith precedence stripping class
