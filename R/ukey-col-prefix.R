
#' @export
new_ukey_col_prefix <- function(data) {
  # TODO NOTE

  `class<-`(data, c("hardhat_ukey_col_prefix", class(data)))
}

# FIXME marker dropping

# vctrs::vec_c(ukey_col_prefix(tibble(g = 1:3) %>% group_by(g)), ukey_col_prefix(tibble(g = 1:3) %>% group_by(g)))

#' @export
ukey_col_prefix <- function(col) {
  vctrs::obj_check_vector(col)
  new_ukey_col_prefix(col)
}

#' @export
is_ukey_col_prefix <- function(x) {
  inherits(x, "hardhat_ukey_col_prefix")
}

# TODO proxy & restore?

ukey_col_prefix_get_data <- function(x) {
  class(x) <- setdiff(class(x), "hardhat_ukey_col_prefix")
  x
}

#' @export
format.hardhat_ukey_col_prefix <- function(x, ...) {
  format(ukey_col_prefix_get_data(x))
}

#' @importFrom vctrs obj_print_data
#' @export
obj_print_data.hardhat_ukey_col_prefix <- function(x, ...) {
  print(ukey_col_prefix_get_data(x))
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.hardhat_ukey_col_prefix <- function(x, ...) {
  glue::glue('ukcol<{vec_ptype_abbr(ukey_col_prefix_get_data(x))}>')
}

#' @importFrom vctrs vec_ptype_full
#' @export
vec_ptype_full.hardhat_ukey_col_prefix <- function(x, ...) {
  glue::glue('ukey_col<{vec_ptype_full(ukey_col_prefix_get_data(x))}>')
}

#' @importFrom vctrs vec_ptype2
#' @export
vec_ptype2.hardhat_ukey_col_prefix.hardhat_ukey_col_prefix <- function(x, y, ..., x_arg = "", y_arg = "", call = caller_env()) {
  new_ukey_col_prefix(vec_ptype2(
    ukey_col_prefix_get_data(x),
    ukey_col_prefix_get_data(y),
    ...,
    x_arg = glue::glue('vctrs::field({x_arg}, "data")'),
    y_arg = glue::glue('vctrs::field({y_arg}, "data")'),
    call = call
  ))
}

# vec_ptype2_hardhat_ukey_col_prefix_other <- function(x, y, ..., x_arg = "", y_arg = "", call = caller_env()) {
#   new_ukey_col_prefix(vec_ptype2(
#     ukey_col_prefix_get_data(x),
#     y,
#     ...,
#     x_arg = glue::glue('vctrs::field({x_arg}, "data")'),
#     y_arg = glue::glue('{y_arg}'),
#     call = call
#   ))
# }
# #' @export
# vec_ptype2.hardhat_ukey_col_prefix.integer <- vec_ptype2_hardhat_ukey_col_prefix_other
# #' @export
# vec_ptype2.hardhat_ukey_col_prefix.double <- vec_ptype2_hardhat_ukey_col_prefix_other
# #' @export
# vec_ptype2.hardhat_ukey_col_prefix.character <- vec_ptype2_hardhat_ukey_col_prefix_other
# #' @export
# vec_ptype2.hardhat_ukey_col_prefix.list <- vec_ptype2_hardhat_ukey_col_prefix_other
# #' @export
# vec_ptype2.hardhat_ukey_col_prefix.data.frame <- vec_ptype2_hardhat_ukey_col_prefix_other


# Converting between ukey_cols:

#' @importFrom vctrs vec_cast
#' @export
vec_cast.hardhat_ukey_col_prefix.hardhat_ukey_col_prefix <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  new_ukey_col_prefix(vec_cast(
    ukey_col_prefix_get_data(x),
    ukey_col_prefix_get_data(to),
    ...,
    x_arg = glue::glue('vctrs::field({x_arg}, "data")'),
    to_arg = glue::glue('vctrs::field({to_arg}, "data")'),
    call = call
  ))
}

# Converting other things to ukey_cols:

vec_cast_hardhat_ukey_col_prefix_other <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  new_ukey_col_prefix(vec_cast(
    x,
    ukey_col_prefix_get_data(to),
    ...,
    x_arg = glue::glue('{x_arg}'),
    to_arg = glue::glue('vctrs::field({to_arg}, "data")'),
    call = call
  ))
}

#' @export
vec_cast.hardhat_ukey_col_prefix.integer <- vec_cast_hardhat_ukey_col_prefix_other
#' @export
vec_cast.hardhat_ukey_col_prefix.double <- vec_cast_hardhat_ukey_col_prefix_other
#' @export
vec_cast.hardhat_ukey_col_prefix.character <- vec_cast_hardhat_ukey_col_prefix_other
#' @export
vec_cast.hardhat_ukey_col_prefix.list <- vec_cast_hardhat_ukey_col_prefix_other
#' @export
vec_cast.hardhat_ukey_col_prefix.data.frame <- vec_cast_hardhat_ukey_col_prefix_other

# Converting ukey_cols to other things:

vec_cast_other_hardhat_ukey_col_prefix <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  vec_cast(
    ukey_col_prefix_get_data(x),
    to,
    ...,
    x_arg = glue::glue('vctrs::field({x_arg}, "data")'),
    to_arg = glue::glue('{to_arg}'),
    call = call
  )
}

#' @export
vec_cast.integer.hardhat_ukey_col_prefix <- vec_cast_other_hardhat_ukey_col_prefix
#' @export
vec_cast.double.hardhat_ukey_col_prefix <- vec_cast_other_hardhat_ukey_col_prefix
#' @export
vec_cast.character.hardhat_ukey_col_prefix <- vec_cast_other_hardhat_ukey_col_prefix
#' @export
vec_cast.list.hardhat_ukey_col_prefix <- vec_cast_other_hardhat_ukey_col_prefix
#' @export
vec_cast.data.frame.hardhat_ukey_col_prefix <- vec_cast_other_hardhat_ukey_col_prefix


#' @importFrom vctrs vec_arith
#' @method vec_arith hardhat_ukey_col_prefix
#' @export
vec_arith.hardhat_ukey_col_prefix <- function(op, x, y, ...) {
  UseMethod("vec_arith.hardhat_ukey_col_prefix", y)
}

#' @method vec_arith.hardhat_ukey_col_prefix hardhat_ukey_col_prefix
#' @export
vec_arith.hardhat_ukey_col_prefix.hardhat_ukey_col_prefix <- function(op, x, y, ...) {
  # In some cases, this is nonsensical or buggy. But it could legitimately be
  # used, e.g., to transform ukey columns a and b into a and a+b, or to combine
  # ukey<glue> columns a and b into a single ukey column ab. We can't detect
  # nonsensical or buggy vs. legitimate when working on individual ukey columns
  # without the others.
  new_ukey_col_prefix(vec_arith(op, ukey_col_prefix_get_data(x), ukey_col_prefix_get_data(y), ...))
}

#' @method vec_arith.hardhat_ukey_col_prefix default
#' @export
vec_arith.hardhat_ukey_col_prefix.default <- function(op, x, y, ...) {
  new_ukey_col_prefix(vec_arith(op, ukey_col_prefix_get_data(x), y, ...))
}

# FIXME we can't just forward to vctrs methods in prefix approach
# because are likely not prefixing a vctrs_vctr and vctrs generics for
# non-vctrs_vctrs don't actually work in some cases.  Need to delegate
# to parent class method... perhaps can just NextMethod() for both Ops
# and vec_arith?
