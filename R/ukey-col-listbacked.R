
#' @export
new_ukey_col_listbacked <- function(data) {
  # NOTE the format here is critical to storing the original class (e.g., Date)
  # and obtaining certain S3 dispatch behavior (e.g., hard error rather than
  # invalid results for `+ <difftime>`, not dropping the marker class in other
  # binary operations, avoiding dispatch to any of `data`'s methods that will
  # reconstruct it without the marker).
  vctrs::new_rcrd(list(data = data), class = "hardhat_ukey_col_listbacked")
}

# XXX is.numeric(ukey_col_listbacked(1:3)) is FALSE

#' @export
ukey_col_listbacked <- function(col) {
  vctrs::obj_check_vector(col)
  new_ukey_col_listbacked(col)
}

# TODO consider whether this should just be as_ukey_col_listbacked

# XXX list-backed can't compose with other marker classes without mutual
# awareness. S3-ification of these conversion/validation functions should help
# make this possible in downstream packages, but will still require awareness
# and work,

#' @export
is_ukey_col_listbacked <- function(x) {
  UseMethod("is_ukey_col_listbacked")
}

#' @export
is_ukey_col_listbacked.default <- function(x) {
  inherits(x, "hardhat_ukey_col_listbacked")
}

#' Make `x` satisfy `is_ukey_col_listbacked`, if it isn't already
#'
#' @export
as_ukey_col_listbacked <- function(x, ...) {
  UseMethod("as_ukey_col_listbacked")
}
# FIXME for coherence, this should probably be used in the vec_cast impls

#' @export
as_ukey_col_listbacked.default <- function(x, ...) {
  rlang::check_dots_empty0(...)
  if (is_ukey_col_listbacked(x)) {
    x
  } else {
    ukey_col_listbacked(x)
  }
}

#' Turn an object from being `is_ukey_col_listbacked` to not
#' @export
decay_ukey_col_listbacked <- function(x) {
  UseMethod("decay_ukey_col_listbacked")
}

#' @export
decay_ukey_col_listbacked.hardhat_ukey_col_listbacked <- function(x) {
  vctrs::field(x, "data")
}

#' If `x` `is_ukey_col_listbacked`, make it not be
#'
#' @export
as_non_ukey_col_listbacked <- function(x) {
  if (is_ukey_col_listbacked(x)) {
    decay_ukey_col_listbacked(x)
  } else {
    x
  }
}


#' @export
format.hardhat_ukey_col_listbacked <- function(x, ...) {
  format(vctrs::field(x, "data"))
}

#' @importFrom vctrs obj_print_data
#' @export
obj_print_data.hardhat_ukey_col_listbacked <- function(x, ...) {
  print(vctrs::field(x, "data"))
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.hardhat_ukey_col_listbacked <- function(x, ...) {
  glue::glue('ukcol<{vec_ptype_abbr(vctrs::field(x, "data"))}>')
}

#' @importFrom vctrs vec_ptype_full
#' @export
vec_ptype_full.hardhat_ukey_col_listbacked <- function(x, ...) {
  glue::glue('ukey_col<{vec_ptype_full(vctrs::field(x, "data"))}>')
}

#' @importFrom vctrs vec_ptype2
#' @export
vec_ptype2.hardhat_ukey_col_listbacked.hardhat_ukey_col_listbacked <- function(x, y, ..., x_arg = "", y_arg = "", call = caller_env()) {
  new_ukey_col_listbacked(vec_ptype2(
    vctrs::field(x, "data"),
    vctrs::field(y, "data"),
    ...,
    x_arg = glue::glue('vctrs::field({x_arg}, "data")'),
    y_arg = glue::glue('vctrs::field({y_arg}, "data")'),
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

vec_ptype2_hardhat_ukey_col_listbacked_other <- function(x, y, ..., x_arg = "", y_arg = "", call = caller_env()) {
  new_ukey_col_listbacked(vec_ptype2(
    vctrs::field(x, "data"),
    y,
    ...,
    x_arg = glue::glue('vctrs::field({x_arg}, "data")'),
    y_arg = glue::glue('{y_arg}'),
    call = call
  ))
}
#' @export
vec_ptype2.hardhat_ukey_col_listbacked.integer <- vec_ptype2_hardhat_ukey_col_listbacked_other
#' @export
vec_ptype2.hardhat_ukey_col_listbacked.double <- vec_ptype2_hardhat_ukey_col_listbacked_other
#' @export
vec_ptype2.hardhat_ukey_col_listbacked.character <- vec_ptype2_hardhat_ukey_col_listbacked_other
#' @export
vec_ptype2.hardhat_ukey_col_listbacked.list <- vec_ptype2_hardhat_ukey_col_listbacked_other
#' @export
vec_ptype2.hardhat_ukey_col_listbacked.data.frame <- vec_ptype2_hardhat_ukey_col_listbacked_other
#' @export
vec_ptype2.hardhat_ukey_col_listbacked.vctrs_vctr <- vec_ptype2_hardhat_ukey_col_listbacked_other


# Converting between ukey_cols:

#' @importFrom vctrs vec_cast
#' @export
vec_cast.hardhat_ukey_col_listbacked.hardhat_ukey_col_listbacked <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  new_ukey_col_listbacked(vec_cast(
    vctrs::field(x, "data"),
    vctrs::field(to, "data"),
    ...,
    x_arg = glue::glue('vctrs::field({x_arg}, "data")'),
    to_arg = glue::glue('vctrs::field({to_arg}, "data")'),
    call = call
  ))
}

# Converting other things to ukey_cols:

vec_cast_hardhat_ukey_col_listbacked_other <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  new_ukey_col_listbacked(vec_cast(
    x,
    vctrs::field(to, "data"),
    ...,
    x_arg = glue::glue('{x_arg}'),
    to_arg = glue::glue('vctrs::field({to_arg}, "data")'),
    call = call
  ))
}

#' @export
vec_cast.hardhat_ukey_col_listbacked.integer <- vec_cast_hardhat_ukey_col_listbacked_other
#' @export
vec_cast.hardhat_ukey_col_listbacked.double <- vec_cast_hardhat_ukey_col_listbacked_other
#' @export
vec_cast.hardhat_ukey_col_listbacked.character <- vec_cast_hardhat_ukey_col_listbacked_other
#' @export
vec_cast.hardhat_ukey_col_listbacked.list <- vec_cast_hardhat_ukey_col_listbacked_other
#' @export
vec_cast.hardhat_ukey_col_listbacked.data.frame <- vec_cast_hardhat_ukey_col_listbacked_other
#' @export
vec_cast.hardhat_ukey_col_listbacked.vctrs_vctr <- vec_cast_hardhat_ukey_col_listbacked_other

# Converting ukey_cols to other things:

# NOTE This also makes `as.character` work to drop this wrapper class, which
# makes methods like `toupper` "work", but they will drop the ukey_col wrapper.
# We could override this behavior, but it might make some users unclear on how
# to remove the ukey_col class if they encounter it.

vec_cast_other_hardhat_ukey_col_listbacked <- function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  vec_cast(
    vctrs::field(x, "data"),
    to,
    ...,
    x_arg = glue::glue('vctrs::field({x_arg}, "data")'),
    to_arg = glue::glue('{to_arg}'),
    call = call
  )
}

#' @export
vec_cast.integer.hardhat_ukey_col_listbacked <- vec_cast_other_hardhat_ukey_col_listbacked
#' @export
vec_cast.double.hardhat_ukey_col_listbacked <- vec_cast_other_hardhat_ukey_col_listbacked
#' @export
vec_cast.character.hardhat_ukey_col_listbacked <- vec_cast_other_hardhat_ukey_col_listbacked
#' @export
vec_cast.list.hardhat_ukey_col_listbacked <- vec_cast_other_hardhat_ukey_col_listbacked
#' @export
vec_cast.data.frame.hardhat_ukey_col_listbacked <- vec_cast_other_hardhat_ukey_col_listbacked
#' @export
vec_cast.vctrs_vctr.hardhat_ukey_col_listbacked <- vec_cast_other_hardhat_ukey_col_listbacked


#' @importFrom vctrs vec_arith
#' @method vec_arith hardhat_ukey_col_listbacked
#' @export
vec_arith.hardhat_ukey_col_listbacked <- function(op, x, y, ...) {
  # NOTE `Ops.difftime` appears to have higher priority in S3 dispatch; backing
  # ukey_cols by lists will make `+ <difftime>` output an error plus a warning
  # about conflicting implementations, rather than just outputting an invalid
  # result. `Ops.difftime` priority could be beaten by maintaining an S4 flag on
  # the object or using S4 objects, but requires more code.
  UseMethod("vec_arith.hardhat_ukey_col_listbacked", y)
}

#' @method vec_arith.hardhat_ukey_col_listbacked hardhat_ukey_col_listbacked
#' @export
vec_arith.hardhat_ukey_col_listbacked.hardhat_ukey_col_listbacked <- function(op, x, y, ...) {
  # In some cases, this is nonsensical or buggy. But it could legitimately be
  # used, e.g., to transform ukey columns a and b into a and a+b, or to combine
  # ukey<glue> columns a and b into a single ukey column ab. We can't detect
  # nonsensical or buggy vs. legitimate when working on individual ukey columns
  # without the others.
  new_ukey_col_listbacked(vec_arith(op, vctrs::field(x, "data"), vctrs::field(y, "data"), ...))
}

#' @method vec_arith.hardhat_ukey_col_listbacked default
#' @export
vec_arith.hardhat_ukey_col_listbacked.default <- function(op, x, y, ...) {
  new_ukey_col_listbacked(vec_arith(op, vctrs::field(x, "data"), y, ...))
}
