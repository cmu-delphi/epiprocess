


# NOTE it might be nice to have a ptype2 for ukey_cols and the ptype they are
# matching; however, it would be tricky or impossible to properly implement
# ptype2s for everything up the coercion hierarchy, since even if doing
# something in vec_ptype2.hardhat_ukey_col_listbacked is possible, there are still the
# vec_ptype2.*.hardhat_ukey_col_listbacked methods to consider. It may be possible to
# register and dynamically update (but not in time? unless there is a way to add
# hooks for loading additional packages... or maybe with setGeneric, though then
# there may not be a need to actually go through with this registration) a set
# of such implementations using `methods` and `s3_register`. For now, rather
# than mess up the vctrs coercion hierarchy, just require things to be properly
# marked/not.

# #' @method vec_castq.hardhat_ukey_col_listbacked hardhat_ukey_col_listbacked
# #' @export
# vec_castq.hardhat_ukey_col_listbacked.hardhat_ukey_col_listbacked <- function(x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
#   new_ukey_col_listbacked(vec_castq(
#     vctrs::field(x, "data"),
#     vctrs::field(to, "data"),
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
# #' @method vec_cast hardhat_ukey_col_listbacked
# #' @export
# vec_cast.hardhat_ukey_col_listbacked <- function(x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
#   print("AAAA")
#   UseMethod("vec_cast.hardhat_ukey_col_listbacked", x)
# }


# # #' @importFrom vctrs vec_cast
# # #' @method vec_cast.hardhat_ukey_col_listbacked default
# #' @method vec_cast hardhat_ukey_col_listbacked.default
# #' @export
# vec_cast.hardhat_ukey_col_listbacked.default <- function(x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
#   stop("BBBB")
# }

# # setOldClass(c("hardhat_ukey_col_listbacked", "vctrs_rcrd", "vctrs_vctr"))
# setOldClass(class(ukey_col(1:5)))
# # XXX setGeneric & setMethod will slow down unrelated vec_cast operations... potentially... try actually installing instead of dev_loading
# # --- actually doesn't slow down unrelated stuff when installed!  however, we do take a hit within the package; e.g., bench_date_cast below.
# #
# # ... but help(Methods_for_Nongenerics) suggests problems when others try to use ours... yes, when installed, vec_cast doesn't actually use our method...
# #
# # library(vctrs); date = Sys.Date(); bench::mark(vec_cast(date, date), min_time = 5, max_iterations = 1e9)
# setGeneric("vec_cast")
# setMethod(
#   "vec_cast", c("hardhat_ukey_col_listbacked", "hardhat_ukey_col_listbacked"),
#   function (x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
#     print("S4!!!")
#   }
# )

# #' @export
# bench_date_cast <- function() {
#   library(vctrs); date = Sys.Date(); bench::mark(vec_cast(date, date), min_time = 5, max_iterations = 1e9)
# }

# TODO setting S4 flag on hardhat_ukey_col_listbacked, even though apparently the above
# works for vec_cast... will be needed for

# TODO ptype stuff

# TODO S4 trick for difftime ...


# #' @export
# asdf <- function(x, to) {
#   UseMethod("asdf", to)
# }
# #' @method asdf hardhat_ukey_col_listbacked
# #' @export
# asdf.hardhat_ukey_col_listbacked <- function(x, to) UseMethod("asdf.hardhat_ukey_col_listbacked", x)
# #' @method asdf.hardhat_ukey_col_listbacked hardhat_ukey_col_listbacked
# #' @export
# asdf.hardhat_ukey_col_listbacked.hardhat_ukey_col_listbacked <- function(x, to) print("AAA")
# #' @method asdf.hardhat_ukey_col_listbacked default
# #' @export
# asdf.hardhat_ukey_col_listbacked.default <- function(x, to) print("BBB")

# #' @method str hardhat_ukey_col_listbacked
# #' @export
# str.hardhat_ukey_col_listbacked <- function(object, object2, ...) {
#   UseMethod("str.hardhat_ukey_col_listbacked", object2)
# }

# #' @method str hardhat_ukey_col_listbacked
# #' @export
# str.hardhat_ukey_col_listbacked <- function(object, object2, ...) {
#   UseMethod("str.hardhat_ukey_col_listbacked", object2)
# }

# #' @method str.hardhat_ukey_col_listbacked default
# #' @export
# str.hardhat_ukey_col_listbacked.default <- function(object, object2, ...) {
#   print("EEE")
# }

# NOTE there seems to be no way to make toupper work correctly... besides attr-,
# not class-based, approach. And further, it will work incorrectly if we allow
# removing the marker class via vec_cast.
