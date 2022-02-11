#' dplyr verbs
#'
#' dplyr verbs for `epi_df` objexts, preserving class and attributes. 
#'
#' @method arrange epi_df
#' @importFrom dplyr arrange
#' @export
arrange.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  return(reclass(x, metadata))
}

#' @method filter epi_df
#' @rdname arrange.epi_df
#' @importFrom dplyr filter
#' @export
filter.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  return(reclass(x, metadata))
}

#' @method group_by epi_df
#' @rdname arrange.epi_df
#' @importFrom dplyr group_by
#' @export
group_by.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  return(reclass(x, metadata))
}

#' @method group_modify epi_df
#' @rdname arrange.epi_df
#' @importFrom dplyr group_modify
#' @export
group_modify.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  return(reclass(x, metadata))
}

#' @method mutate epi_df
#' @rdname arrange.epi_df
#' @importFrom dplyr mutate
#' @export
mutate.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  return(reclass(x, metadata))
}

#' @method relocate epi_df
#' @rdname arrange.epi_df
#' @importFrom dplyr relocate
#' @export
relocate.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  return(reclass(x, metadata))
}

#' @method rename epi_df
#' @rdname arrange.epi_df
#' @importFrom dplyr rename
#' @export
rename.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  return(reclass(x, metadata))
}

#' @method slice epi_df
#' @rdname arrange.epi_df
#' @importFrom dplyr slice
#' @export
slice.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  return(reclass(x, metadata))
}

#' @method ungroup epi_df
#' @rdname arrange.epi_df
#' @importFrom dplyr ungroup
#' @export
ungroup.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  return(reclass(x, metadata))
}

#' @method unnest epi_df
#' @rdname arrange.epi_df
#' @importFrom tidyr unnest
#' @export
unnest.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  return(reclass(x, metadata))
}

# Simple reclass function
reclass = function(x, metadata) {
  class(x) = unique(c("epi_df", class(x)))
  attributes(x)$metadata = metadata
  return(x)
}
