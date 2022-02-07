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
  class(x) = c("epi_df", class(x))
  attributes(x)$metadata = metadata
  return(x)
}

#' @method group_by epi_df
#' @rdname arrange.epi_df
#' @importFrom dplyr group_by
#' @export
group_by.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  class(x) = c("epi_df", class(x))
  attributes(x)$metadata = metadata
  return(x)
}

#' @method group_modify epi_df
#' @rdname arrange.epi_df
#' @importFrom dplyr group_modify
#' @export
group_modify.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  class(x) = c("epi_df", class(x))
  attributes(x)$metadata = metadata
  return(x)
}

#' @method mutate epi_df
#' @rdname arrange.epi_df
#' @importFrom dplyr mutate
#' @export
mutate.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  class(x) = c("epi_df", class(x))
  attributes(x)$metadata = metadata
  return(x)
}

#' @method ungroup epi_df
#' @rdname arrange.epi_df
#' @importFrom dplyr ungroup
#' @export
ungroup.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  class(x) = c("epi_df", class(x))
  attributes(x)$metadata = metadata
  return(x)
}

#' @method unnest epi_df
#' @rdname arrange.epi_df
#' @importFrom tidyr unnest
#' @export
unnest.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  class(x) = c("epi_df", class(x))
  attributes(x)$metadata = metadata
  return(x)
}
