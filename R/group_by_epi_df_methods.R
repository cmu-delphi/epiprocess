#' @importFrom dplyr select
#' @export
select.epi_df <- function(.data, ...) {
  selected <- NextMethod(.data)
  might_decay <- reclass(selected, attr(selected, "metadata"))
  return(dplyr_reconstruct(might_decay, might_decay))
}

# others to consider:
# - arrange
# -
