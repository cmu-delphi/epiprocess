#' @importFrom dplyr select
#' @export
select.epi_df <- function(.data, ...) {
  selected <- NextMethod(.data)
  return(dplyr_reconstruct(selected, .data))
}

# others to consider:
# - arrange
# -
