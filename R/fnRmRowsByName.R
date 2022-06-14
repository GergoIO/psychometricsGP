#' Removes specific columns by name from a dataframe. Additional functionality to handle scenarios where no columns need to be removed. 1D lists and 2D data frames are handled
#'
#' @param input The name of the data frame or list.
#' @param itemsToRm The items to remove. Can be an empty vector c()
#'
#' @return This function will return a dataframe with the selected rows removed
#' @export
#'
#' @examples fnRmRowsByName(df, c(2)), fnRmRowsByName(df, c()), fnRmRowsByName(list, c(1,2,3))

################################################################################

fnRmRowsByName <- function(input, itemsToRm) {
  continue <- TRUE
  if (is.null(input) == TRUE) {
    continue <- FALSE
    stop("Please specify the data")
  } else if (is.null(itemsToRm) == TRUE) {
    continue <- FALSE
    warning("There are no items to remove, no rows will be deleted")
    return(input)
  } else if (is.null(dim(input)) == TRUE) {
    return(input[-itemsToRm]) # for 1D lists etc
  } else {
    return(input[-itemsToRm, ]) # for 2D dfs etc
  }
} # END
