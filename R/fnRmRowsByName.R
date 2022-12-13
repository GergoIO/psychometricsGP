#' Remove Rows by Name
#'
#' @description Removes specific rows by name from a dataframe or list. Additional functionality to handle scenarios where no columns need to be removed. Works for 1D lists and 2D data frames.
#'
#' @param input The name of the data frame or list.
#' @param itemsToRm (A vector of numerics) The items to remove. Can be an empty vector, c()
#'
#' @return This function will return a dataframe with the selected rows removed
#' @export
#'
#' @examples fnRmRowsByName(dfName, c(2)), fnRmRowsByName(dfName, c()), fnRmRowsByName(list, c(1,2,3))

################################################################################

fnRmRowsByName <- function(input, itemsToRm) {
  continue <- TRUE
  if (is.null(input)) {
    stop("fnRmRowsByName: No data is specified. Please specify the data")
  }
  if (is.null(itemsToRm)) {
    message("fnRmRowsByName: There are no items to remove, no rows will be deleted")
    return(input)
  } else if (is.null(dim(input))) {
    return(input[-itemsToRm]) # for 1D lists etc
  } else {
    return(input[!(rownames(input) %in% itemsToRm), ])
    # return(input[-itemsToRm, ]) # for 2D dfs etc
  }
} # END
