#' Remove Columns by Name
#'
#' @description Removes specific columns by name from a dataframe. Additional functionality to handle scenarios where no columns need to be removed.
#'
#' @param input The name of the data frame.
#' @param itemsToRm (Vector of numerics) The items to remove. Can be an empty vector, c()
#'
#' @return This function will return a dataframe with the selected columns removed
#' @export
#'
#' @examples fnRmColsByName(dfName, c(15,29,43)), fnRmColsByName(dfName, c())

################################################################################

fnRmColsByName <- function(input, itemsToRm) {
  if (is.null(input)) {
    stop("Please specify the dataframe")
  }
  if (is.null(itemsToRm)) {
    message("fnRmColsByName: There are no items to remove, no columns will be deleted")
    return(input)
  } else {
    return(input[, !(colnames(input) %in% itemsToRm)])
    # return(input[, -itemsToRm])
  }
} # THE END
