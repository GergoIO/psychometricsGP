#' Remove Rows By Name
#'
#' @description Removes specific columns by name from a dataframe. Additional functionality to handle scenarios where no columns need to be removed.
#'
#' @param df The name of the data frame.
#' @param itemsToRm (Vector of numerics) The items to remove. Can be an empty vector, c()
#'
#' @return This function will return a dataframe with the selected columns removed
#' @export
#'
#' @examples fnRmColsByName(dfName, c(15,29,43)), fnRmColsByName(dfName, c())

################################################################################

fnRmColsByName <- function(df, itemsToRm) {
  if (is.null(df) == TRUE) {
    stop("Please specify the dataframe")
  }
  if (is.null(itemsToRm) == TRUE) {
    message("fnRmColsByName: There are no items to remove, no columns will be deleted")
    return(df)
  } else {
    return(df[, -itemsToRm])
  }
} # THE END
