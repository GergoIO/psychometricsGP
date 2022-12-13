#' Remove assessment items from a dataframe by specifying the column containing the item numbers/names and the list of items numbers/names to remove
#'
#' @param df The name of the data frame.
#' @param itemColName The name of the column containing the item numbers/names
#' @param itemsToRm A list of the items to remove
#'
#' @return A data frame with the specified items removed
#' @export
#'
#' @examples fnRmExclItems(df, itemColName, itemsToRm)

################################################################################

fnRmExclItems <- function(df = NULL, itemColName = NULL, itemsToRm = NULL) {
  continue <- TRUE
  if (is.null(df) |
      is.null(itemColName)) {
    continue <- FALSE
    stop(
      "Please specify the dataframe, the column name where items are stored and the items to remove"
    )
  } else if (is.null(itemsToRm)) {
    continue <- FALSE
    message("There are no items to remove, no rows will be deleted")
    return(df)
  }
  if (continue) {
    if (length(which(df[, itemColName] %in% itemsToRm)) == 0) {
      continue <- FALSE
      message("None of the items to remove appear in the dataframe. No rows will be deleted")
      return(df)
    } else {
      return(df[-which(df[, itemColName] %in% itemsToRm), ])
    }
  }
} # END
