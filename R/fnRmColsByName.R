#' Removes specific columns by name from a dataframe
#'
#' @param df The name of the data frame.
#' @param itemsToRm The
#'
#' @return This function will return a dataframe with the selected columns removed
#' @export
#'
#' @examples fnRmColsByName(dfName, c(15,29,43))

################################################################################

fnRmColsByName <- function(df, itemsToRm) {
  continue <- TRUE
  if (is.null(df) == TRUE) {
    continue <- FALSE
    warning("Please specify the dataframe")
  } else if (is.null(itemsToRm) == TRUE) {
    continue <- FALSE
    warning("There are no items to remove, no columns will be deleted")
    return(df)
  } else {
    return(df[, -itemsToRm])
  }
} # THE END
