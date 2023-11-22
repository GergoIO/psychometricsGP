#' Remove Columns by Name
#'
#' @description Removes specific columns by name from a dataframe. Additional functionality to handle scenarios where no columns need to be removed.
#'
#' @param input The name of the data frame.
#' @param items_to_rm (Vector of numerics) The items to remove. Can be an empty vector, c()
#'
#' @return This function will return a dataframe with the selected columns removed
#' @export
#'
#' @examples fn_rm_cols_by_name(df_name, c(15,29,43)), fn_rm_cols_by_name(df_name, c())

################################################################################

fn_rm_cols_by_name <- function(input, items_to_rm) {
  if (is.null(input)) {
    stop("Please specify the dataframe")
  }
  if (is.null(items_to_rm)) {
    message("fn_rm_cols_by_name: There are no items to remove, no columns will be deleted")
    return(input)
  } else {
    return(input[, !(colnames(input) %in% items_to_rm)])
  }
} # END
