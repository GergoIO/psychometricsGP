#' Create Lists if Missing
#'
#' This function checks whether specified variables exist in the global environment.
#' If a variable does not exist or is not a list, it creates the variable as an empty list.
#'
#' @param var_names A character vector containing the names of the lists to create.
#'
#' @return This function does not return a value but creates empty lists in the global environment as a side effect.
#'
#' @details
#' The function prevents overwriting existing lists and checks for non-list objects.
#' If a variable already exists as a list, no changes are made.
#'
#' @examples
#' fn_create_lists_if_missing(c("paths", "names", "data_list"))
#' # Creates empty lists in the global environment only if they don't already exist.
#'
#' @seealso [assign()], [exists()], [is.list()]
#'
#' @export
fn_create_lists_if_missing <- function(var_names) {
  message("fn_create_lists_if_missing: Creating lists, if missing.")
  for (var_name in var_names) {
    # Check if the variable exists and is a non-list object
    if (!exists(var_name, envir = .GlobalEnv) ||
        !(is.list(get(var_name, envir = .GlobalEnv)) ||
          is.null(get(var_name, envir = .GlobalEnv)))) {

      # Assign a new list only if not already a list or non-existent
      assign(var_name, list(), envir = .GlobalEnv)
      message(paste0("Created '", var_name, "' as a list."))
    } else {
      message(paste0("'", var_name, "' already exists and is a list."))
    }
  }
}
