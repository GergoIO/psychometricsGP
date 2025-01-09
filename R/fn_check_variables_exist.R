#' Check If Variables Exist
#'
#' This function checks whether specified variables exist in the global environment.
#' If a variable does not exist or is empty, it alerts the user to set the variable.
#'
#' @param var_names A character vector containing the names of the variables to check.
#' @param message_text A character string to customize the alert message. Defaults to "Please set the variable".
#'
#' @return This function does not return any value but prints messages to alert the user to set missing variables.
#'
#' @details
#' The function checks if the variables exist in the global environment. For list variables,
#' it checks if the specified element of the list exists and is non-NULL.
#' For non-list variables, it checks if they exist and are non-NULL.
#'
#' @examples
#' files <- list(data_source = 1)
#' some_var = 2
#' fn_check_variables_exist(c("files$data_source", "files$other_var", "some_var"))
#' # Customizing the message text:
#' fn_check_variables_exist(c("files$data_source", "files$other_var"), message_text = "Set the following variable:")
#'
#' @seealso [exists()], [get()]
#'
#' @export
fn_check_variables_exist <- function(var_names, message_text = "Please set the variable: ") {
  for (var_name in var_names) {
    # Split the string at "$" to handle list access
    parts <- strsplit(var_name, "\\$")[[1]]

    # Check if the variable is a list
    if (length(parts) > 1) {
      # Check if the list exists
      if (exists(parts[1], envir = .GlobalEnv)) {
        # Get the list and check if the specific element exists
        list_object <- get(parts[1], envir = .GlobalEnv)

        # Check if the element exists in the list
        if (parts[2] %in% names(list_object)) {
          if (!is.null(list_object[[parts[2]]]) && list_object[[parts[2]]] != "") {
            # Do nothing if the variable is set
          } else {
            message(paste(message_text, var_name))
          }
        } else {
          message(paste(message_text, var_name))
        }
      } else {
        message(paste(message_text, var_name))
      }
    } else {
      # Handle standalone variables
      if (exists(var_name, envir = .GlobalEnv)) {
        var_value <- get(var_name, envir = .GlobalEnv)
        if (!is.null(var_value) && var_value != "") {
          # Do nothing if the variable is set
        } else {
          message(paste(message_text, var_name))
        }
      } else {
        message(paste(message_text, var_name))
      }
    }
  }
}
