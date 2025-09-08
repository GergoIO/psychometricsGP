
# OLD USE fn_check_nested_var instead
#' Check if a nested variable exists and has a value
#'
#' This function safely checks whether a nested variable or list element
#' exists and contains a non-NULL value. It handles cases where any part
#' of the nested structure may not exist without throwing errors.
#'
#' @param expr An unquoted expression representing the nested variable to check.
#'   Can be any level of nesting (e.g., \code{obj$level1$level2$item}).
#'
#' @return A logical value: \code{TRUE} if the variable exists and has a
#'   non-NULL value, \code{FALSE} if it doesn't exist or is NULL.
#'
#' @details
#' The function uses \code{tryCatch} to safely evaluate the expression.
#' If any part of the nested path doesn't exist, it returns \code{FALSE}
#' instead of throwing an error. Only non-NULL values are considered as
#' "having a value".
#'
#' @examples
#' \dontrun{
#' # Create some test data
#' grading <- list(hofstee = list(remove_judges = TRUE))
#'
#' # Check if nested variable exists
#' if (fn_has_nested_value(grading$hofstee$remove_judges)) {
#'   print("Variable exists!")
#' }
#'
#' # Works with missing paths
#' fn_has_nested_value(grading$missing$path)  # Returns FALSE
#'
#' # Works with data frames
#' df <- data.frame(x = 1:3, y = letters[1:3])
#' fn_has_nested_value(df$x)  # Returns TRUE
#' fn_has_nested_value(df$missing_col)  # Returns FALSE
#' }
#'
#' @seealso \code{\link{get_nested_value}} for safely retrieving nested values
#' @export
fn_has_nested_value <- function(expr) {
  tryCatch({
    result <- eval(substitute(expr), envir = parent.frame())
    !is.null(result)
  }, error = function(e) {
    FALSE
  })
}
