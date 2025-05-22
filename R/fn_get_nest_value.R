#' Safely retrieve a nested variable value
#'
#' This function safely retrieves the value of a nested variable or list
#' element. If any part of the nested structure doesn't exist, it returns
#' NULL instead of throwing an error.
#'
#' @param expr An unquoted expression representing the nested variable to retrieve.
#'   Can be any level of nesting (e.g., \code{obj$level1$level2$item}).
#'
#' @return The value of the nested variable if it exists, or \code{NULL} if
#'   any part of the path doesn't exist or if the final value is NULL.
#'
#' @details
#' The function uses \code{tryCatch} to safely evaluate the expression.
#' This is particularly useful when working with nested lists or data
#' structures where the existence of intermediate levels is uncertain.
#'
#' Unlike \code{\link{has_nested_value}}, this function returns the actual
#' value rather than just checking for existence.
#'
#' @examples
#' \dontrun{
#' # Create some test data
#' config <- list(
#'   database = list(
#'     host = "localhost",
#'     port = 5432,
#'     credentials = list(user = "admin")
#'   )
#' )
#'
#' # Safely get nested values
#' host <- fn_get_nested_value(config$database$host)  # Returns "localhost"
#' user <- fn_get_nested_value(config$database$credentials$user)  # Returns "admin"
#'
#' # Returns NULL for missing paths
#' missing <- fn_get_nested_value(config$missing$path)  # Returns NULL
#'
#' # Can be used with conditional logic
#' port <- fn_get_nested_value(config$database$port)
#' if (!is.null(port)) {
#'   print(paste("Connecting to port:", port))
#' }
#' }
#'
#' @seealso \code{\link{has_nested_value}} for checking existence without retrieval
#' @export
fn_get_nested_value <- function(expr) {
  tryCatch({
    eval(substitute(expr), envir = parent.frame())
  }, error = function(e) {
    NULL
  })
}
