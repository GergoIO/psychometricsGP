#' Check Nested Variable Existence and Value from Full Variable String
#'
#' This function safely checks whether a fully qualified nested variable (given as a single string
#' like "include$AR_report$scores_plot") exists in the global environment, and evaluates its value
#' either by membership (%in%) or logical truthiness.
#' If any part of the path is missing, returns FALSE.
#'
#' @param full_var A string specifying the full variable path including root, separated by "$"
#'                 (e.g. "include$AR_report$scores_plot").
#' @param check_values Optional vector of values to check membership against. If missing or NULL,
#'                     the value is tested for logical truthiness (isTRUE).
#'
#' @return Logical TRUE if the nested variable exists and checks out. FALSE otherwise.
#'
#' @examples
#' include <- list(AR_report = list(scores_plot = "Raw Scores"))
#' assign("include", include, envir = .GlobalEnv)
#' fn_check_nested_var("include$AR_report$scores_plot", c("Raw Scores", "Both"))
#' # Returns TRUE
#'
#' fn_check_nested_var("include$AR_report$scores_density_plot", c("Raw Scores"))
#' # Returns FALSE (variable missing)
#'
fn_check_nested_var <- function(full_var, check_values = NULL) {
  parts <- strsplit(full_var, "\\$")[[1]]

  # Try to get the root variable from global environment
  if (!exists(parts[1], envir = .GlobalEnv)) {
    return(FALSE)
  }
  current <- get(parts[1], envir = .GlobalEnv)

  # Traverse nested parts
  if (length(parts) > 1) {
    for (p in parts[-1]) {
      if (!is.list(current) && !is.environment(current)) {
        return(FALSE)
      }
      if (!(p %in% names(current))) {
        return(FALSE)
      }
      current <- current[[p]]
    }
  }

  # Evaluate condition
  if (is.null(check_values)) {
    return(isTRUE(current))
  } else {
    return(current %in% check_values)
  }
}
