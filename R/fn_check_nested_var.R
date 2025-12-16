#   ____________________________________________________________________________
#   Check Nested Variable Existence and Value from Full Variable String    ####

#' Check nested variable existence and value from a string path
#'
#' Safely checks whether a fully qualified nested variable (given as a single string
#' like `"include$AR_report$scores_plot"`) exists in the global environment, and
#' evaluates its value either by membership (`%in%`), logical truthiness, or by
#' simply requiring that it has some value set (not `NULL`, not zero-length, not
#' all `NA`).
#'
#' The function is designed to be robust when intermediate components of the
#' path are missing; if any part does not exist, it returns `FALSE` instead of
#' erroring.
#'
#' @param full_var String, the full variable path including the root object,
#'   using `$` to separate nesting levels (for example `"include$AR_report$scores_plot"`).
#' @param check_values Optional vector of values to check membership against.
#'   If supplied, the function returns `TRUE` when the resolved value is
#'   contained in `check_values` (using `%in%` semantics, with `any()` for
#'   vectors/lists).
#' @param require_value Logical, default `FALSE`. If `TRUE` and `check_values`
#'   is `NULL`, the function returns `TRUE` when the resolved value is
#'   considered to be “set” (not `NULL`, length greater than zero, and not all
#'   `NA`). If `FALSE` and `check_values` is `NULL`, the function falls back to
#'   the original behaviour of `isTRUE(resolved_value)`.
#'
#' @return A single logical. `TRUE` if the nested variable exists and satisfies
#'   the requested condition (truthiness, membership, or having any value);
#'   `FALSE` otherwise.
#'
#' @details
#' The logic is:
#' - If the root object (first element of `full_var`) does not exist in the
#'   global environment, the function returns `FALSE`.
#' - The function then walks the remaining `$`-separated parts, assuming lists
#'   and environments along the way. If a required component is missing in a
#'   list or environment, it returns `FALSE`.
#' - Once the final object (`current`) is resolved:
#'   - If `check_values` is not `NULL`, the function performs a membership test:
#'     `current %in% check_values`, using `any()` for vectors/lists.
#'   - If `check_values` is `NULL` and `require_value = FALSE`, the function
#'     uses `isTRUE(current)` (original behaviour).
#'   - If `check_values` is `NULL` and `require_value = TRUE`, the function
#'     checks that `current` is not `NULL`, has length greater than zero, and
#'     is not entirely `NA`.
#'
#' @examples
#' # Basic setup
#' include <- list(
#'   AR_report = list(
#'     scores_plot          = "Raw Scores",
#'     scores_density_plot  = NULL,
#'     flags                = c("Raw Scores", "Something Else")
#'   )
#' )
#' assign("include", include, envir = .GlobalEnv)
#'
#' # 1. Membership check (original behaviour with check_values)
#' fn_check_nested_var(
#'   "include$AR_report$scores_plot",
#'   check_values = c("Raw Scores", "Both")
#' )
#' # TRUE, because "Raw Scores" is in the supplied values
#'
#' fn_check_nested_var(
#'   "include$AR_report$scores_plot",
#'   check_values = "Something Else"
#' )
#' # FALSE, not in the supplied values
#'
#' fn_check_nested_var(
#'   "include$AR_report$flags",
#'   check_values = "Something Else"
#' )
#' # TRUE, any element of the vector matching is enough
#'
#' # 2. Logical truthiness (original behaviour, no check_values)
#' include$AR_report$show_section <- TRUE
#' fn_check_nested_var("include$AR_report$show_section")
#' # TRUE, because isTRUE(TRUE) is TRUE
#'
#' include$AR_report$show_section <- FALSE
#' fn_check_nested_var("include$AR_report$show_section")
#' # FALSE, because isTRUE(FALSE) is FALSE
#'
#' # 3. Has-value check (new behaviour via require_value = TRUE)
#' include$AR_report$note <- "Some note"
#' fn_check_nested_var(
#'   "include$AR_report$note",
#'   require_value = TRUE
#' )
#' # TRUE, non-empty character string
#'
#' include$AR_report$empty_vec <- numeric(0)
#' fn_check_nested_var(
#'   "include$AR_report$empty_vec",
#'   require_value = TRUE
#' )
#' # FALSE, zero-length vector
#'
#' include$AR_report$all_na <- c(NA_real_, NA_real_)
#' fn_check_nested_var(
#'   "include$AR_report$all_na",
#'   require_value = TRUE
#' )
#' # FALSE, all values are NA
#'
#' fn_check_nested_var(
#'   "include$AR_report$does_not_exist",
#'   require_value = TRUE
#' )
#' # FALSE, path does not exist at all
#'
#' # 4. Handling unquoted or malformed full_var
#' fn_check_nested_var(include$AR_report$scores_plot)
#' # Returns FALSE with a warning, because full_var was not passed as a string
fn_check_nested_var <- function(full_var,
                                check_values = NULL,
                                require_value = FALSE) {

  # Check if full_var is a character string
  if (!is.character(full_var) || length(full_var) != 1) {
    warning(
      "Input 'full_var' must be a single string in quotes specifying the variable path (e.g. \"include$AR_report$scores_plot\"). ",
      "It looks like the variable name was passed unquoted or incorrectly."
    )
    return(FALSE)
  }

  .parts <- strsplit(full_var, "\\$")[[1]]

  # Try to get the root variable from global environment
  if (!exists(.parts[1], envir = .GlobalEnv, inherits = FALSE)) {
    return(FALSE)
  }
  .current <- get(.parts[1], envir = .GlobalEnv, inherits = FALSE)

  # Traverse nested parts
  if (length(.parts) > 1) {
    for (.p in .parts[-1]) {
      # Must be a list or environment to look up elements
      if (is.list(.current) || is.environment(.current)) {
        # If it's a list and the name exists, traverse; else FALSE
        if (is.list(.current) && .p %in% names(.current)) {
          .current <- .current[[.p]]
        } else if (is.environment(.current) && exists(.p, envir = .current, inherits = FALSE)) {
          .current <- get(.p, envir = .current, inherits = FALSE)
        } else {
          return(FALSE)
        }
      } else {
        return(FALSE)
      }
    }
  }

  # Helper: does this object have a value set (not NULL, not length 0, not all NA)?
  .has_value <- function(x) {
    if (is.null(x)) {
      return(FALSE)
    }
    if (length(x) == 0) {
      return(FALSE)
    }
    if (all(is.na(x))) {
      return(FALSE)
    }
    TRUE
  }

  # Existing behaviour + new require_value behaviour on top
  if (is.null(check_values)) {
    if (require_value) {
      return(.has_value(.current))
    } else {
      return(isTRUE(.current))
    }
  }

  # Compare atomic value, flatten if needed
  if (is.atomic(.current) && length(.current) == 1) {
    return(.current %in% check_values)
  }
  # For vector/list, any() to check for membership in check_values
  if (is.vector(.current) || is.list(.current)) {
    return(any(.current %in% check_values))
  }
  # Otherwise, just do direct comparison
  .current %in% check_values
}
