#' Round Values in Data Frame by Rows and Columns
#'
#' @description
#' Rounds numeric values in a data frame with fine-grained control over which rows and columns are rounded
#' and to how many decimal places. This function supports two rounding modes: sequential (traditional)
#' and maximum precision. It can handle an arbitrary number of row and column specifications.
#'
#' @param data A data frame containing values to be rounded.
#' @param ... Row and column specifications. These should be pairs of arguments:
#'   * `rows` (or `rows2`, `rows3`, etc.): Row indices or names to be rounded
#'   * `rows_decimal` (or `rows_decimal2`, `rows_decimal3`, etc.): Number of decimal places for corresponding rows
#'   * `cols` (or `cols2`, `cols3`, etc.): Column indices or names to be rounded
#'   * `cols_decimal` (or `cols_decimal2`, `cols_decimal3`, etc.): Number of decimal places for corresponding columns
#' @param rows_first Logical. If TRUE (default), rows are processed before columns in sequential mode.
#'   This parameter has no effect when `use_max_precision` is TRUE.
#' @param use_max_precision Logical. If TRUE, the function uses the maximum precision mode where
#'   each cell is rounded to the maximum number of decimal places specified for it in any row or column specification.
#'   If FALSE (default), it uses sequential mode where rounding is applied in order.
#' @param use_threshold_zero_replacement Logical. If TRUE (default), values that round to zero are replaced with
#'   threshold strings (e.g., "<0.001" for 3 decimal places). If FALSE, values are displayed as zeros with the
#'   appropriate number of decimal places (e.g., "0.000" for 3 decimal places).
#' @param threshold_replacement_format Character string. Template for the threshold replacement text.
#'   Default is "<{threshold}", where {threshold} will be replaced by the smallest representable value
#'   at the given decimal places. For example, "<0.001" for 3 decimal places.
#' @param rounding_method Character string. The method to use for rounding. Options are:
#'   * "round" (default): Standard rounding, away from zero for .5
#'   * "floor": Always rounds down toward negative infinity
#'   * "ceiling": Always rounds up toward positive infinity
#'   * "trunc": Truncates, always toward zero
#'   * "signif": Uses significant digits instead of decimal places
#' @param use_scientific_notation Logical. If TRUE, very large or small numbers will be formatted using
#'   scientific notation. If FALSE (default), standard decimal notation is used.
#' @param return_numeric Logical. If TRUE, returns a data frame with numeric values instead of
#'   character strings. If FALSE (default), returns a data frame with formatted character strings.
#'
#' @details
#' The function has two operational modes:
#'
#' 1. Sequential Mode (`use_max_precision = FALSE`): Rounding is applied sequentially. The `rows_first` parameter
#'    determines whether rows or columns are processed first. This means that later operations may override
#'    earlier ones when cells are at the intersection of specified rows and columns.
#'
#' 2. Maximum Precision Mode (`use_max_precision = TRUE`): The function first analyzes all specifications to
#'    determine the maximum number of decimal places required for each cell, then applies rounding once.
#'    The `rows_first` parameter has no effect in this mode.
#'
#' The rounding behavior is controlled by several parameters:
#'
#' * `rounding_method` determines how values are rounded (standard, floor, ceiling, etc.)
#' * `use_threshold_zero_replacement` determines whether very small values are replaced with threshold strings
#' * `threshold_replacement_format` controls the format of threshold replacement strings
#' * `use_scientific_notation` enables scientific notation for very large or small numbers
#' * `return_numeric` controls whether the function returns formatted strings or numeric values
#'
#' For specifying multiple sets of rows or columns, append a number to the parameter name (e.g., `rows2`, `rows3`).
#' Each row or column specification must have a corresponding decimal places specification.
#'
#' The function converts numeric values to character strings after rounding (unless `return_numeric = TRUE`). It also:
#' * Preserves NA values
#' * Leaves non-numeric values unchanged
#' * Handles zero values based on `use_threshold_zero_replacement` setting
#'
#' @return A data frame with specified numeric values rounded according to the parameters. By default,
#'   returns character strings, but will return numeric values if `return_numeric = TRUE`.
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(
#'   A = c(1.23456, 2.34567, 3.45678),
#'   B = c(0.00012, 5.67890, 6.78901),
#'   C = c("text", "NA", "7.89012")
#' )
#'
#' # Basic usage with default settings
#' fn_round_table(
#'   df,
#'   rows = 1,
#'   rows_decimal = 2,
#'   cols = "B",
#'   cols_decimal = 3
#' )
#'
#' # Display actual zeros instead of threshold strings
#' fn_round_table(
#'   df,
#'   rows = 1,
#'   rows_decimal = 3,
#'   cols = "B",
#'   cols_decimal = 3,
#'   use_threshold_zero_replacement = FALSE
#' )
#'
#' # Change the threshold replacement format
#' fn_round_table(
#'   df,
#'   rows = 1,
#'   rows_decimal = 3,
#'   threshold_replacement_format = "Less than {threshold}"
#' )
#'
#' # Use ceiling rounding method
#' fn_round_table(
#'   df,
#'   rows = 1:3,
#'   rows_decimal = 2,
#'   rounding_method = "ceiling"
#' )
#'
#' # Use scientific notation for large/small numbers
#' df_big <- data.frame(
#'   X = c(0.00000123, 9876543210),
#'   Y = c(2.34567, 3.45678)
#' )
#' fn_round_table(
#'   df_big,
#'   rows = 1:2,
#'   rows_decimal = 3,
#'   use_scientific_notation = TRUE
#' )
#'
#' # Return numeric values instead of strings
#' result <- fn_round_table(
#'   df,
#'   rows = 1:3,
#'   rows_decimal = 2,
#'   return_numeric = TRUE
#' )
#' # Now you can perform calculations on the result
#' result$A * 2
#'
#' @export
fn_round_table <- function(data,
                           ...,
                           rows_first = TRUE,
                           use_max_precision = FALSE,
                           use_threshold_zero_replacement = TRUE,
                           threshold_replacement_format = "<{threshold}",
                           rounding_method = "round",
                           use_scientific_notation = FALSE,
                           return_numeric = FALSE) {
  # Validate data argument
  if (!is.data.frame(data)) {
    stop("The 'data' argument must be a data frame.")
  }

  # Validate rounding method
  valid_rounding_methods <- c("round", "floor", "ceiling", "trunc", "signif")
  if (!(rounding_method %in% valid_rounding_methods)) {
    stop(paste0("Invalid rounding method. Must be one of: ",
                paste(valid_rounding_methods, collapse = ", "), "."))
  }

  # Helper function to perform rounding based on the specified method
  perform_rounding <- function(value, decimals) {
    if (rounding_method == "round") {
      return(round(value, digits = decimals))
    } else if (rounding_method == "floor") {
      multiplier <- 10^decimals
      return(floor(value * multiplier) / multiplier)
    } else if (rounding_method == "ceiling") {
      multiplier <- 10^decimals
      return(ceiling(value * multiplier) / multiplier)
    } else if (rounding_method == "trunc") {
      multiplier <- 10^decimals
      return(trunc(value * multiplier) / multiplier)
    } else if (rounding_method == "signif") {
      return(signif(value, digits = decimals))
    }
  }

  # Helper function to format numbers (can be scientific or standard)
  format_number <- function(value, decimals) {
    if (use_scientific_notation) {
      # Format with scientific notation
      if (abs(value) < 0.0001 || abs(value) > 10000) {
        if (rounding_method == "signif") {
          return(format(value, digits = decimals, scientific = TRUE))
        } else {
          return(format(value, nsmall = decimals, scientific = TRUE))
        }
      }
    }

    # Format without scientific notation
    if (rounding_method == "signif") {
      format_string <- paste0("%.", decimals, "g")
    } else {
      format_string <- paste0("%.", decimals, "f")
    }
    return(sprintf(format_string, value))
  }

  # Helper function to create threshold string
  create_threshold_string <- function(decimals) {
    threshold_value <- 1 / (10^decimals)
    threshold_formatted <- format_number(threshold_value, decimals)
    return(gsub("\\{threshold\\}", threshold_formatted, threshold_replacement_format))
  }

  # Collect all arguments
  args <- list(...)

  # Extract all row/col specifications from arguments
  all_specs <- list()
  pattern <- "^(rows|cols)(\\d*)$"
  decimal_args <- grep("_decimal", names(args), value = TRUE)

  for (arg_name in names(args)) {
    if (grepl(pattern, arg_name)) {
      # Extract base (rows/cols) and index (could be empty, indicating first set)
      matches <- regmatches(arg_name, regexec(pattern, arg_name))[[1]]
      base <- matches[2]
      index <- matches[3]

      # Get corresponding decimal parameter
      decimal_name <- paste0(base, "_decimal", index)

      if (decimal_name %in% names(args)) {
        spec <- list(
          type = base,
          # "rows" or "cols"
          indices = args[[arg_name]],
          decimal = args[[decimal_name]],
          arg_name = arg_name,
          # Store original argument name for error messages
          order = if (index == "")
            1
          else
            as.numeric(index)  # Order: 1 for no index, otherwise the index number
        )
        all_specs <- c(all_specs, list(spec))
      } else {
        stop(
          paste0(
            "Missing required parameter: '",
            decimal_name,
            "' must be provided when '",
            arg_name,
            "' is specified."
          )
        )
      }
    }
  }

  # Check for unused Decimal parameters
  for (dec_arg in decimal_args) {
    base_arg <- sub("_decimal(\\d*)$", "\\1", dec_arg)
    base_arg <- sub("^rows", "rows", base_arg)
    base_arg <- sub("^cols", "cols", base_arg)
    if (!(base_arg %in% names(args))) {
      stop(
        paste0(
          "Missing required parameter: '",
          base_arg,
          "' must be provided when '",
          dec_arg,
          "' is specified."
        )
      )
    }
  }

  # Validate decimal values are numeric and non-negative
  for (spec in all_specs) {
    if (!is.numeric(spec$decimal) || spec$decimal < 0) {
      stop(paste0(
        "The '",
        paste0(spec$type, "_decimal", if (spec$order > 1)
          spec$order
          else
            ""),
        "' parameter must be a non-negative numeric value."
      ))
    }
  }

  # Convert row names to row indices with improved error handling
  row_names <- row.names(data)
  for (i in seq_along(all_specs)) {
    if (all_specs[[i]]$type == "rows") {
      if (is.character(all_specs[[i]]$indices)) {
        missing_rows <- setdiff(all_specs[[i]]$indices, row_names)
        if (length(missing_rows) > 0) {
          warning(
            paste0(
              "The following row names specified in '",
              all_specs[[i]]$arg_name,
              "' are not present: ",
              paste(missing_rows, collapse = ", "),
              ". These will be ignored."
            )
          )
        }
        all_specs[[i]]$indices <- which(row_names %in% all_specs[[i]]$indices)
        if (length(all_specs[[i]]$indices) == 0) {
          stop(
            paste0(
              "No valid rows found for '",
              all_specs[[i]]$arg_name,
              "'. Please check your row specifications."
            )
          )
        }
      } else if (is.numeric(all_specs[[i]]$indices)) {
        invalid_indices <- all_specs[[i]]$indices[all_specs[[i]]$indices < 1 |
                                                    all_specs[[i]]$indices > nrow(data)]
        if (length(invalid_indices) > 0) {
          warning(
            paste0(
              "The following row indices specified in '",
              all_specs[[i]]$arg_name,
              "' are out of bounds: ",
              paste(invalid_indices, collapse = ", "),
              ". These will be ignored."
            )
          )
          all_specs[[i]]$indices <- all_specs[[i]]$indices[all_specs[[i]]$indices >= 1 &
                                                             all_specs[[i]]$indices <= nrow(data)]
        }
        if (length(all_specs[[i]]$indices) == 0) {
          stop(
            paste0(
              "No valid rows found for '",
              all_specs[[i]]$arg_name,
              "'. Please check your row specifications."
            )
          )
        }
      } else {
        stop(
          paste0(
            "The '",
            all_specs[[i]]$arg_name,
            "' parameter must be either a character vector of row names or a numeric vector of row indices."
          )
        )
      }
    }
  }

  # Convert column names to column indices with improved error handling
  col_names <- colnames(data)
  for (i in seq_along(all_specs)) {
    if (all_specs[[i]]$type == "cols") {
      if (is.character(all_specs[[i]]$indices)) {
        missing_cols <- setdiff(all_specs[[i]]$indices, col_names)
        if (length(missing_cols) > 0) {
          warning(
            paste0(
              "The following column names specified in '",
              all_specs[[i]]$arg_name,
              "' are not present: ",
              paste(missing_cols, collapse = ", "),
              ". These will be ignored."
            )
          )
        }
        all_specs[[i]]$indices <- which(col_names %in% all_specs[[i]]$indices)
        if (length(all_specs[[i]]$indices) == 0) {
          stop(
            paste0(
              "No valid columns found for '",
              all_specs[[i]]$arg_name,
              "'. Please check your column specifications."
            )
          )
        }
      } else if (is.numeric(all_specs[[i]]$indices)) {
        invalid_indices <- all_specs[[i]]$indices[all_specs[[i]]$indices < 1 |
                                                    all_specs[[i]]$indices > ncol(data)]
        if (length(invalid_indices) > 0) {
          warning(
            paste0(
              "The following column indices specified in '",
              all_specs[[i]]$arg_name,
              "' are out of bounds: ",
              paste(invalid_indices, collapse = ", "),
              ". These will be ignored."
            )
          )
          all_specs[[i]]$indices <- all_specs[[i]]$indices[all_specs[[i]]$indices >= 1 &
                                                             all_specs[[i]]$indices <= ncol(data)]
        }
        if (length(all_specs[[i]]$indices) == 0) {
          stop(
            paste0(
              "No valid columns found for '",
              all_specs[[i]]$arg_name,
              "'. Please check your column specifications."
            )
          )
        }
      } else {
        stop(
          paste0(
            "The '",
            all_specs[[i]]$arg_name,
            "' parameter must be either a character vector of column names or a numeric vector of column indices."
          )
        )
      }
    }
  }

  # Convert "NA" string cells to actual NA
  data <- data %>%
    mutate(across(where(is.character), ~ ifelse(. == "NA", NA, .)))

  # Create a copy of the data for numeric return if needed
  if (return_numeric) {
    numeric_data <- data
  }

  # If using max precision mode, build precision matrix and apply once
  if (use_max_precision) {
    # Create precision matrix
    precision_matrix <- matrix(0, nrow = nrow(data), ncol = ncol(data))

    # Update precision matrix for all specifications
    for (spec in all_specs) {
      is_row <- spec$type == "rows"
      indices <- spec$indices
      decimals <- spec$decimal

      if (is_row) {
        for (idx in indices) {
          precision_matrix[idx, ] <- pmax(precision_matrix[idx, ], decimals)
        }
      } else {
        for (idx in indices) {
          precision_matrix[, idx] <- pmax(precision_matrix[, idx], decimals)
        }
      }
    }

    # Apply rounding based on precision matrix
    for (r in 1:nrow(data)) {
      for (c in 1:ncol(data)) {
        if (precision_matrix[r, c] > 0) {
          # Only attempt to round numeric columns
          if (is.numeric(data[[c]]) ||
              (is.character(data[[c]]) &&
               suppressWarnings(!is.na(as.numeric(data[[c]][r]))))) {
            decimals <- precision_matrix[r, c]
            value <- data[[c]][r]

            if (is.na(value)) {
              # Keep NA as is
            } else if (is.na(as.numeric(value))) {
              # Keep text unchanged
            } else {
              num_value <- as.numeric(value)
              rounded_value <- perform_rounding(num_value, decimals)
              threshold_string <- create_threshold_string(decimals)

              # Update numeric data if returning numeric values
              if (return_numeric) {
                numeric_data[[c]][r] <- rounded_value
              }

              # Format the value for string output
              if (rounded_value == 0) {
                # Check if we should use threshold replacement or display actual zero
                if (use_threshold_zero_replacement && abs(num_value) > 0) {
                  data[[c]][r] <- threshold_string
                } else {
                  data[[c]][r] <- format_number(0, decimals)
                }
              } else {
                data[[c]][r] <- format_number(rounded_value, decimals)
              }
            }
          }
        }
      }
    }
  } else {
    # Conventional sequential rounding based on rows_first parameter

    # Sort specifications based on type and order
    row_specs <- all_specs[sapply(all_specs, function(x)
      x$type == "rows")]
    row_specs <- row_specs[order(sapply(row_specs, function(x)
      x$order))]

    col_specs <- all_specs[sapply(all_specs, function(x)
      x$type == "cols")]
    col_specs <- col_specs[order(sapply(col_specs, function(x)
      x$order))]

    # Function to apply rounding to rows or columns
    apply_rounding <- function(spec_list, is_row) {
      for (spec in spec_list) {
        indices <- spec$indices
        decimals <- spec$decimal

        for (idx in indices) {
          if (is_row) {
            # Apply to a specific row, all columns
            for (c in 1:ncol(data)) {
              if (is.numeric(data[[c]]) ||
                  (is.character(data[[c]]) &&
                   suppressWarnings(!is.na(as.numeric(
                     data[[c]][idx]
                   ))))) {
                value <- data[[c]][idx]

                if (is.na(value)) {
                  # Keep NA as is
                } else if (is.na(as.numeric(value))) {
                  # Keep text unchanged
                } else {
                  num_value <- as.numeric(value)
                  rounded_value <- perform_rounding(num_value, decimals)
                  threshold_string <- create_threshold_string(decimals)

                  # Update numeric data if returning numeric values
                  if (return_numeric) {
                    numeric_data[[c]][idx] <- rounded_value
                  }

                  # Format the value for string output
                  if (rounded_value == 0) {
                    # Check if we should use threshold replacement or display actual zero
                    if (use_threshold_zero_replacement && abs(num_value) > 0) {
                      data[[c]][idx] <- threshold_string
                    } else {
                      data[[c]][idx] <- format_number(0, decimals)
                    }
                  } else {
                    data[[c]][idx] <- format_number(rounded_value, decimals)
                  }
                }
              }
            }
          } else {
            # Apply to a specific column, all rows
            if (is.numeric(data[[idx]]) ||
                (is.character(data[[idx]]) &&
                 any(suppressWarnings(!is.na(
                   as.numeric(data[[idx]])
                 ))))) {
              for (r in 1:nrow(data)) {
                value <- data[[idx]][r]

                if (is.na(value)) {
                  # Keep NA as is
                } else if (is.na(as.numeric(value))) {
                  # Keep text unchanged
                } else {
                  num_value <- as.numeric(value)
                  rounded_value <- perform_rounding(num_value, decimals)
                  threshold_string <- create_threshold_string(decimals)

                  # Update numeric data if returning numeric values
                  if (return_numeric) {
                    numeric_data[[idx]][r] <- rounded_value
                  }

                  # Format the value for string output
                  if (rounded_value == 0) {
                    # Check if we should use threshold replacement or display actual zero
                    if (use_threshold_zero_replacement && abs(num_value) > 0) {
                      data[[idx]][r] <- threshold_string
                    } else {
                      data[[idx]][r] <- format_number(0, decimals)
                    }
                  } else {
                    data[[idx]][r] <- format_number(rounded_value, decimals)
                  }
                }
              }
            }
          }
        }
      }
      return(data)
    }

    # Apply rounding based on rows_first parameter
    if (rows_first) {
      # Apply all row specifications first
      data <- apply_rounding(row_specs, TRUE)
      # Then apply all column specifications
      data <- apply_rounding(col_specs, FALSE)
    } else {
      # Apply all column specifications first
      data <- apply_rounding(col_specs, FALSE)
      # Then apply all row specifications
      data <- apply_rounding(row_specs, TRUE)
    }
  }

  # Return the appropriate data frame based on return_numeric setting
  if (return_numeric) {
    return(numeric_data)
  } else {
    return(data)
  }
}
