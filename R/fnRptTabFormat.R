#' Format a table for presenting in a report
#'
#' @description This function's primary use is for report generation. Given an input dataframe containing only numbers (no text cells. numbers stored as text permissable) the entire dataframe is rounded to the desired number of decimal places. Then, depending on user selection, either the user-defined integer columns or rows are converted back to be integers (displaying no decimal places). The formatted table is returned.
#'
#' @param table (Dataframe) The input data, which will be formatted
#' @param decimals (Integer, Numeric) The number of decimals to round values to
#' @param editColumnsNotRows (OPTIONAL, Boolean) If TRUE, the specified COLUMN numbers will be converted back to integers. If FALSE, the specified ROW numbers will be converted back to integers. If this variable is not defined, none of the coloumns of rows will be converted to integers
#' @param integerColumnsOrRows (OPTIONAL, Numeric, a list or single value eg: 1 or c(1,2,3)) The numbers of the COLUMNS or ROWS (depending on the editColumnsNotRows variable) which will be displayed as integers. If this variable is not defined, none of the columns or rows will be converted to integers.
#'
#' @return The formatted table is returned
#' @export
#'
#' @examples roundedTable <- fnRptTabFormat(table = tab$gradeBounds, decimals = 2, editColumnsNotRows = TRUE, integerColumnsOrRows = c(3, 4))
#'
################################################################################
#'

fnRptTabFormat <-
  function(table = NULL,
           decimals = NULL,
           editColumnsNotRows = NULL,
           integerColumnsOrRows = NULL) {
    if (is.null(table) == TRUE |
        is.null(decimals) == TRUE)
    {
      stop("One of the required variables for this function has not been specified.")
    } else if (# Check if only one of 'editColumnsNotRows' and 'integerColumnsOrRows' are defined
      ((
        is.null(editColumnsNotRows) == TRUE &&
        is.null(integerColumnsOrRows) == FALSE
      ) |
      (
        is.null(editColumnsNotRows) == FALSE &&
        is.null(integerColumnsOrRows) == TRUE
      )) == TRUE) {
      stop(
        'Only one of the variables ("editColumnsNotRows" or "integerColumnsOrRows") has not been defined. To convert some rounded columns or rows back to integer, both variables must be defined. Alternatively, if neither variable is defined, then no columns or rows are converted back to integer following rounding.'
      )
    } else{
      # Convert table to numeric
      table <- mutate_all(table, function(x)
        as.numeric(as.character(x)))

      # Convert table to rounded version
      table <-
        table %>% format(round(decimals), signif(decimals), nsmall = decimals)

      # Check if columns or rows should be converted to integer
      if (is.null(integerColumnsOrRows) == FALSE) {
        # Convert required cols/rows to integer
        if (editColumnsNotRows == TRUE) {
          # Convert specified COLUMNS to integer
          table[, integerColumnsOrRows] <-
            sapply(table[, integerColumnsOrRows], as.integer)
        } else{
          # Convert specified ROWS to integer
          table[integerColumnsOrRows, ] <-
            sapply(table[integerColumnsOrRows, ], as.integer)
        }
      }
      return(table)
    }
  }
