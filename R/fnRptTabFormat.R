#' Format a table for presenting in a report
#'
#' @description This function's primary use is for report generation. Given an input dataframe containing only numbers (no text cells. numbers stored as text permissable) the entire dataframe is rounded to the desired number of decimal places. Then, depending on user selection, either the user-defined integer columns or rows are converted back to be integers (displaying no decimal places). The formatted table is returned.
#'
#' @param table (Dataframe) The input data, which will be formatted
#' @param decimals (Integer, Numeric) The number of decimals to round values to
#' @param editColumnsNotRows (Boolean) If TRUE, the specified COLUMN numbers will be converted back to integers. If FALSE, the specified ROW numbers will be converted back to integers.
#' @param integerColumnsOrRows (Numeric, a list or single value eg: 1 or c(1,2,3)) The numbers of the COLUMNS or ROWS (depending on the editColumnsNotRows variable) which will be displayed as integers
#'
#' @return The formatted table is returned
#' @export
#'
#' @examples roundedTable <- fnRptTabFormat(table = tab$gradeBounds, decimals = 2, editColumnsNotRows = TRUE, integerColumnsOrRows = c(3, 4))
#'
################################################################################
#'

fnRptTabFormat <-
  function(table,
           decimals,
           editColumnsNotRows,
           integerColumnsOrRows) {
    if (is.null(table) == TRUE |
        is.null(decimals) == TRUE |
        is.null(editColumnsNotRows) == TRUE |
        is.null(integerColumnsOrRows) == TRUE)
    {
      stop("One of the required variables for this function has not been specified.")
    } else{
      # Convert table to numeric
      table <- mutate_all(table, function(x)
        as.numeric(as.character(x)))

      # Convert table to rounded version
      table <-
        table %>% format(round(decimals), signif(decimals), nsmall = decimals)

      # Convert required cols/rows to integer
      if (editColumnsNotRows == TRUE) {
        table[, integerColumnsOrRows] <-
          sapply(table[, integerColumnsOrRows], as.integer)
      } else{
        table[integerColumnsOrRows, ] <-
          sapply(table[integerColumnsOrRows, ], as.integer)
      }
      return(table)
    }
  }
