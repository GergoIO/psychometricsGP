#' fnRoundTable Comprehensive Rounding of Dataframes for Reporting
#'
#' @description
#' Use this function to round dataframes. Define either rows or cols and the number of decimals to round each value row/col to. Rows/Cols can be given as vector of indexes or names. All cells in target rows/cols that cannot be converted to numerics are left as they were. By default rows are rounded first, followed by cols but this logic can be changed. Additionally, a second set of rows/cols can be defined so that they can be rounded to a different number of decimal places. Cell rounding is permanent and the original decimals are not recovered if performing subsequent rounding to a greater number of decimal places.
#'
#' @param data A data frame to be rounded.
#' @param rows (Vector) Row indexes or row names to specify the rows for rounding.
#' @param cols (Vector) Column indexes or column names to specify the columns for rounding.
#' @param rowsDecimal (Integer) The number of decimals for row rounding.
#' @param colsDecimal (Integer) The number of decimals for column rounding.
#' @param rows2 (Vector) Row indexes or row names for a second round of row rounding.
#' @param cols2 (Vector) Column indexes or column names for a second round of column rounding.
#' @param rowsDecimal2 (Integer) The number of decimals for the second round of row rounding.
#' @param colsDecimal2 (Integer) The number of decimals for the second round of column rounding.
#' @param rows3 (Vector) Row indexes or row names for a third round of row rounding.
#' @param cols3 (Vector) Column indexes or column names for a third round of column rounding.
#' @param rowsDecimal3 (Integer) The number of decimals for the third round of row rounding.
#' @param colsDecimal3 (Integer) The number of decimals for the third round of column rounding.
#' @param rowsFirst (Boolean) Indicating whether row rounding should be performed before column rounding.
#'
#' @examples
#' df <- data.frame(
#'   Name = c("John", "Jane", "Alice", "Bob"),
#'   Score1 = c("85.743", -90.123, "NA", -79.862),
#'   Score2 = c(92.567, "NA", -88.923, 95.369),
#'   Score3 = c(100.567, NA, 100.923, 902.329),
#'   Grade = c("A", "B", "C", "A"),
#'   row.names = c("Row1", "Row2", "Row3", "Row4")
#'   )
#'
#' # Example 1: Round specific rows (by row index) with specified decimals
#' rounded_rows_by_index <- fnRoundTable(df, rows = c(1, 3), rowsDecimal = 1)
#'
#' # Example 2: Round specific rows (by row name) with specified decimals
#' rounded_rows_by_name <- fnRoundTable(df, rows = c("Row1", "Row3"), rowsDecimal = 2)
#'
#' # Example 3: Round specific columns (by col index) with specified decimals
#' rounded_cols_by_index <- fnRoundTable(df, cols = c(2, 3), colsDecimal = 1)
#'
#' # Example 4: Round specific columns (by col name) with specified decimals
#' rounded_cols_by_index <- fnRoundTable(df, cols = c("Score1", "Score3"), colsDecimal = 2)
#'
#' # Example 5: Perform row rounding before column rounding (DEFAULT)
#' rounded_rowsFirst <- fnRoundTable(df, rows = c(1), cols = c("Score1", "Score3"), rowsDecimal = 1, colsDecimal = 4)
#'
#' # Example 6: Perform col rounding before row rounding
#' rounded_cols_first <- fnRoundTable(df, rows = c(1), cols = c("Score1", "Score3"), rowsDecimal = 3, colsDecimal = 1, rowsFirst = FALSE)
#'
#' # Example 7: Perform a second round of row and column rounding
#' rounded_second_round <- fnRoundTable(df, rows = c(1), cols = c("Score1", "Score3"), rowsDecimal = 1, colsDecimal = 2,
#'                                    rows2 = c(3), cols2 = c("Score2"), rowsDecimal2 = 2, colsDecimal2 = 1)
#'
#' @return A modified data frame with rounded values. The cells are coerced to strings
#' @export
#' @keywords rounding table dataframe
#' @author Gergo Pinter, \email{gergo.pinter@@plymouth.ac.uk}, \url{https://www.gergo.io}
#'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
fnRoundTable <- function(data,
                         rows = NULL,
                         cols = NULL,
                         rowsDecimal = NULL,
                         colsDecimal = NULL,
                         rows2 = NULL,
                         cols2 = NULL,
                         rowsDecimal2 = NULL,
                         colsDecimal2 = NULL,
                         rows3 = NULL,
                         cols3 = NULL,
                         rowsDecimal3 = NULL,
                         colsDecimal3 = NULL,
                         rowsFirst = TRUE) {
  # Check if both rows and cols are NULL
  if (is.null(rows) && is.null(cols)) {
    stop("At least one of 'rows' or 'cols' must be provided.")
  }

  # If rows is defined then rowsDecimal must be defined (+ vice versa and extend for other vars)
  if (!is.null(rows) && is.null(rowsDecimal)) {
    stop("Argument 'rowsDecimal' must be provided when 'rows' is specified.")
  }

  if (is.null(rows) && !is.null(rowsDecimal)) {
    stop("Argument 'rows' must be provided when 'rowsDecimal' is specified.")
  }

  if (!is.null(cols) && is.null(colsDecimal)) {
    stop("Argument 'colsDecimal' must be provided when 'cols' is specified.")
  }

  if (is.null(cols) && !is.null(colsDecimal)) {
    stop("Argument 'cols' must be provided when 'colsDecimal' is specified.")
  }

  if (!is.null(rows2) && is.null(rowsDecimal2)) {
    stop("Argument 'rowsDecimal2' must be provided when 'rows2' is specified.")
  }

  if (is.null(rows2) && !is.null(rowsDecimal2)) {
    stop("Argument 'rows2' must be provided when 'rowsDecimal2' is specified.")
  }

  if (!is.null(cols2) && is.null(colsDecimal2)) {
    stop("Argument 'colsDecimal2' must be provided when 'cols2' is specified.")
  }

  if (is.null(cols2) && !is.null(colsDecimal2)) {
    stop("Argument 'cols2' must be provided when 'colsDecimal2' is specified.")
  }

    if (!is.null(rows3) && is.null(rowsDecimal3)) {
    stop("Argument 'rowsDecimal3' must be provided when 'rows3' is specified.")
  }

  if (is.null(rows3) && !is.null(rowsDecimal3)) {
    stop("Argument 'rows3' must be provided when 'rowsDecimal3' is specified.")
  }

  if (!is.null(cols3) && is.null(colsDecimal3)) {
    stop("Argument 'colsDecimal3' must be provided when 'cols3' is specified.")
  }

  if (is.null(cols3) && !is.null(colsDecimal3)) {
    stop("Argument 'cols3' must be provided when 'colsDecimal3' is specified.")
  }

  # Get the row names from the data frame
  row_names <- row.names(data)

  # Convert row names to row indexes if row names are provided as rows argument
  # NOTE: Cols do not need the same treatment
  if (!is.null(rows)) {
    if (is.character(rows)) {
      # Check for any row names not present in the data frame
      missing_rows <- setdiff(rows, row_names)
      if (length(missing_rows) > 0) {
        warning(paste(
          "The following row names are not present in the data frame:",
          paste(missing_rows, collapse = ", ")
        ))
      }
      rows <- which(row_names %in% rows)
    }
  }

  # Convert "NA" string cells to actual NA
  data <- data %>%
    mutate(across(where(is.character), ~ ifelse(. == "NA", NA, .)))

  # Define Row Rounding Function
  round_rows <- function(data, rows, decimals) {
    # Set chars that go into sprintf function later (based on rowsDecimal)
    rows_format <- paste("%.", decimals, "f", sep = "")

    data <- data %>%
      # First convert all numeric cells to character
      mutate(across(where(is.numeric), as.character)) %>%
      # Try rounding all character cells (by trying to convert them to numeric)
      # The ^-?\\d*.. regex matches strings that represent numeric values (integers/decimals/negatives)
      # ie cells that can be converted to numeric
      mutate(across(
        where(is.character),
        ~ ifelse(
          row_number() %in% rows &
            !is.na(.),
          ifelse(grepl("^-?\\d*\\.?\\d+$", .), suppressWarnings(sprintf(
            rows_format, round(as.numeric(.), digits = decimals)
          )), .),
          .
        )
      )) %>%
      # Convert all rounded numeric cells back to character
      mutate(across(where(is.numeric), as.character))
  }

  # Define Column Rounding Function
  round_cols <- function(data, cols, decimals) {
    cols_format <- paste("%.", decimals, "f", sep = "")

    data <- data %>%
      # Try rounding all character cells (by trying to convert them to numeric)
      # The ^-?\\d*.. regex matches strings that represent numeric values (integers/decimals/negatives)
      # ie cells that can be converted to numeric
      mutate(across(cols, ~ ifelse(
        !is.na(.), ifelse(grepl("^-?\\d*\\.?\\d+$", .), suppressWarnings(sprintf(
          cols_format, round(as.numeric(.), digits = decimals)
        )), .), .
      ))) %>%
      # Convert all rounded numeric cells back to character
      mutate(across(where(is.numeric), as.character))
  }

  # Perform rounding based on rowsFirst parameter
  if (rowsFirst) {
    # Perform row rounding if rows are specified
    if (!is.null(rows) && !is.null(rowsDecimal)) {
      data <- round_rows(data, rows, rowsDecimal)
    }

    # Perform column rounding if columns are specified
    if (!is.null(cols) && !is.null(colsDecimal)) {
      data <- round_cols(data, cols, colsDecimal)
    }
  } else {
    # Perform column rounding if columns are specified
    if (!is.null(cols) && !is.null(colsDecimal)) {
      data <- round_cols(data, cols, colsDecimal)
    }

    # Perform row rounding if rows are specified
    if (!is.null(rows) && !is.null(rowsDecimal)) {
      data <- round_rows(data, rows, rowsDecimal)
    }
  }

  # Check if a second round of rounding should be performed
  if (!is.null(rows2) ||
      !is.null(cols2) ||
      !is.null(rowsDecimal2) ||
      !is.null(colsDecimal2)) {
    # Perform second round of rounding based on rowsFirst parameter
    if (rowsFirst) {
      # Perform row rounding if rows are specified
      if (!is.null(rows2) && !is.null(rowsDecimal2)) {
        data <- round_rows(data, rows2, rowsDecimal2)
      }

      # Perform column rounding if columns are specified
      if (!is.null(cols2) && !is.null(colsDecimal2)) {
        data <- round_cols(data, cols2, colsDecimal2)
      }
    } else {
      # Perform column rounding if columns are specified
      if (!is.null(cols2) && !is.null(colsDecimal2)) {
        data <- round_cols(data, cols2, colsDecimal2)
      }

      # Perform row rounding if rows are specified
      if (!is.null(rows2) && !is.null(rowsDecimal2)) {
        data <- round_rows(data, rows2, rowsDecimal2)
      }
    }
  }

  # Check if a third round of rounding should be performed
  if (!is.null(rows3) ||
      !is.null(cols3) ||
      !is.null(rowsDecimal3) ||
      !is.null(colsDecimal3)) {
    # Perform third round of rounding based on rowsFirst parameter
    if (rowsFirst) {
      # Perform row rounding if rows are specified
      if (!is.null(rows3) && !is.null(rowsDecimal3)) {
        data <- round_rows(data, rows3, rowsDecimal3)
      }

      # Perform column rounding if columns are specified
      if (!is.null(cols3) && !is.null(colsDecimal3)) {
        data <- round_cols(data, cols3, colsDecimal3)
      }
    } else {
      # Perform column rounding if columns are specified
      if (!is.null(cols3) && !is.null(colsDecimal3)) {
        data <- round_cols(data, cols3, colsDecimal3)
      }

      # Perform row rounding if rows are specified
      if (!is.null(rows3) && !is.null(rowsDecimal3)) {
        data <- round_rows(data, rows3, rowsDecimal3)
      }
    }
  }
  return(data)
}
