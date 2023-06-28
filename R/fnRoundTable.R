#Latest ver, c2nd rounding round untested

#' fnRoundTable Comprehensive Rounding of Dataframes for Reporting
#'
#' @description
#' Use this function to round dataframes. Define either rows or cols and the number of decimals to round each value row/col to. Rows/Cols can be given as vector of indexes or names. All cells in target rows/cols that cannot be converted to numerics are left as they were. By default rows are rounded first, followed by cols but this logic can be changed. Additionally, a second set of rows/cols can be defined so that they can be rounded to a different number of decimal places
#'
#' @param data A data frame to be rounded.
#' @param rows (Vector) Row indexes or row names to specify the rows for rounding.
#' @param cols (Vector) Column names to specify the columns for rounding.
#' @param row_decimals (Integer) The number of decimals for row rounding.
#' @param col_decimals (Integer) The number of decimals for column rounding.
#' @param rows2 (Vector) Row indexes or row names for a second round of row rounding.
#' @param cols2 (Vector) Column names for a second round of column rounding.
#' @param row_decimals2 (Integer) The number of decimals for the second round of row rounding.
#' @param col_decimals2 (Integer) The number of decimals for the second round of column rounding.
#' @param rows_first (Boolean) Indicating whether row rounding should be performed before column rounding.
#'
#' @examples
#' df <- data.frame(
#'   Name = c("John", "Jane", "Alice", "Bob"),
#'   Score1 = c("85.743", 90.123, "NA", 79.862),
#'   Score2 = c(92.567, "NA", 88.923, 95.369),
#'   Score3 = c(100.567, NA, 100.923, 902.329),
#'   Grade = c("A", "B", "C", "A"),
#'   row.names = c("Row1", "Row2", "Row3", "Row4")
#'
#' # Example 1: Round specific rows (by row index) with specified decimals
#' rounded_rows_ny_index <- fnRoundTable(df, rows = c(1, 3), row_decimals = 1)
#'
#' # Example 2: Round specific rows (by row name) with specified decimals
#' rounded_rows_by_name <- fnRoundTable(df, rows = c("Row1", "Row3"), row_decimals = 2)
#'
#' # Example 3: Round specific columns (by col index) with specified decimals
#' rounded_cols_by_index <- fnRoundTable(df, cols = c(2, 3), col_decimals = 2)
#'
#' # Example 4: Round specific columns (by col name) with specified decimals
#' rounded_cols_by_index <- fnRoundTable(df, cols = c("Score1", "Score3"), col_decimals = 2)
#'
#' # Example 5: Perform row rounding before column rounding (DEFAULT)
#' rounded_rows_first <- fnRoundTable(df, rows = c(1), cols = c("Score1", "Score3"), row_decimals = 1, col_decimals = 4)
#'
#' # Example 5: Perform col rounding before row rounding
#' rounded_rows_first <- fnRoundTable(df, rows = c(1), cols = c("Score1", "Score3"), row_decimals = 3, col_decimals = 1, rows_first = FALSE)
#'
#' # Example 6: Perform a second round of row and column rounding
#' rounded_second_round <- fnRoundTable(df, rows = c(1), cols = c("Score1", "Score3"), row_decimals = 1, col_decimals = 2,
#'                                    rows2 = c(3), cols2 = c("Score2"), row_decimals2 = 2, col_decimals2 = 1)
#'
#' @return A modified data frame with rounded values. The cells are coerced to strings
#' @export
#' @keywords rounding table dataframe
#' @author Gergo Pinter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
fnRoundTable <- function(data,
                         rows = NULL,
                         cols = NULL,
                         row_decimals = NULL,
                         col_decimals = NULL,
                         rows2 = NULL,
                         cols2 = NULL,
                         row_decimals2 = NULL,
                         col_decimals2 = NULL,
                         rows_first = TRUE) {
  # Check if both rows and cols are NULL
  if (is.null(rows) && is.null(cols)) {
    stop("At least one of 'rows' or 'cols' must be provided.")
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

  # Convert "NA" string cells to NA
  data <- data %>%
    mutate(across(where(is.character), ~ ifelse(. == "NA", NA, .)))

  # Define Row Rounding Function
  round_rows <- function(data, rows, decimals) {
    # Set chars that go into sprintf function later (based on row_decimals)
    rows_format <- paste("%.", decimals, "f", sep = "")

    data <- data %>%
      # First convert all numeric cells to character
      mutate(across(where(is.numeric), as.character)) %>%
      # Try rounding all character cells (by trying to convert them to numeric)
      mutate(across(
        where(is.character),
        ~ ifelse(
          row_number() %in% rows &
            !is.na(.),
          ifelse(grepl("^\\d*\\.?\\d+$", .), suppressWarnings(sprintf(
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
      mutate(across(cols, ~ ifelse(
        !is.na(.), ifelse(grepl("^\\d*\\.?\\d+$", .), suppressWarnings(sprintf(
          cols_format, round(as.numeric(.), digits = decimals)
        )), .), .
      ))) %>%
      mutate(across(where(is.numeric), as.character))
  }

  # Perform rounding based on rows_first parameter
  if (rows_first) {
    # Perform row rounding if rows are specified
    if (!is.null(rows) && !is.null(row_decimals)) {
      data <- round_rows(data, rows, row_decimals)
    }

    # Perform column rounding if columns are specified
    if (!is.null(cols) && !is.null(col_decimals)) {
      data <- round_cols(data, cols, col_decimals)
    }
  } else {
    # Perform column rounding if columns are specified
    if (!is.null(cols) && !is.null(col_decimals)) {
      data <- round_cols(data, cols, col_decimals)
    }

    # Perform row rounding if rows are specified
    if (!is.null(rows) && !is.null(row_decimals)) {
      data <- round_rows(data, rows, row_decimals)
    }
  }

  # Check if a second round of rounding should be performed
  if (!is.null(rows2) ||
      !is.null(cols2) ||
      !is.null(row_decimals2) ||
      !is.null(col_decimals2)) {
    # Perform second round of rounding based on rows_first parameter
    if (rows_first) {
      # Perform row rounding if rows are specified
      if (!is.null(rows2) && !is.null(row_decimals2)) {
        data <- round_rows(data, rows2, row_decimals2)
      }

      # Perform column rounding if columns are specified
      if (!is.null(cols2) && !is.null(col_decimals2)) {
        data <- round_cols(data, cols2, col_decimals2)
      }
    } else {
      # Perform column rounding if columns are specified
      if (!is.null(cols2) && !is.null(col_decimals2)) {
        data <- round_cols(data, cols2, col_decimals2)
      }

      # Perform row rounding if rows are specified
      if (!is.null(rows2) && !is.null(row_decimals2)) {
        data <- round_rows(data, rows2, row_decimals2)
      }
    }
  }
  return(data)
}
