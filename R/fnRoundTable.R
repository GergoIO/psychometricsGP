#Latest ver, c2nd rounding round untested

#' fnRoundTable Comprehensive Rounding of Dataframes for Reporting
#'
#' @param data
#' @param rows
#' @param cols
#' @param row_decimals
#' @param col_decimals
#' @param rows2
#' @param cols2
#' @param row_decimals2
#' @param col_decimals2
#' @param rows_first
#'
#' @return
#' @export
#'
#' @examples
#' # Example usage
data <- data.frame(
  Name = c("John", "Jane", "Alice", "Bob"),
  Score1 = c("85.743", 90.123, "NA", 79),
  Score2 = c(92.567, "NA", 88.923, 95.369),
  Score3 = c(100.567, NA, 100.923, 902.329),
  Grade = c("A", "B", "C", "A")
)
# Assign row names to the data frame
row_names <- paste0("Row", 1:nrow(data))
row.names(data) <- row_names

# Round rows with specified decimals
rounded <-
  round_rows(
    data,
    rows = c("Row1", "Row2"),
    cols = c("Score2"),
    row_decimals = 7,
    col_decimals = 0,
    rows_first = FALSE,
    rows2 = 4,
    row_decimals2 = 1
  )
print(rounded)
#'
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
      !is.null(row_decimals2) || !is.null(col_decimals2)) {
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
  data
}

