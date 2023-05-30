#' fnRoundTable - Round Rows/Cols in a table for reports
#'
#' @param data The dataframe to round
#' @param rows (Optional, Vector) Either numeric row indexes OR string row names
#' @param cols (Optional, Vector) Either numeric col indexes OR string col names
#' @param decimals Numeric, integer - the number of decimals to coerce in the chosen rows/cols
#' @param replaceNA (Optional, Default = TRUE) Boolean - decides whether all NAs in the data should be replaced with 0
#'
#' @return Returns the rounded input data
#' @export
#'
#' @examples
#'
#' # Create a sample data frame
#' df <- data.frame(
#'   Value1 = c(10.123, 15.789, 20.456, 25.678, 30.987),
#'   Value2 = c(5.678, 8.912, 12.345, 18.901, 23.456),
#'   Value3 = c(15.432, 19.876, 24.789, 29.123, 35.678),
#'   row.names = c("Row1", "Row2", "Row3", "Row4", "Row5")
#' )
#'
#' # Round specific rows (by row name)
#' rounded_rows <- fnRoundTable(data = df, rows = c("Row2", "Row4"), decimals = 2)
#' print(rounded_rows)
#'
#' # Round specific columns (by col name)
#' rounded_cols <- fnRoundTable(data = df, cols = c("Value2", "Value3"), decimals = 2)
#' print(rounded_cols)
#'
#' #' # Round specific rows (by row index)
#' rounded_rows <- fnRoundTable(data = df, rows = c(1,2), decimals = 1)
#' print(rounded_rows)
#'
#' # Round specific columns (by col index)
#' rounded_cols <- fnRoundTable(data = df, cols = c(4,5), decimals = 1)
#' print(rounded_cols)
#'
################################################################################
#'
fnRoundTable <-
  function(data,
           rows = NULL,
           cols = NULL,
           decimals,
           replaceNA = TRUE) {
    # Must define either cols or rows but not both
    # Check if both variables are defined or both variables are NULL
    if (!(is.null(rows) && is.null(cols)) &&
        !(exists("rows") && exists("cols"))) {
      stop("Please provide either `rows` or `cols`, but not both.")
    }
    if (replaceNA) {
      data <- data %>%
        mutate(across(everything(), ~ replace_na(., 0)))
    }
    # Rounding ROWS/COLS?
    if (!is.null(rows)) {
      # Convert row names to indexes, if necessary
      if (all(is.character(rows))) {
        rows <- match(rows, row.names(data))
      }
      # Round rows
      data <- data %>%
        mutate(across(
          where(is.numeric),
          ~ ifelse(row_number() %in% rows, trimws(format(
            round(., decimals), nsmall = decimals
          )), .)
        ))
    } else if (!is.null(cols)) {
      # Convert col names to indexes, if necessary
      if (all(is.character(cols))) {
        cols <- which(names(data) %in% cols)
      }
      # Round cols
      data <- data %>%
        mutate(across(cols, ~ trimws(format(
          round(., decimals), nsmall = decimals
        ))))
    }
    else{
      stop("Please provide either `rows` or `cols`, but not both.")
    }
    return(data)
  }
