#' Add a Table with Accompanying Caption to Officer Report
#'
#' Adds a `flextable` (with autofit layout by default) to an Officer report, along with a caption placed above the table.
#' Supports adding vertical and horizontal lines to specific columns and rows for enhanced formatting.
#'
#' @param report The Officer report object.
#' @param table The table to be added. Can be a `data.frame` or a `flextable`.
#' @param table_count Integer variable storing the current table number (e.g., usually named `table_count`).
#' @param caption String, the caption to add above the table.
#' @param stop_trailing_line Logical. If `TRUE`, no empty line is added after the table. Defaults to `FALSE`.
#' @param stop_flextable_conversion Logical. If `TRUE`, the table is assumed to be a flextable and will not be converted. Defaults to `FALSE`.
#' @param vertical_lines Numeric vector specifying the columns where vertical lines should be added. Defaults to `NULL`.
#' @param horizontal_lines Numeric vector specifying the rows where horizontal lines should be added. Defaults to `NULL`.
#' @param ... Additional arguments passed to `flextable::body_add_flextable()`, such as text alignment.
#'
#' @return The updated Officer report object with the added table and caption. The `table_count` variable is incremented by 1 in the calling environment.
#' @export
#'
#' @examples
#' library(officer)
#' library(flextable)
#'
#' # Create example data frame
#' test_df <- data.frame(
#'   col1 = c("A", "B", "C", "D"),
#'   col2 = c(10, 20, 30, 40),
#'   col3 = c(TRUE, FALSE, TRUE, FALSE)
#' )
#'
#' # Initialise Officer report
#' my_report <- read_docx()
#'
#' # Initialise table count
#' table_count <- 1
#'
#' # Add table to report
#' my_report <- fn_report_add_table(
#'   report = my_report,
#'   table = test_df,
#'   table_count = table_count,
#'   caption = "Example Table",
#'   vertical_lines = c(2),
#'   horizontal_lines = c(2),
#'   stop_trailing_line = FALSE
#' )
fn_report_add_table <- function(
    report = NULL,
    table = NULL,
    table_count = NULL,
    caption = NULL,
    stop_trailing_line = FALSE,
    stop_flextable_conversion = FALSE,
    vertical_lines = NULL,
    horizontal_lines = NULL,
    ...
) {
  if (is.null(report) | is.null(table) | is.null(caption) | is.null(table_count)) {
    stop("One of the required variables for this function has not been specified.")
  }
  # Add caption above the table
  report <- officer::body_add_par(report, paste0("Table ", table_count, ": ", caption), style = "caption")
  # Update table count in caller's environment
  assign(deparse(substitute(table_count)), table_count + 1, envir = parent.frame())
  # Convert to flextable unless told not to
  if (!stop_flextable_conversion) {
    table <- flextable::qflextable(table)
  }
  # Set autofit layout
  table <- flextable::set_table_properties(table, layout = "autofit")
  # Add vertical lines if specified
  if (!is.null(vertical_lines)) {
    for (col in vertical_lines) {
      table <- flextable::vline(table, j = col, border = officer::fp_border(width = 1, color = "grey60"))
    }
  }
  # Add horizontal lines if specified
  if (!is.null(horizontal_lines)) {
    for (row in horizontal_lines) {
      table <- flextable::hline(table, i = row, border = officer::fp_border(width = 1, color = "grey60"))
    }
  }
  # Add the table to the report
  report <- flextable::body_add_flextable(report, value = table, ...)
  # Optionally add an empty line
  if (!stop_trailing_line) {
    report <- officer::body_add_par(report, "")
  }
  return(report)
}
