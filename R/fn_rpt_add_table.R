#' Add a table with accompanying caption
#'
#' @description
#' This function adds a `flextable` (with `autofit` layout by default) to an Officer report, along with a caption placed above the table. It supports adding vertical and horizontal lines to specific columns and rows.
#'
#' @param report The Officer report object.
#' @param table The table to be added. It can be a `data.frame` or an already converted `flextable`.
#' @param table_count The variable storing the current number of tables in the report (integer, usually named `count_tab`).
#' @param caption The caption to add above the table (string).
#' @param stop_trailing_line Logical. If `TRUE`, no empty line is added after the table. Defaults to `FALSE`.
#' @param stop_flextable_conversion Logical. If `TRUE`, the table is assumed to be a `flextable` and no conversion is performed. Defaults to `FALSE`.
#' @param vertical_lines A numeric vector specifying the columns where vertical lines should be added. Defaults to `NULL`.
#' @param horizontal_lines A numeric vector specifying the rows where horizontal lines should be added. Defaults to `NULL`.
#' @param ... Additional arguments passed to `officer::body_add_flextable()`, such as text alignment (`align = "left"`).
#'
#' @return The function returns the updated Officer report object with the added table and caption. The `table_count` variable in the global environment is incremented by 1.
#' @export
#'
#' @examples
#' # Create an example Officer report
#' library(officer)
#' library(flextable)
#'
#' # Example data frame
#' test_df <- data.frame(
#'   Column1 = c("A", "B", "C", "D"),
#'   Column2 = c(10, 20, 30, 40),
#'   Column3 = c(TRUE, FALSE, TRUE, FALSE)
#' )
#'
#' # Initialize Officer report
#' rpt <- read_docx()
#'
#' # Initialize table counter
#' count_tab <- 1
#'
#' # Add the test_df as a table to the report
#' rpt <- fn_rpt_add_table(
#'   report = rpt,
#'   table = test_df,
#'   table_count = count_tab,
#'   caption = "Example Table Caption",
#'   vertical_lines = c(2),        # Add a vertical line after Column 2
#'   horizontal_lines = c(3),      # Add a horizontal line after Row 3
#'   stop_trailing_line = FALSE
#' )

################################################################################

fn_rpt_add_table <- function(
    report = NULL,
    table = NULL,
    table_count = NULL,
    caption = NULL,
    stop_trailing_line = FALSE,
    stop_flextable_conversion = FALSE,
    vertical_lines = NULL,       # New var for vertical lines
    horizontal_lines = NULL,     # New var for horizontal lines
    ...
) {
  if (is.null(report) | is.null(table) | is.null(caption) | is.null(table_count)) {
    stop("One of the required variables for this function has not been specified.")
  }

  # Add caption above the table
  report <- officer::body_add_par(report, paste0("Table ", table_count, ": ", caption), style = "caption")

  # Update the table count in the main script
  assign(deparse(substitute(table_count)), table_count + 1, envir = globalenv())

  # Convert table to flextable if not already done
  if (!stop_flextable_conversion) {
    table <- flextable::qflextable(table)
  }

  # Default to autofit layout
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
  report <- body_add_flextable(report, value = table, ...)

  # Add an empty paragraph if stop_trailing_line is FALSE or NULL
  if (!stop_trailing_line) {
    report <- officer::body_add_par(report, "")
  }

  return(report)
}
