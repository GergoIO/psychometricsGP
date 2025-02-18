#' Color Cells in a Flextable Based on Numeric Values
#'
#' @description
#' Creates a flextable with cell background colors assigned on a gradient based on numeric values.
#' Colors can be applied across all specified columns (global scale) or individually for each column.
#'
#' @param data A data frame or flextable containing numeric data to be colored
#' @param colnames_colour Character vector. Names of columns to apply gradient coloring to.
#' @param colours Character vector. Colors to use for the gradient. At least 2 colors must be specified.
#' @param single_col_colouring Logical. If TRUE, each column is colored based on its own min/max values.
#'   If FALSE (default), coloring is based on values across all specified columns.
#'
#' @details
#' The function applies gradient coloring to numeric columns in a flextable. The gradient
#' is determined by the numeric values in the specified columns and the provided colors.
#'
#' If `single_col_colouring = FALSE` (default), the function finds the minimum and maximum
#' values across all specified columns and creates a single gradient scale that is applied
#' to all columns. This ensures that the same value gets the same color regardless of which
#' column it appears in.
#'
#' If `single_col_colouring = TRUE`, each column is treated independently. The function
#' finds the minimum and maximum values for each column and creates a separate gradient
#' scale for each column. This means that the same value might get different colors in
#' different columns.
#'
#' The function automatically handles input validation, including checking that all specified
#' columns exist and contain numeric data.
#'
#' @return A flextable object with gradient coloring applied to the specified columns.
#'
#' @examples
#' # Load required libraries
#' library(dplyr)
#' library(flextable)
#'
#' # Create example data
#' df <- tibble(
#'   Category = c("A", "B", "C", "D", "E"),
#'   Value1 = c(10, 20, 30, 40, 50),
#'   Value2 = c(5, 15, 25, 35, 45),
#'   Value3 = c(50, 40, 30, 20, 10),
#'   Text = c("low", "low-mid", "mid", "mid-high", "high")
#' )
#'
#' # Basic usage - color two columns on a global scale with two colors
#' ft1 <- fn_colour_columns(df,
#'                        colnames_colour = c("Value1", "Value2"),
#'                        colours = c("white", "red"))
#'
#' # Color three columns individually with a multi-color gradient
#' ft2 <- fn_colour_columns(df,
#'                        colnames_colour = c("Value1", "Value2", "Value3"),
#'                        colours = c("blue", "white", "red"),
#'                        single_col_colouring = TRUE)
#'
#' # Use with existing flextable and more complex gradient
#' ft3 <- flextable(df) %>%
#'   bold(j = "Category") %>%
#'   fn_colour_columns(colnames_colour = c("Value1", "Value2", "Value3"),
#'                   colours = c("purple", "blue", "cyan", "green", "yellow", "orange", "red"))
#'
#' @export
fn_colour_columns <- function(data,
                              colnames_colour,
                              colours,
                              single_col_colouring = FALSE) {

  # Check if at least 2 colors are provided
  if (length(colours) < 2) {
    stop("At least 2 colors must be provided for the gradient")
  }

  # Convert to flextable if not already
  if (!inherits(data, "flextable")) {
    if (!is.data.frame(data)) {
      stop("Input 'data' must be a data frame or a flextable object")
    }
    ft <- flextable(data)
  } else {
    ft <- data
    data <- ft$body$dataset
  }

  # Check if all columns exist
  missing_cols <- setdiff(colnames_colour, colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste("The following columns were not found in the data:",
               paste(missing_cols, collapse = ", ")))
  }

  # Check if all columns are numeric
  non_numeric_cols <- character(0)
  for (col in colnames_colour) {
    if (!is.numeric(data[[col]])) {
      non_numeric_cols <- c(non_numeric_cols, col)
    }
  }
  if (length(non_numeric_cols) > 0) {
    stop(paste("The following columns are not numeric:",
               paste(non_numeric_cols, collapse = ", ")))
  }

  # Get column indices
  col_indices <- which(colnames(data) %in% colnames_colour)

  # Function to get color from value based on min, max and color palette
  get_color <- function(value, min_val, max_val, color_palette) {
    if (is.na(value)) return(NA)
    if (min_val == max_val) return(color_palette[1])  # Handle edge case where all values are equal

    # Normalize value to 0-1 range
    norm_value <- (value - min_val) / (max_val - min_val)

    # Get color from palette
    color_ramp <- colorRampPalette(color_palette)
    colors <- color_ramp(100)
    idx <- max(1, min(100, round(norm_value * 99) + 1))
    return(colors[idx])
  }

  # Apply coloring
  if (single_col_colouring) {
    # Process each column separately
    for (col in colnames_colour) {
      col_idx <- which(colnames(data) == col)
      col_values <- data[[col]]
      min_val <- min(col_values, na.rm = TRUE)
      max_val <- max(col_values, na.rm = TRUE)

      for (i in 1:nrow(data)) {
        value <- data[[col]][i]
        if (!is.na(value)) {
          color <- get_color(value, min_val, max_val, colours)
          ft <- ft %>% bg(i = i, j = col_idx, bg = color)
        }
      }
    }
  } else {
    # Process all columns together
    all_values <- unlist(lapply(colnames_colour, function(col) data[[col]]))
    min_val <- min(all_values, na.rm = TRUE)
    max_val <- max(all_values, na.rm = TRUE)

    for (col in colnames_colour) {
      col_idx <- which(colnames(data) == col)
      for (i in 1:nrow(data)) {
        value <- data[[col]][i]
        if (!is.na(value)) {
          color <- get_color(value, min_val, max_val, colours)
          ft <- ft %>% bg(i = i, j = col_idx, bg = color)
        }
      }
    }
  }

  return(ft)
}
