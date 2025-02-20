#' Highlight Correct and Incorrect Answers in Assessment Data
#'
#' @description
#' Creates a flextable with colored highlighting to indicate correct and incorrect answers
#' in assessment data. Supports both MCQ (Multiple Choice Questions) and Non-MCQ (Very Short
#' Answer Questions) formats. For MCQ items, highlights correct answers and creates a smooth
#' color gradient for top incorrect answers based on their values.
#'
#' @param data A data frame or flextable containing assessment data
#' @param colname_correct_answer Character. The name of the column containing correct answer indicators.
#'   Default is "CorrectResponse". Can be NULL for Non-MCQ-only datasets.
#' @param colname_item_category Character. The name of the column indicating item types.
#'   Default is "ItemCategory".
#' @param prefix_mcq Character. The prefix for MCQ option columns. Default is "MCQ_Option_".
#' @param prefix_non_mcq Character. The prefix for Non-MCQ columns. Default is "Non-MCQ_".
#' @param colour_correct Character. The color to use for correct answers. Default is "lightgreen".
#' @param colour_incorrect Character vector. Colors to create gradient for incorrect answers.
#'   Colors will be interpolated if more highlights are requested than colors provided.
#' @param highlight_correct Logical. Whether to highlight correct answers. Default is TRUE.
#' @param highlight_top_n_incorrect Numeric. Number of top incorrect answers to highlight.
#'   Default is 1.
#' @param min_value_threshold Numeric. Minimum value threshold for highlighting incorrect answers.
#'   Default is 0.
#' @param border_correct Logical. Whether to add border to correct answers. Default is FALSE.
#' @param border_top_incorrect Logical. Whether to add border to top incorrect answer. Default is FALSE.
#' @param bold_correct Logical. Whether to bold correct answers. Default is FALSE.
#' @param bold_top_incorrect Logical. Whether to bold top incorrect answer. Default is FALSE.
#' @param horizontal_lines Numeric vector. Row indices where horizontal lines should be added.
#'   Default is NULL (no lines).
#' @param vertical_lines Numeric vector. Column indices where vertical lines should be added.
#'   Default is NULL (no lines).
#' @param border_colour Character. Color for borders around correct and incorrect answers.
#'   Default is "black".
#' @param border_width Numeric. Width for borders around correct and incorrect answers.
#'   Default is 1.5.
#' @param line_colour Character. Color for horizontal and vertical lines.
#'   Default is "black".
#' @param line_width Numeric. Width for horizontal and vertical lines.
#'   Default is 1.
#'
#' @return A flextable object with highlighting applied based on the specified parameters.
#'
#' @examples
#' # Load required libraries
#' library(dplyr)
#' library(flextable)
#' library(colorRamp2)
#'
#' # Create example data
#' df <- tibble(
#'   ItemCategory = c("MCQ", "MCQ", "Non-MCQ", "MCQ", "Non-MCQ"),
#'   CorrectResponse = c(4, 2, NA, 3, NA),
#'   MCQ_Option_1 = c(0.1, 0.2, 0, 0.3, 0),
#'   MCQ_Option_2 = c(0.2, 0.7, 0, 0.1, 0),
#'   MCQ_Option_3 = c(0.3, 0.1, 0, 0.6, 0),
#'   MCQ_Option_4 = c(0.8, 0.0, 0, 0.2, 0),
#'   Non_MCQ_Correct = c(NA, NA, 0.8, NA, 0.9),
#'   Non_MCQ_Incorrect = c(NA, NA, 0.2, NA, 0.5)
#' )
#'
#' # Basic usage with custom lines and borders
#' ft1 <- fn_colour_responses(
#'   data = df,
#'   colour_incorrect = c("red", "orange", "yellow"),
#'   highlight_top_n_incorrect = 3,
#'   bold_correct = TRUE,
#'   bold_top_incorrect = TRUE,
#'   horizontal_lines = c(2, 4),
#'   vertical_lines = c(3, 5),
#'   border_colour = "navy",
#'   line_colour = "grey",
#'   border_width = 2,
#'   line_width = 0.5
#' )
#'
#' @export
fn_colour_responses <- function(data,
                                 colname_correct_answer = "CorrectResponse",
                                 colname_item_category = "ItemCategory",
                                 prefix_mcq = "MCQ_Option_",
                                 prefix_non_mcq = "Non-MCQ_",
                                 colour_correct = "lightgreen",
                                 colour_incorrect = c("lightpink"),
                                 highlight_correct = TRUE,
                                 highlight_top_n_incorrect = 1,
                                 min_value_threshold = 0,
                                 border_correct = FALSE,
                                 border_top_incorrect = FALSE,
                                 bold_correct = FALSE,
                                 bold_top_incorrect = FALSE,
                                 horizontal_lines = NULL,
                                 vertical_lines = NULL,
                                 border_colour = "black",
                                 border_width = 1.5,
                                 line_colour = "grey40",
                                 line_width = 1) {
  # Input validation function
  validate_inputs <- function() {
    # Check color validity
    tryCatch({
      col2rgb(colour_correct)
      sapply(colour_incorrect, col2rgb)
    }, error = function(e) {
      stop(
        "Invalid color specified. Please check colour_correct and colour_incorrect values."
      )
    })

    # Check numeric parameters
    if (!is.numeric(highlight_top_n_incorrect) ||
        highlight_top_n_incorrect < 0) {
      stop("highlight_top_n_incorrect must be a non-negative number")
    }
    if (!is.numeric(min_value_threshold) ||
        min_value_threshold < 0 || min_value_threshold > 1) {
      stop("min_value_threshold must be between 0 and 1")
    }

    # Validate line indices
    if (!is.null(horizontal_lines)) {
      if (!is.numeric(horizontal_lines) || any(horizontal_lines < 1)) {
        stop("horizontal_lines must be a vector of positive integers")
      }
    }
    if (!is.null(vertical_lines)) {
      if (!is.numeric(vertical_lines) || any(vertical_lines < 1)) {
        stop("vertical_lines must be a vector of positive integers")
      }
    }

    # Check required columns
    if (!colname_item_category %in% colnames(data)) {
      stop(sprintf(
        "Required column '%s' not found in data",
        colname_item_category
      ))
    }

    # Check for MCQ columns if MCQ items exist
    if ("MCQ" %in% data[[colname_item_category]]) {
      if (is.null(colname_correct_answer) ||
          !colname_correct_answer %in% colnames(data)) {
        stop(
          sprintf(
            "Column '%s' required for MCQ items not found in data",
            colname_correct_answer
          )
        )
      }
      mcq_cols <- grep(paste0("^", prefix_mcq), colnames(data), value = TRUE)
      if (length(mcq_cols) == 0) {
        stop(sprintf(
          "No columns with prefix '%s' found for MCQ items",
          prefix_mcq
        ))
      }
    }

    # Check for Non-MCQ columns if Non-MCQ items exist
    if ("Non-MCQ" %in% data[[colname_item_category]]) {
      non_mcq_cols <- c(paste0(prefix_non_mcq, "Correct"),
                     paste0(prefix_non_mcq, "Incorrect"))
      missing_cols <- non_mcq_cols[!non_mcq_cols %in% colnames(data)]
      if (length(missing_cols) > 0) {
        stop(sprintf(
          "Required 'Non-MCQ' columns missing: %s",
          paste(missing_cols, collapse = ", ")
        ))
      }
    } else {
      message("No Non-MCQ items found in data. Skipping Non-MCQ validation.")
    }
  }

  # Create color gradient function
  create_color_gradient <- function(n_colors) {
    if (n_colors <= length(colour_incorrect)) {
      return(colour_incorrect[1:n_colors])
    }

    # Create color interpolation function
    ramp <- colorRamp(colour_incorrect)

    # Generate colors
    rgb_colors <- ramp(seq(0, 1, length.out = n_colors))
    apply(rgb_colors, 1, function(x)
      rgb(x[1], x[2], x[3], maxColorValue = 255))
  }

  # Process MCQ items with consistent coloring for identical values
  process_mcq <- function(ft, row_idx) {
    if (is.na(data[[colname_correct_answer]][row_idx]))
      return(ft)

    # Get correct answer column
    correct_col <- paste0(prefix_mcq, data[[colname_correct_answer]][row_idx])

    # Highlight correct answer
    if (highlight_correct && correct_col %in% colnames(data)) {
      col_idx <- which(colnames(data) == correct_col)
      ft <- ft %>%
        bg(i = row_idx, j = col_idx, bg = colour_correct)

      if (border_correct) {
        ft <- ft %>%
          flextable::border(
            i = row_idx,
            j = col_idx,
            border = fp_border(color = border_colour, width = border_width),
            part = "body"
          )
      }

      if (bold_correct) {
        ft <- ft %>%
          flextable::bold(i = row_idx, j = col_idx)
      }
    }

    # Process incorrect answers
    if (highlight_top_n_incorrect > 0) {
      incorrect_cols <- setdiff(grep(paste0("^", prefix_mcq), colnames(data), value = TRUE), correct_col)

      if (length(incorrect_cols) > 0) {
        # Get values for incorrect answers
        incorrect_values <- sapply(incorrect_cols, function(col) {
          val <- data[[col]][row_idx]
          if (is.numeric(val))
            val
          else
            as.numeric(val)
        })

        # Filter by threshold
        valid_incorrect <- incorrect_values[!is.na(incorrect_values) &
                                              incorrect_values >= min_value_threshold]

        if (length(valid_incorrect) > 0) {
          # Get unique values and sort them
          unique_values <- sort(unique(valid_incorrect), decreasing = TRUE)

          # Determine how many unique values to highlight
          n_highlights <- min(highlight_top_n_incorrect, length(unique_values))

          # Get colors for the unique values
          highlight_colors <- create_color_gradient(n_highlights)

          # Create a mapping of values to colors
          value_color_map <- setNames(highlight_colors[1:length(unique_values)], unique_values)

          # Apply highlights based on value-color mapping
          for (col in names(valid_incorrect)) {
            value <- valid_incorrect[col]
            if (value %in% unique_values[1:n_highlights]) {
              col_idx <- which(colnames(data) == col)
              color_idx <- which(unique_values == value)

              ft <- ft %>%
                bg(i = row_idx,
                   j = col_idx,
                   bg = value_color_map[as.character(value)])

              # Add border to top value if requested
              if (border_top_incorrect && color_idx == 1) {
                ft <- ft %>%
                  flextable::border(
                    i = row_idx,
                    j = col_idx,
                    border = fp_border(color = border_colour, width = border_width),
                    part = "body"
                  )
              }

              # Add bold to top value if requested
              if (bold_top_incorrect && color_idx == 1) {
                ft <- ft %>%
                  flextable::bold(i = row_idx, j = col_idx)
              }
            }
          }
        }
      }
    }

    return(ft)
  }

  # Process Non-MCQ items
  process_non_mcq <- function(ft, row_idx) {
    non_mcq_correct_col <- paste0(prefix_non_mcq, "Correct")
    non_mcq_incorrect_col <- paste0(prefix_non_mcq, "Incorrect")

    # Handle correct answer
    if (highlight_correct &&
        !is.na(data[[non_mcq_correct_col]][row_idx]) &&
        data[[non_mcq_correct_col]][row_idx] > 0) {
      col_idx <- which(colnames(data) == non_mcq_correct_col)
      ft <- ft %>%
        bg(i = row_idx, j = col_idx, bg = colour_correct)

      if (border_correct) {
        ft <- ft %>%
          flextable::border(
            i = row_idx,
            j = col_idx,
            border = fp_border(color = border_colour, width = border_width),
            part = "body"
          )
      }

      if (bold_correct) {
        ft <- ft %>%
          flextable::bold(i = row_idx, j = col_idx)
      }
    }

    # Handle incorrect answer
    if (highlight_top_n_incorrect > 0 &&
        !is.na(data[[non_mcq_incorrect_col]][row_idx]) &&
        data[[non_mcq_incorrect_col]][row_idx] >= min_value_threshold) {
      col_idx <- which(colnames(data) == non_mcq_incorrect_col)
      ft <- ft %>%
        bg(i = row_idx, j = col_idx, bg = colour_incorrect[1])

      if (border_top_incorrect) {
        ft <- ft %>%
          flextable::border(
            i = row_idx,
            j = col_idx,
            border = fp_border(color = border_colour, width = border_width),
            part = "body"
          )
      }

      if (bold_top_incorrect) {
        ft <- ft %>%
          flextable::bold(i = row_idx, j = col_idx)
      }
    }

    return(ft)
  }


  # Main function execution
  validate_inputs()

  # Convert to flextable if needed
  if (!inherits(data, "flextable")) {
    if (!is.data.frame(data)) {
      stop("Input 'data' must be a data frame or a flextable object")
    }
    ft <- flextable(data)
  } else {
    ft <- data
    data <- ft$body$dataset
  }

  # Add horizontal lines
  if (!is.null(horizontal_lines)) {
    for (row in horizontal_lines) {
      if (row <= nrow(data)) {
        ft <- ft %>%
          hline(i = row,
                border = fp_border(color = line_colour, width = line_width))
      }
    }
  }

  # Add vertical lines
  if (!is.null(vertical_lines)) {
    for (col in vertical_lines) {
      if (col <= ncol(data)) {
        ft <- ft %>%
          vline(j = col,
                border = fp_border(color = line_colour, width = line_width))
      }
    }
  }

  # Process each row
  for (i in 1:nrow(data)) {
    if (data[[colname_item_category]][i] == "MCQ") {
      ft <- process_mcq(ft, i)
    } else if (data[[colname_item_category]][i] == "Non-MCQ") {
      ft <- process_non_mcq(ft, i)
    }
  }

  return(ft)
}
