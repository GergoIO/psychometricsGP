#' Highlight Correct and Incorrect Answers in Assessment Data
#'
#' @description
#' Creates a flextable with colored highlighting to indicate correct and incorrect answers
#' in assessment data. Supports both MCQ (Multiple Choice Questions) and VSAQ (Very Short
#' Answer Questions) formats. For MCQ items, highlights correct answers and creates a smooth
#' color gradient for top incorrect answers based on their values.
#'
#' @param data A data frame or flextable containing assessment data
#' @param colname_correct_answer Character. The name of the column containing correct answer indicators.
#'   Default is "CorrectResponse". Can be NULL for VSAQ-only datasets.
#' @param colname_item_category Character. The name of the column indicating item types.
#'   Default is "ItemCategory".
#' @param prefix_mcq Character. The prefix for MCQ option columns. Default is "MCQ_Option_".
#' @param prefix_vsaq Character. The prefix for VSAQ columns. Default is "VSAQ_".
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
#'   ItemCategory = c("MCQ", "MCQ", "VSAQ", "MCQ", "VSAQ"),
#'   CorrectResponse = c(4, 2, NA, 3, NA),
#'   MCQ_Option_1 = c(0.1, 0.2, 0, 0.3, 0),
#'   MCQ_Option_2 = c(0.2, 0.7, 0, 0.1, 0),
#'   MCQ_Option_3 = c(0.3, 0.1, 0, 0.6, 0),
#'   MCQ_Option_4 = c(0.8, 0.0, 0, 0.2, 0),
#'   VSAQ_Correct = c(NA, NA, 0.8, NA, 0.9),
#'   VSAQ_Incorrect = c(NA, NA, 0.2, NA, 0.5)
#' )
#'
#' # Basic usage with gradient colors and bold formatting
#' ft1 <- fn_colour_responses(
#'   data = df,
#'   colour_incorrect = c("red", "orange", "yellow"),
#'   highlight_top_n_incorrect = 3,
#'   bold_correct = TRUE,
#'   bold_top_incorrect = TRUE
#' )
#'
#' # With borders and minimum threshold
#' ft2 <- fn_colour_responses(
#'   data = df,
#'   colour_incorrect = c("pink", "lightyellow"),
#'   highlight_top_n_incorrect = 2,
#'   min_value_threshold = 0.2,
#'   border_correct = TRUE,
#'   border_top_incorrect = TRUE
#' )
#'
#' @export
fn_colour_responses <- function(data,
                                 colname_correct_answer = "CorrectResponse",
                                 colname_item_category = "ItemCategory",
                                 prefix_mcq = "MCQ_Option_",
                                 prefix_vsaq = "VSAQ_",
                                 colour_correct = "lightgreen",
                                 colour_incorrect = c("lightpink"),
                                 highlight_correct = TRUE,
                                 highlight_top_n_incorrect = 1,
                                 min_value_threshold = 0,
                                 border_correct = FALSE,
                                 border_top_incorrect = FALSE,
                                 bold_correct = FALSE,
                                 bold_top_incorrect = FALSE) {

  # Input validation function
  validate_inputs <- function() {
    # Check color validity
    tryCatch({
      col2rgb(colour_correct)
      sapply(colour_incorrect, col2rgb)
    }, error = function(e) {
      stop("Invalid color specified. Please check colour_correct and colour_incorrect values.")
    })

    # Check numeric parameters
    if (!is.numeric(highlight_top_n_incorrect) || highlight_top_n_incorrect < 0) {
      stop("highlight_top_n_incorrect must be a non-negative number")
    }
    if (!is.numeric(min_value_threshold) || min_value_threshold < 0 || min_value_threshold > 1) {
      stop("min_value_threshold must be between 0 and 1")
    }

    # Check required columns
    if (!colname_item_category %in% colnames(data)) {
      stop(sprintf("Required column '%s' not found in data", colname_item_category))
    }

    # Check for MCQ columns if MCQ items exist
    if ("MCQ" %in% data[[colname_item_category]]) {
      if (is.null(colname_correct_answer) || !colname_correct_answer %in% colnames(data)) {
        stop(sprintf("Column '%s' required for MCQ items not found in data", colname_correct_answer))
      }
      mcq_cols <- grep(paste0("^", prefix_mcq), colnames(data), value = TRUE)
      if (length(mcq_cols) == 0) {
        stop(sprintf("No columns with prefix '%s' found for MCQ items", prefix_mcq))
      }
    }

    # Check for VSAQ columns if VSAQ items exist
    if ("VSAQ" %in% data[[colname_item_category]]) {
      vsaq_cols <- c(paste0(prefix_vsaq, "Correct"), paste0(prefix_vsaq, "Incorrect"))
      missing_cols <- vsaq_cols[!vsaq_cols %in% colnames(data)]
      if (length(missing_cols) > 0) {
        stop(sprintf("Required VSAQ columns missing: %s", paste(missing_cols, collapse = ", ")))
      }
    }
    # Add validation for bolding
    if (!is.logical(bold_correct)) {
      stop("bold_correct must be a logical value (TRUE/FALSE)")
    }
    if (!is.logical(bold_top_incorrect)) {
      stop("bold_top_incorrect must be a logical value (TRUE/FALSE)")
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
    apply(rgb_colors, 1, function(x) rgb(x[1], x[2], x[3], maxColorValue = 255))
  }

  # Process MCQ items
  process_mcq <- function(ft, row_idx) {
    if (is.na(data[[colname_correct_answer]][row_idx])) return(ft)

    # Get correct answer column
    correct_col <- paste0(prefix_mcq, data[[colname_correct_answer]][row_idx])

    # Highlight correct answer
    if (highlight_correct && correct_col %in% colnames(data)) {
      col_idx <- which(colnames(data) == correct_col)
      ft <- ft %>%
        bg(i = row_idx, j = col_idx, bg = colour_correct)

      if (border_correct) {
        ft <- ft %>%
          flextable::border(i = row_idx, j = col_idx, border = fp_border(color = "black", width = 1), part = "body")
      }

      if (bold_correct) {
        ft <- ft %>%
          flextable::bold(i = row_idx, j = col_idx)
      }
    }

    # Process incorrect answers
    if (highlight_top_n_incorrect > 0) {
      incorrect_cols <- setdiff(grep(paste0("^", prefix_mcq), colnames(data), value = TRUE),
                                correct_col)

      if (length(incorrect_cols) > 0) {
        # Get values and sort
        incorrect_values <- sapply(incorrect_cols, function(col) {
          val <- data[[col]][row_idx]
          if (is.numeric(val)) val else as.numeric(val)
        })

        # Filter by threshold and sort
        valid_incorrect <- incorrect_values[!is.na(incorrect_values) &
                                              incorrect_values >= min_value_threshold]
        if (length(valid_incorrect) > 0) {
          sorted_incorrect <- sort(valid_incorrect, decreasing = TRUE)

          # Get colors for highlighting
          n_highlights <- min(highlight_top_n_incorrect, length(sorted_incorrect))
          highlight_colors <- create_color_gradient(n_highlights)

          # Apply highlights
          for (i in 1:n_highlights) {
            col <- names(sorted_incorrect)[i]
            col_idx <- which(colnames(data) == col)

            ft <- ft %>%
              bg(i = row_idx, j = col_idx, bg = highlight_colors[i])

            # Add border to top incorrect if requested
            if (border_top_incorrect && i == 1) {
              ft <- ft %>%
                flextable::border(i = row_idx, j = col_idx, border = fp_border(color = "black", width = 1), part = "body")
            }

            # Add bold to top incorrect if requested
            if (bold_top_incorrect && i == 1) {
              ft <- ft %>%
                flextable::bold(i = row_idx, j = col_idx)
            }
          }
        }
      }
    }

    return(ft)
  }

  # Process VSAQ items
  process_vsaq <- function(ft, row_idx) {
    vsaq_correct_col <- paste0(prefix_vsaq, "Correct")
    vsaq_incorrect_col <- paste0(prefix_vsaq, "Incorrect")

    # Handle correct answer
    if (highlight_correct &&
        !is.na(data[[vsaq_correct_col]][row_idx]) &&
        data[[vsaq_correct_col]][row_idx] > 0) {
      col_idx <- which(colnames(data) == vsaq_correct_col)
      ft <- ft %>%
        bg(i = row_idx, j = col_idx, bg = colour_correct)

      if (border_correct) {
        ft <- ft %>%
          flextable::border(i = row_idx, j = col_idx, border = fp_border(color = "black", width = 1), part = "body")
      }

      if (bold_correct) {
        ft <- ft %>%
          flextable::bold(i = row_idx, j = col_idx)
      }
    }

    # Handle incorrect answer
    if (highlight_top_n_incorrect > 0 &&
        !is.na(data[[vsaq_incorrect_col]][row_idx]) &&
        data[[vsaq_incorrect_col]][row_idx] >= min_value_threshold) {
      col_idx <- which(colnames(data) == vsaq_incorrect_col)
      ft <- ft %>%
        bg(i = row_idx, j = col_idx, bg = colour_incorrect[1])

      if (border_top_incorrect) {
        ft <- ft %>%
          flextable::border(i = row_idx, j = col_idx, border = fp_border(color = "black", width = 1), part = "body")
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

  # Process each row
  for (i in 1:nrow(data)) {
    if (data[[colname_item_category]][i] == "MCQ") {
      ft <- process_mcq(ft, i)
    } else if (data[[colname_item_category]][i] == "VSAQ") {
      ft <- process_vsaq(ft, i)
    }
  }

  return(ft)
}
