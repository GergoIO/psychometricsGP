#' Highlight Correct and Incorrect Answers in Assessment Data
#'
#' @description
#' Creates a flextable with colored highlighting to indicate correct and incorrect answers
#' in assessment data. Supports both MCQ (Multiple Choice Questions) and VSAQ (Very Short
#' Answer Questions) formats. For MCQ items, highlights correct answers and can optionally
#' highlight incorrect answers by rank of their values. For VSAQ items, highlights the
#' "Correct" and "Incorrect" columns.
#'
#' @param data A data frame or flextable containing assessment data
#' @param colname_correct_answer Character. The name of the column containing correct answer indicators.
#'   Default is "CorrectResponse". Can be NULL for VSAQ-only datasets.
#' @param colname_item_category Character. The name of the column indicating item types.
#'   Default is "ItemCategory".
#' @param prefix_mcq Character. The prefix for MCQ option columns. Default is "MCQ_Option_".
#' @param prefix_vsaq Character. The prefix for VSAQ columns. Default is "VSAQ_".
#' @param colour_correct Character. The color to use for correct answers. Default is "lightgreen".
#' @param colour_incorrect Character vector. The colors to use for incorrect answers, in order of
#'   rank (highest value first). Default is c("lightpink").
#' @param add_colour_correct Logical. Whether to highlight correct answers. Default is TRUE.
#' @param add_colour_incorrect Logical vector. Whether to highlight incorrect answers, by rank.
#'   Default is c(TRUE). Must be the same length as colour_incorrect.
#'
#' @details
#' The function supports two types of assessment items:
#'
#' 1. MCQ (Multiple Choice Questions): For these items, the function uses the value in
#'    `colname_correct_answer` to identify which option is correct (e.g., if CorrectResponse = 3,
#'    then MCQ_Option_3 is the correct answer). Incorrect answers are ranked by their values
#'    and colored according to the `colour_incorrect` vector.
#'
#' 2. VSAQ (Very Short Answer Questions): For these items, the function looks for columns
#'    named `prefix_vsaq + "Correct"` and `prefix_vsaq + "Incorrect"` and highlights them
#'    according to the `colour_correct` and `colour_incorrect` settings.
#'
#' The function automatically handles input validation, including checking for required columns
#' and handling mismatches between `colour_incorrect` and `add_colour_incorrect` lengths.
#'
#' @return A flextable object with highlighting applied based on the specified parameters.
#'
#' @examples
#' # Load required libraries
#' library(dplyr)
#' library(flextable)
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
#' # Basic usage - highlight only correct answers
#' ft1 <- fn_colour_responses(df)
#'
#' # Using vectors for incorrect colors - highlight top 3 incorrect answers
#' ft2 <- fn_colour_responses(
#'   data = df,
#'   colour_correct = "lightgreen",
#'   colour_incorrect = c("pink", "lightyellow", "lightblue"),
#'   add_colour_correct = TRUE,
#'   add_colour_incorrect = c(TRUE, TRUE, TRUE)
#' )
#'
#' # Many MCQ options example
#' many_options_df <- tibble(
#'   ItemCategory = c("MCQ", "MCQ"),
#'   CorrectResponse = c(7, 3),
#'   MCQ_Option_1 = c(0.1, 0.1),
#'   MCQ_Option_2 = c(0.2, 0.2),
#'   MCQ_Option_3 = c(0.3, 0.7),
#'   MCQ_Option_4 = c(0.1, 0.3),
#'   MCQ_Option_5 = c(0.05, 0.1),
#'   MCQ_Option_6 = c(0.15, 0.05),
#'   MCQ_Option_7 = c(0.7, 0.05),
#'   MCQ_Option_8 = c(0.05, 0.05)
#' )
#' ft3 <- fn_colour_responses(
#'   data = many_options_df,
#'   colour_incorrect = c("pink", "lightyellow"),
#'   add_colour_incorrect = c(TRUE, TRUE)
#' )
#'
#' # VSAQ-only dataset
#' vsaq_only_df <- tibble(
#'   ItemCategory = c("VSAQ", "VSAQ"),
#'   VSAQ_Correct = c(0.8, 0.9),
#'   VSAQ_Incorrect = c(0.2, 0.1)
#' )
#' ft4 <- fn_colour_responses(
#'   data = vsaq_only_df,
#'   colname_correct_answer = NULL  # No correct answer column needed for VSAQ only
#' )
#'
#' # Use with existing flextable
#' ft5 <- flextable(df) %>%
#'   bold(j = "ItemCategory") %>%
#'   fn_colour_responses(
#'     colour_correct = "lightblue",
#'     colour_incorrect = c("lightsalmon"),
#'     add_colour_incorrect = c(TRUE)
#'   )
#'
#' @export
fn_colour_responses <- function(data,
                              colname_correct_answer = "CorrectResponse",
                              colname_item_category = "ItemCategory",
                              prefix_mcq = "MCQ_Option_",
                              prefix_vsaq = "VSAQ_",
                              colour_correct = "lightgreen",
                              colour_incorrect = c("lightpink"),
                              add_colour_correct = TRUE,
                              add_colour_incorrect = c(TRUE)) {

  # Input validation
  if (length(colour_incorrect) != length(add_colour_incorrect)) {
    warning("Length mismatch: 'colour_incorrect' and 'add_colour_incorrect' have different lengths. ",
            "Using the shorter length of the two.")
    max_incorrect <- min(length(colour_incorrect), length(add_colour_incorrect))
    colour_incorrect <- colour_incorrect[1:max_incorrect]
    add_colour_incorrect <- add_colour_incorrect[1:max_incorrect]
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

  # Check if required columns exist
  if (!colname_item_category %in% colnames(data)) {
    stop(paste("Column", colname_item_category, "not found in data"))
  }

  # For MCQ items, check if correct answer column exists
  if ("MCQ" %in% data[[colname_item_category]] && !is.null(colname_correct_answer) &&
      !colname_correct_answer %in% colnames(data)) {
    stop(paste("Column", colname_correct_answer, "not found in data"))
  }

  # Get all column names with the prefixes
  mcq_cols <- grep(paste0("^", prefix_mcq), colnames(data), value = TRUE)
  if (length(mcq_cols) == 0 && "MCQ" %in% data[[colname_item_category]]) {
    warning(paste("No columns with prefix", prefix_mcq, "found in data, but MCQ items exist"))
  }

  # For VSAQ, only look for "Correct" and "Incorrect" without numbers
  vsaq_correct_col <- paste0(prefix_vsaq, "Correct")
  vsaq_incorrect_col <- paste0(prefix_vsaq, "Incorrect")

  if (("VSAQ" %in% data[[colname_item_category]]) &&
      !(vsaq_correct_col %in% colnames(data) || vsaq_incorrect_col %in% colnames(data))) {
    warning(paste("No columns named", vsaq_correct_col, "or", vsaq_incorrect_col,
                  "found in data, but VSAQ items exist"))
  }

  # Process each row
  for (i in 1:nrow(data)) {
    # For MCQ items
    if (data[[colname_item_category]][i] == "MCQ" && !is.null(colname_correct_answer) &&
        !is.na(data[[colname_correct_answer]][i])) {
      # Get correct answer column based on the correct response value
      correct_col <- paste0(prefix_mcq, data[[colname_correct_answer]][i])

      # Highlight correct answer if needed
      if (add_colour_correct && correct_col %in% colnames(data)) {
        colname_idx <- which(colnames(data) == correct_col)
        ft <- ft %>%
          bg(i = i, j = colname_idx, bg = colour_correct) %>%
          bold(i = i, j = colname_idx)
      }

      # Get all incorrect answer columns and their values
      incorrect_cols <- mcq_cols[mcq_cols != correct_col]
      if (length(incorrect_cols) > 0 && any(add_colour_incorrect)) {
        # Get values for all incorrect columns
        incorrect_values <- sapply(incorrect_cols, function(col) {
          val <- data[[col]][i]
          if (is.numeric(val)) return(val)
          as.numeric(val)
        })
        names(incorrect_values) <- incorrect_cols

        # Sort by value (descending)
        incorrect_sorted <- sort(incorrect_values, decreasing = TRUE)

        # Colour incorrect answers based on their rank
        for (rank in 1:min(length(incorrect_sorted), length(add_colour_incorrect))) {
          if (add_colour_incorrect[rank] && !is.na(incorrect_sorted[rank]) && incorrect_sorted[rank] > 0) {
            col <- names(incorrect_sorted)[rank]
            colname_idx <- which(colnames(data) == col)
            ft <- ft %>%
              bg(i = i, j = colname_idx, bg = colour_incorrect[rank])
          }
        }
      }
    }

    # For VSAQ items
    else if (data[[colname_item_category]][i] == "VSAQ") {
      # Handle correct answer for VSAQ (single column)
      if (add_colour_correct && vsaq_correct_col %in% colnames(data)) {
        if (!is.na(data[[vsaq_correct_col]][i]) && data[[vsaq_correct_col]][i] > 0) {
          colname_idx <- which(colnames(data) == vsaq_correct_col)
          ft <- ft %>%
            bg(i = i, j = colname_idx, bg = colour_correct) %>%
            bold(i = i, j = colname_idx)
        }
      }

      # Handle incorrect answer for VSAQ (single column)
      if (length(add_colour_incorrect) > 0 && add_colour_incorrect[1] &&
          vsaq_incorrect_col %in% colnames(data)) {
        if (!is.na(data[[vsaq_incorrect_col]][i]) && data[[vsaq_incorrect_col]][i] > 0) {
          colname_idx <- which(colnames(data) == vsaq_incorrect_col)
          ft <- ft %>%
            bg(i = i, j = colname_idx, bg = colour_incorrect[1])
        }
      }
    }
  }

  return(ft)
}
