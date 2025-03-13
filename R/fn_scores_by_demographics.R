#' Calculate demographic breakdowns of scores and pass rates
#'
#' @description
#' This function analyzes educational performance metrics broken down by demographic factors.
#' It calculates observed and adjusted mean scores, student counts, and passing rates for
#' each demographic category. The function also accounts for different educational stages
#' and generates statistics for each valid demographic factor.
#'
#' @param data A data frame containing the student data to analyze.
#' @param col_stage_current A character string specifying the column name that contains the stage information.
#' @param col_score A character string specifying the column name containing the numerical scores to analyze.
#' @param col_grade A character string specifying the column name containing the grade information.
#' @param passing_grades A vector of values that are considered passing grades.
#' @param cols_demographics A character vector specifying the column names of demographic factors to analyze.
#'
#' @return A tibble with the following columns:
#'   \item{StageCurrent}{The current educational stage}
#'   \item{Factor}{The demographic factor being analyzed}
#'   \item{Level}{The specific category/level within the demographic factor}
#'   \item{Count}{The number of students in each category}
#'   \item{ObservedMean}{The mean score observed for each category}
#'   \item{AdjustedMean}{The adjusted mean score calculated using linear models}
#'   \item{PassCount}{The number of students with passing grades in each category}
#'   \item{PassRate}{The percentage of students with passing grades in each category}
#'
#' @details
#' The function processes each demographic factor within each stage, excluding factors with fewer
#' than two distinct categories. It calculates both observed means (raw averages) and
#' adjusted means (using linear models via emmeans). The pass rate is calculated as the
#' percentage of students within each demographic category who achieved one of the specified
#' passing grades.
#'
#' @note
#' The function requires the dplyr, emmeans, and rlang packages.
#'
#' @examples
#' \dontrun{
#' # Example with a student dataset
#' results <- fn_scores_by_demographics(
#'   data = student_data,
#'   col_stage_current = "Stage",
#'   col_score = "FinalScore",
#'   col_grade = "LetterGrade",
#'   passing_grades = c("A", "B", "C"),
#'   cols_demographics = c("Gender", "Ethnicity", "SES")
#' )
#'
#' # View results
#' print(results)
#' }
#'
#' @importFrom dplyr filter group_by summarise mutate left_join bind_rows rename select n
#' @importFrom rlang sym !!
#' @importFrom stats as.formula lm
#' @importFrom emmeans emmeans
#'
#' @export
fn_scores_by_demographics <- function(data,
                                      col_stage_current,
                                      col_score,
                                      col_grade,
                                      passing_grades,
                                      cols_demographics) {
  results <- list()

  unique_stages <- unique(data[[col_stage_current]])

  for (stage_current in unique_stages) {
    data_stage_current <- data %>% filter(!!sym(col_stage_current) == stage_current)

    # Filter cols_demographics to only those with at least 2 factors
    valid_factors <- cols_demographics[sapply(data_stage_current[cols_demographics], function(col)
      length(unique(col)) > 1)]

    # Check if there are no valid factors and stop the function with an error message
    if (length(valid_factors) == 0) {
      stop("fn_scores_by_demographics - No valid factors available after filtering. Please check your columns.")
    }

    # Check if there are any factors that could not be included and display a message
    factors_not_included <- setdiff(cols_demographics, valid_factors)
    if (length(factors_not_included) > 0) {
      message("fn_scores_by_demographic - The following factors could not be included due to insufficient categories or non-existence in the dataset: ",
              paste(factors_not_included, collapse = ", "))
    }

    # Iterate over each demographic column
    for (col_factor in valid_factors) {

      # Calculate the observed mean score, count, and pass rate for each subset of the demographic
      mean_scores <- data_stage_current %>%
        group_by(!!sym(col_factor)) %>%
        summarise(
          ObservedMean = mean(!!sym(col_score), na.rm = TRUE),
          Count = n(),
          PassCount = sum(!!sym(col_grade) %in% passing_grades, na.rm = TRUE),
          PassRate = round(sum(!!sym(col_grade) %in% passing_grades, na.rm = TRUE) / n() * 100, 1)
        ) %>%
        mutate(Factor = col_factor,
               StageCurrent = stage_current,
               Level = as.character(!!sym(col_factor)))

      # Fit the linear model to calculate adjusted means
      lm_model <- lm(as.formula(paste(col_score, "~", col_factor)), data = data_stage_current)

      # Get adjusted means using emmeans
      adj_means <- as_tibble(emmeans(lm_model, specs = col_factor))

      # Merge observed means with adjusted means
      merged_data <- left_join(mean_scores, adj_means, by = c("Level" = col_factor))

      # Append the result for this factor
      results[[length(results) + 1]] <- merged_data
    }
  }

  # Bind all results into a single dataframe
  final_results <- bind_rows(results) %>%
    rename(AdjustedMean = emmean) %>%
    select(
      StageCurrent,
      Factor,
      Level,
      Count,
      ObservedMean,
      AdjustedMean,
      PassCount,
      PassRate
    )

  return(final_results)
}
