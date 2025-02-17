#' Scores by Demographics with Adjusted Means
#'
#' This function calculates the observed mean scores and adjusted mean scores
#' for each level of demographic factors, within each stage of a given dataset.
#' The observed mean is simply the average score for each group, while the
#' adjusted mean is calculated by fitting a linear model to account for the
#' influence of other factors.
#'
#' @param data A data frame containing the dataset.
#' @param col_stage_current A string specifying the column name for the stages (e.g., "Stage").
#' @param col_score A string specifying the column name for the score variable (e.g., "Score").
#' @param cols_demographics A character vector of column names for the demographic variables (e.g., c("Gender", "AgeGroup")).
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{StageCurrent}{The stage corresponding to the subset of the data.}
#'   \item{Factor}{The demographic variable being analyzed.}
#'   \item{Level}{The specific level within the demographic factor (e.g., "Male", "Female").}
#'   \item{Count}{The number of observations in each level of the factor.}
#'   \item{ObservedMean}{The observed mean score for each level of the demographic factor.}
#'   \item{AdjustedMean}{The adjusted mean score, accounting for other factors in the model.}
#' }
#'
#' @import dplyr
#' @import emmeans
#'
#' @examples
#' # Example dataset
#' data <- data.frame(
#'   Stage = c("A", "A", "B", "B", "A", "B"),
#'   Score = c(85, 90, 88, 93, 87, 95),
#'   Gender = c("Male", "Female", "Male", "Female", "Female", "Male"),
#'   AgeGroup = c("Young", "Old", "Young", "Old", "Young", "Old")
#' )
#'
#' # Example usage
#' results <- fn_scores_by_demographics(data, col_stage_current = "Stage", col_score = "Score", cols_demographics = c("Gender", "AgeGroup"))
#' print(results)
#'
#' @export
fn_scores_by_demographics <- function(data,
                                      col_stage_current,
                                      col_score,
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

      # Calculate the observed mean score and count for each subset of the demographic
      mean_scores <- data_stage_current %>%
        group_by(!!sym(col_factor)) %>%
        summarise(
          ObservedMean = mean(!!sym(col_score), na.rm = TRUE),
          Count = n()
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
      AdjustedMean
    )

  return(final_results)
}
