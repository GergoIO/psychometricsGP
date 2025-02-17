#' ANOVA Analysis by Demographics
#'
#' This function performs individual and combined ANOVA analyses for each demographic factor,
#' as well as a drop1 test for factor removal. The individual ANOVA is performed for each
#' demographic factor, and a combined ANOVA is run for all factors. Additionally, a drop1
#' test is applied to assess the effect of each factor when removed.
#'
#' @param data A data frame containing the dataset.
#' @param col_stage_current A string specifying the column name for the stages (e.g., "Stage").
#' @param col_score A string specifying the column name for the score variable (e.g., "Score").
#' @param cols_demographics A character vector of column names for the demographic variables (e.g., c("Gender", "AgeGroup")).
#' @param cols_anova A character vector of column names to test in the ANOVA (e.g., c("Gender", "AgeGroup", "Region")).
#'
#' @return A data frame with ANOVA results. The columns include:
#' \describe{
#'   \item{Factor}{The demographic factor being tested.}
#'   \item{StageCurrent}{The stage corresponding to the subset of the data.}
#'   \item{Method}{The method used for the analysis (e.g., "Individual", "All", "All - Drop1").}
#'   \item{data}{Degrees of freedom for the factor.}
#'   \item{sumsq}{Sum of squares for the factor.}
#'   \item{rss}{Residual sum of squares (meansq for the factor).}
#'   \item{AIC}{Akaike Information Criterion for the model.}
#'   \item{F.statistic}{F-statistic value for the ANOVA.}
#'   \item{p.value}{P-value for the factor in the ANOVA.}
#' }
#'
#' @import dplyr
#' @import broom
#'
#' @examples
#' # Example dataset
#' data <- data.frame(
#'   Stage = c("A", "A", "B", "B", "A", "B"),
#'   Score = c(85, 90, 88, 93, 87, 95),
#'   Gender = c("Male", "Female", "Male", "Female", "Female", "Male"),
#'   AgeGroup = c("Young", "Old", "Young", "Old", "Young", "Old"),
#'   Region = c("North", "South", "North", "South", "North", "South")
#' )
#'
#' # Example usage
#' anova_results <- fn_anova_analysis(
#'   data,
#'   col_stage_current = "Stage",
#'   col_score = "Score",
#'   cols_demographics = c("Gender", "AgeGroup"),
#'   cols_anova = c("Gender", "AgeGroup", "Region")
#' )
#' print(anova_results)
#'
#' @export
fn_anova_analysis <- function(data,
                           col_stage_current,
                           col_score,
                           cols_demographics,
                           cols_anova) {
  results <- list()

  unique_stages <- unique(data[[col_stage_current]])

  # Check if there are any factors in cols_anova that are not in cols_demographics
  factors_not_in_demographics <- setdiff(cols_anova, cols_demographics)
  if (length(factors_not_in_demographics) > 0) {
    stop("fn_anova_analysis - The following ANOVA factors are not in cols_demographics and will be ignored: ",
         paste(factors_not_in_demographics, collapse = ", "))
  }

  for (stage_current in unique_stages) {
    data_stage_current <- data %>% filter(.data[[col_stage_current]] == stage_current)

    # Filter cols_anova to only include those present in cols_demographics and having at least 2 factors
    valid_factors <- cols_anova[cols_anova %in% cols_demographics &
                                  sapply(data_stage_current[cols_anova], function(col)
                                    length(unique(col)) > 1)]

    # Check if there are no valid factors and stop the function with an error message
    if (length(valid_factors) == 0) {
      stop("fn_anova_analysis - No valid ANOVA factors available after filtering. Please check your columns.")
    }

    # Check if there are any factors that could not be included and display a message
    factors_not_included <- setdiff(cols_anova, valid_factors)
    if (length(factors_not_included) > 0) {
      message("fn_anova_analysis - The following ANOVA factors could not be included due to insufficient categories or non-existence in the dataset: ",
              paste(factors_not_included, collapse = ", "))
    }

    # Run individual ANOVA for each factor
    for (factor_col in valid_factors) {
      formula <- as.formula(paste(col_score, "~", factor_col))
      anova_result <- aov(formula, data = data_stage_current)
      tidy_result <- tidy(anova_result)

      # Add metadata to the results
      tidy_result <- tidy_result %>% mutate(Factor = factor_col,
                                            StageCurrent = stage_current,
                                            Method = "Individual")

      results[[length(results) + 1]] <- tidy_result
    }

    # Run a combined ANOVA with all valid factors
    if (length(valid_factors) > 0) {
      formula_all <- as.formula(paste(col_score, "~ ."))
      anova_all <- aov(formula_all, data = na.omit(data_stage_current[, c(col_score, valid_factors)]))
      tidy_all <- tidy(anova_all)

      # Add metadata for "All" method
      tidy_all <- tidy_all %>% mutate(Factor = term,
                                      StageCurrent = stage_current,
                                      Method = "All")

      results[[length(results) + 1]] <- tidy_all

      # Apply the drop1 test and capture results
      tabAov <- drop1(anova_all, test = "F")
      drop1_results <- tidy(tabAov)

      # Add metadata for "All - Drop1" method
      drop1_results <- drop1_results %>% mutate(Factor = term,
                                                StageCurrent = stage_current,
                                                Method = "All - Drop1")

      results[[length(results) + 1]] <- drop1_results
    }
  }

  # Bind all results into a single dataframe
  final_results <- bind_rows(results) %>%
    select(
      Factor,
      StageCurrent,
      Method,
      df,
      `Sum of Sq` = sumsq,
      RSS = meansq,
      AIC = statistic,
      `F-Statistic` = statistic,
      `P-Value` = p.value
    )

  return(final_results)
}
