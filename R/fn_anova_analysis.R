#' Perform ANOVA Analysis Across Multiple Stages
#'
#' This function performs ANOVA analysis on a dataset across different stages,
#' with filtering for group sizes and handling of missing data. It runs both
#' individual and combined ANOVA tests for specified demographic factors.
#'
#' @param data A data frame containing the dataset to analyze
#' @param col_stage_current Character string specifying the column name that
#'   contains the current stage information
#' @param col_score Character string specifying the column name that contains
#'   the dependent variable (score) for ANOVA analysis
#' @param cols_demographics Character vector of column names that represent
#'   all available demographic variables in the dataset
#' @param cols_anova Character vector of column names specifying which
#'   demographic factors to include in the ANOVA analysis. Must be a subset
#'   of cols_demographics
#' @param min_group_pct Numeric value specifying the minimum percentage of
#'   observations required in each group for a factor to be included in analysis.
#'   Default is 2 (representing 2%)
#' @param min_group_n Integer specifying the minimum number of observations
#'   required in each group for a factor to be included in analysis. Default is 5
#' @param remove_na_missing Logical indicating whether to remove rows with
#'   NA values or "Missing" entries in relevant columns. Default is TRUE
#'
#' @return A list containing two elements:
#' \describe{
#'   \item{anova_results}{A data frame with ANOVA results including Factor,
#'     StageCurrent, Method, degrees of freedom, Sum of Squares, RSS,
#'     F-Statistic, and P-Value}
#'   \item{exclusion_summary}{A data frame summarizing exclusions for each
#'     stage including original sample size, excluded missing data, and
#'     which demographics were included/excluded}
#' }
#'
#' @details
#' The function performs three types of ANOVA analysis:
#' \itemize{
#'   \item Individual: Separate one-way ANOVA for each valid factor
#'   \item All: Combined ANOVA using all valid factors simultaneously
#'   \item All - Drop1: Type III ANOVA using drop1() with F-test
#' }
#'
#' Factors are excluded from analysis if they don't meet the minimum group
#' size requirements (both absolute count and percentage) or if they have
#' only one unique value.
#'
#' @importFrom dplyr filter select mutate all_of bind_rows
#' @importFrom broom tidy
#' @importFrom stats aov drop1
#' @importFrom rlang .data
#'
#' @examples
#' # Create sample data
#' set.seed(123)
#' sample_data <- data.frame(
#'   stage = rep(c("Stage1", "Stage2"), each = 100),
#'   score = c(rnorm(100, 75, 10), rnorm(100, 80, 12)),
#'   gender = sample(c("Male", "Female"), 200, replace = TRUE),
#'   age_group = sample(c("Young", "Middle", "Old"), 200, replace = TRUE),
#'   education = sample(c("High School", "College", "Graduate"), 200, replace = TRUE),
#'   income = sample(c("Low", "Medium", "High"), 200, replace = TRUE)
#' )
#'
#' # Define demographic columns
#' demo_cols <- c("gender", "age_group", "education", "income")
#' anova_cols <- c("gender", "age_group", "education")
#'
#' # Run ANOVA analysis
#' results <- fn_anova_analysis(
#'   data = sample_data,
#'   col_stage_current = "stage",
#'   col_score = "score",
#'   cols_demographics = demo_cols,
#'   cols_anova = anova_cols,
#'   min_group_pct = 5,
#'   min_group_n = 10
#' )
#'
#' # View results
#' print(results$anova_results)
#' print(results$exclusion_summary)
#'
#' @export
fn_anova_analysis <- function(data,
                              col_stage_current,
                              col_score,
                              cols_demographics,
                              cols_anova,
                              min_group_pct = 2,
                              min_group_n = 5,
                              remove_na_missing = TRUE) {

  # Check factors exist in demographics
  missing_factors <- setdiff(cols_anova, cols_demographics)
  if (length(missing_factors) > 0) {
    stop("ANOVA factors not in demographics: ", paste(missing_factors, collapse = ", "))
  }

  results <- list()
  exclusion_summary <- list()
  min_prop <- min_group_pct / 100

  for (stage in unique(data[[col_stage_current]])) {
    stage_data <- data |> filter(.data[[col_stage_current]] == stage)
    original_n <- nrow(stage_data)

    # Remove missing data
    excluded_missing <- 0
    if (remove_na_missing) {
      relevant_cols <- c(col_score, cols_anova[cols_anova %in% names(stage_data)])
      stage_data <- stage_data |>
        filter(if_all(all_of(relevant_cols), ~ !is.na(.))) |>
        filter(if_all(all_of(relevant_cols), ~ . != "Missing"))
      excluded_missing <- original_n - nrow(stage_data)

      if (excluded_missing > 0) {
        message("Removed ", excluded_missing, " rows with NA/Missing values for stage '", stage, "'")
      }
    }

    # Find valid factors
    valid_factors <- c()
    excluded_factors <- c()
    for (factor in cols_anova) {
      if (!factor %in% names(stage_data)) next
      if (length(unique(stage_data[[factor]])) <= 1) next

      # Check group sizes
      group_counts <- table(stage_data[[factor]])
      group_props <- group_counts / nrow(stage_data)

      if (all(group_counts >= min_group_n & group_props >= min_prop)) {
        valid_factors <- c(valid_factors, factor)
      } else {
        excluded_factors <- c(excluded_factors, factor)
        small_groups <- names(group_counts[group_counts < min_group_n | group_props < min_prop])
        message("Excluding demographic '", factor, "' for Stage '", stage,
                "' - small groups: ", paste(small_groups, collapse = ", "))
      }
    }

    # Store exclusion info
    exclusion_summary[[length(exclusion_summary) + 1]] <- data.frame(
      stage = stage,
      original_n = original_n,
      excluded_missing = excluded_missing,
      demographics_included = paste(valid_factors, collapse = ", "),
      demographics_excluded = paste(setdiff(cols_anova, valid_factors), collapse = ", ")
    )

    if (length(valid_factors) == 0) {
      message("No valid ANOVA factors for Stage: ", stage, " - creating blank result rows")

      # Create blank result rows for each factor in cols_anova
      for (factor in cols_anova) {
        blank_result <- data.frame(
          Factor = factor,
          StageCurrent = stage,
          Method = NA,
          df = NA,
          sumsq = NA,
          meansq = NA,
          statistic = NA,
          p.value = NA
        )
        results[[length(results) + 1]] <- blank_result
      }
      next
    }

    # Run individual ANOVAs
    for (factor in valid_factors) {
      aov_result <- aov(as.formula(paste(col_score, "~", factor)), data = stage_data)
      tidy_result <- broom::tidy(aov_result) |>
        mutate(Factor = factor, StageCurrent = stage, Method = "Individual")
      results[[length(results) + 1]] <- tidy_result
    }

    # Run combined ANOVA
    if (length(valid_factors) > 1) {
      combined_data <- stage_data |>
        select(all_of(c(col_score, valid_factors))) |>
        na.omit()

      aov_all <- aov(as.formula(paste(col_score, "~ .")), data = combined_data)

      # All method
      tidy_all <- broom::tidy(aov_all) |>
        mutate(Factor = term, StageCurrent = stage, Method = "All")
      results[[length(results) + 1]] <- tidy_all

      # Drop1 method
      drop1_result <- broom::tidy(drop1(aov_all, test = "F")) |>
        mutate(Factor = term, StageCurrent = stage, Method = "All - Drop1")
      results[[length(results) + 1]] <- drop1_result
    }
  }

  # Combine results
  final_results <- bind_rows(results) |>
    select(Factor, StageCurrent, Method, df,
           `Sum of Sq` = sumsq, RSS = meansq,
           `F-Statistic` = statistic, `P-Value` = p.value)

  exclusion_df <- bind_rows(exclusion_summary)

  return(list(
    anova_results = final_results,
    exclusion_summary = exclusion_df
  ))
}
