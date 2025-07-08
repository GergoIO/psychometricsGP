#' Perform Chi-Square Analysis of Demographics vs Pass/Fail Rates
#'
#' This function performs chi-square tests for each demographic factor against
#' pass/fail rates derived from Grade data. It handles minimum group size requirements
#' and can fallback to binary comparisons when needed.
#'
#' @param data A data frame containing the dataset with demographic variables,
#'   Grade, and ScorePct columns
#' @param col_stage_current Character string specifying the column name for stages
#' @param cols_demographics Character vector of demographic column names to analyze
#' @param min_subgroup_n Minimum number of students required in each Pass/Fail
#'   category for a subgroup to be included. Default is 5
#' @param verbose_stats Logical indicating whether to include detailed chi-square
#'   statistics or just p-values. Default is FALSE
#' @param include_ci Logical indicating whether to include confidence intervals
#'   for pass rates. Default is TRUE
#' @param overall_test Logical indicating whether to perform overall chi-square
#'   test for each demographic. Default is TRUE
#' @param use_binary_fallback Logical indicating whether to use binary comparison
#'   (most common vs others) when original analysis fails due to small groups.
#'   Default is TRUE
#' @param binary_on_any_exclusion Logical indicating whether to use binary fallback
#'   if ANY subgroup needs to be excluded (not just when analysis completely fails).
#'   Default is FALSE
#' @param entry_override Character string specifying a demographic name that should
#'   use "Foundation" vs "Not Foundation" comparison instead of other approaches.
#'   Default is NULL
#'
#' @return A list containing:
#' \describe{
#'   \item{results_tables}{List of tibbles, one per demographic, with subgroup
#'     statistics including count, observed mean score, pass rate, and confidence intervals}
#'   \item{exclusion_summary}{Tibble summarizing exclusions per demographic}
#'   \item{overall_tests}{Tibble with overall chi-square test results per demographic}
#' }
#'
#' @importFrom dplyr filter select mutate group_by summarise ungroup arrange
#' @importFrom dplyr left_join bind_rows case_when n
#' @importFrom stats chisq.test prop.test
#' @importFrom binom binom.wilson
#'
#' @examples
#' # Assuming your data structure
#' results <- fn_chisq_analysis(
#'   data = demographics_sitting |>
#'     filter(StageCurrent %in% demographics$anovas$stages) |>
#'     left_join(universal_students |>
#'               select(StudentID, Grade, ScorePct), by = "StudentID") |>
#'     filter(!is.na(Grade)),
#'   col_stage_current = "StageCurrent",
#'   cols_demographics = demographics$anovas$included,
#'   min_subgroup_n = 5,
#'   verbose_stats = FALSE,
#'   include_ci = TRUE,
#'   overall_test = TRUE,
#'   use_binary_fallback = TRUE,
#'   binary_on_any_exclusion = FALSE,
#'   entry_override = NULL
#' )
#'
#' @export
fn_chisq_analysis <- function(data,
                                           col_stage_current,
                                           cols_demographics,
                                           min_subgroup_n = 5,
                                           verbose_stats = TRUE,
                                           include_ci = TRUE,
                                           overall_test = TRUE,
                                           use_binary_fallback = TRUE,
                                           binary_on_any_exclusion = TRUE,
                                           entry_override = NULL) {

  # Check required columns exist
  required_cols <- c("Grade", "ScorePct")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check demographic columns exist
  missing_demographics <- setdiff(cols_demographics, names(data))
  if (length(missing_demographics) > 0) {
    stop("Missing demographic columns: ", paste(missing_demographics, collapse = ", "))
  }

  # Create Pass/Fail variable
  data <- data |>
    mutate(
      PassFail = case_when(
        Grade %in% c("Unsatisfactory", "Fail") ~ "Fail",
        Grade %in% c("Pass", "Satisfactory", "Borderline", "Excellent") ~ "Pass",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(PassFail))

  # Initialize results storage
  results_tables <- list()
  exclusion_summary <- list()
  overall_tests <- list()

  # Process each demographic
  for (demographic in cols_demographics) {
    # Skip if column doesn't exist or has no variation
    if (!demographic %in% names(data)) {
      exclusion_summary[[demographic]] <- tibble(
        Demographic = demographic,
        Stage = "All",
        Reason = "Column not found",
        Subgroups_Excluded = NA_character_,
        Students_Affected = 0
      )
      next
    }

    # Get clean data for this demographic
    demo_data <- data |>
      filter(!is.na(.data[[demographic]]), !is.na(ScorePct)) |>
      select(all_of(c(col_stage_current, demographic, "PassFail", "ScorePct")))

    if (nrow(demo_data) == 0) {
      exclusion_summary[[demographic]] <- tibble(
        Demographic = demographic,
        Stage = "All",
        Reason = "No valid data",
        Subgroups_Excluded = NA_character_,
        Students_Affected = 0
      )
      next
    }

    # Process each stage
    for (stage in unique(demo_data[[col_stage_current]])) {
      stage_data <- demo_data |> filter(.data[[col_stage_current]] == stage)

      # Calculate subgroup statistics
      subgroup_stats <- stage_data |>
        group_by(.data[[demographic]]) |>
        summarise(
          Count = n(),
          Pass_Count = sum(PassFail == "Pass"),
          Fail_Count = sum(PassFail == "Fail"),
          Observed_Mean = mean(ScorePct, na.rm = TRUE),
          Pass_Rate = Pass_Count / Count,
          .groups = "drop"
        ) |>
        arrange(desc(Observed_Mean))

      # Check which subgroups meet minimum requirements
      valid_subgroups <- subgroup_stats |>
        filter(Pass_Count >= min_subgroup_n & Fail_Count >= min_subgroup_n)

      excluded_subgroups <- subgroup_stats |>
        filter(Pass_Count < min_subgroup_n | Fail_Count < min_subgroup_n)

      # Store exclusion information
      if (nrow(excluded_subgroups) > 0) {
        exclusion_summary[[paste0(demographic, "_", stage)]] <- tibble(
          Demographic = demographic,
          Stage = stage,
          Reason = "Insufficient count in Pass/Fail categories",
          Subgroups_Excluded = paste(excluded_subgroups[[demographic]], collapse = ", "),
          Students_Affected = sum(excluded_subgroups$Count)
        )
      }

      # Determine analysis approach
      analysis_data <- NULL
      analysis_type <- "original"

      # Check if we should use Entry override
      if (!is.null(entry_override) && demographic == entry_override) {
        # Use Foundation vs Not Foundation
        analysis_data <- stage_data |>
          mutate(
            Binary_Group = ifelse(.data[[demographic]] == "Foundation",
                                  "Foundation", "Not Foundation")
          )
        analysis_type <- "entry_override"

        # Recompute stats for Foundation vs Not Foundation
        foundation_stats <- analysis_data |>
          group_by(Binary_Group) |>
          summarise(
            Count = n(),
            Pass_Count = sum(PassFail == "Pass"),
            Fail_Count = sum(PassFail == "Fail"),
            Observed_Mean = mean(ScorePct, na.rm = TRUE),
            Pass_Rate = Pass_Count / Count,
            .groups = "drop"
          ) |>
          arrange(desc(Observed_Mean))

        # Check if Foundation approach meets requirements
        if (all(foundation_stats$Pass_Count >= min_subgroup_n &
                foundation_stats$Fail_Count >= min_subgroup_n)) {
          valid_subgroups <- foundation_stats |>
            rename(!!demographic := Binary_Group)
        } else {
          analysis_data <- NULL
        }

      } else if (nrow(valid_subgroups) >= 2 &&
                 (!binary_on_any_exclusion || nrow(excluded_subgroups) == 0)) {
        # Use original subgroups
        analysis_data <- stage_data |>
          filter(.data[[demographic]] %in% valid_subgroups[[demographic]])
        analysis_type <- "original"

      } else if (use_binary_fallback && nrow(subgroup_stats) >= 2) {
        # Use binary fallback (most common vs others)
        most_common <- subgroup_stats |>
          slice_max(Count, n = 1) |>
          pull(.data[[demographic]])

        analysis_data <- stage_data |>
          mutate(
            Binary_Group = ifelse(.data[[demographic]] == most_common,
                                  most_common, "Other")
          )
        analysis_type <- "binary"

        # Recompute stats for binary groups
        binary_stats <- analysis_data |>
          group_by(Binary_Group) |>
          summarise(
            Count = n(),
            Pass_Count = sum(PassFail == "Pass"),
            Fail_Count = sum(PassFail == "Fail"),
            Observed_Mean = mean(ScorePct, na.rm = TRUE),
            Pass_Rate = Pass_Count / Count,
            .groups = "drop"
          ) |>
          arrange(desc(Observed_Mean))

        # Check if binary approach meets requirements
        if (all(binary_stats$Pass_Count >= min_subgroup_n &
                binary_stats$Fail_Count >= min_subgroup_n)) {
          valid_subgroups <- binary_stats |>
            rename(!!demographic := Binary_Group)
        } else {
          analysis_data <- NULL
        }
      }

      # Perform analysis if we have valid data
      if (!is.null(analysis_data) && nrow(valid_subgroups) >= 2) {

        # Add confidence intervals if requested
        if (include_ci) {
          ci_results <- valid_subgroups |>
            rowwise() |>
            mutate(
              CI_Lower = binom.confint(Pass_Count, Count, methods = "wilson")$lower,
              CI_Upper = binom.confint(Pass_Count, Count, methods = "wilson")$upper
            ) |>
            ungroup()
          valid_subgroups <- ci_results
        }

        # Perform chi-square test if requested
        if (overall_test) {
          if (analysis_type == "original") {
            contingency_table <- table(analysis_data[[demographic]],
                                       analysis_data$PassFail)
          } else if (analysis_type == "entry_override") {
            contingency_table <- table(analysis_data$Binary_Group,
                                       analysis_data$PassFail)
          } else {
            contingency_table <- table(analysis_data$Binary_Group,
                                       analysis_data$PassFail)
          }

          if (min(contingency_table) >= min_subgroup_n) {
            chisq_result <- stats::chisq.test(contingency_table)

            test_summary <- tibble(
              Demographic = demographic,
              Stage = stage,
              Analysis_Type = analysis_type,
              Chi_Square = chisq_result$statistic,
              DF = chisq_result$parameter,
              P_Value = chisq_result$p.value
            )

            if (!verbose_stats) {
              test_summary <- test_summary |>
                select(Demographic, Stage, Analysis_Type, P_Value)
            }

            overall_tests[[paste0(demographic, "_", stage)]] <- test_summary
          }
        }

        # Store results table
        results_table <- valid_subgroups |>
          select(
            !!demographic,
            Count,
            Observed_Mean,
            Pass_Rate,
            dplyr::any_of(c("CI_Lower", "CI_Upper"))
          ) |>
          mutate(
            Demographic = demographic,
            Stage = stage,
            Analysis_Type = analysis_type,
            .before = 1
          )

        results_tables[[paste0(demographic, "_", stage)]] <- results_table

      } else {
        # No valid analysis possible
        exclusion_summary[[paste0(demographic, "_", stage, "_failed")]] <- tibble(
          Demographic = demographic,
          Stage = stage,
          Reason = "Insufficient valid subgroups for analysis",
          Subgroups_Excluded = "All",
          Students_Affected = nrow(stage_data)
        )
      }
    }
  }

  # Combine results
  final_results <- list(
    results_tables = if(length(results_tables) > 0) results_tables else list(),
    exclusion_summary = if(length(exclusion_summary) > 0) bind_rows(exclusion_summary) else tibble(),
    overall_tests = if(length(overall_tests) > 0) bind_rows(overall_tests) else tibble()
  )

  return(final_results)
}
#
#
#
# fn_chisq_analysis(
#   data = demographics_sitting |>
#     filter(StageCurrent %in% demographics$anovas$stages) |>
#     left_join(universal_students |>
#                 select(StudentID, Grade, ScorePct), by = "StudentID") |>
#     filter(!is.na(Grade)),
#   col_stage_current = "StageCurrent",
#   cols_demographics = demographics$anovas$available,
#   entry_override = "Entry"
# )
