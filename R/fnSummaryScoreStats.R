#' Summary Score Statistics Table
#'
#' @description
#' Create a summary score statistics table (including the count, mean, median, SD, Min, Max, Range and IQR) for all grades
#'
#'
#' @param data The results dataframe (must contain cols corresponding to those defined in the colScore and colGrade vars)
#' @param colScore String - the name of the column containing the scores
#' @param colGrade String - the name of the column containing the grades
#' @param grades Vector of strings - order of grades for arranging cols
#'
#' @return Returns a dataframe of the summary score statistics
#' @export
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(Grade = c("A", "A", "B", "B", "B", "C"),
#'                  Score = c(85, 92, 78, 85, 90, 72))
#'
#' # Calculate summary statistics for specific grades
#' summary_stats <-
#'   fnSummaryScoreStats(
#'     data = df,
#'     colScore = "Score",
#'     colGrade = "Grade",
#'     grades = c("A", "B", "C")
#'   )
#' print(summary_stats)

#'
################################################################################
#'
fnSummaryScoreStats <- function(data, colScore, colGrade, grades) {
  # Make sure the grade col is renamed to be 'Grade' for these calculations
  data <- data %>% rename(Grade = colGrade)

  rtn <- data %>%
    group_by(Grade) %>%
    # Summary for specific grades
    summarize(
      N = n(),
      Mean = mean(.data[[colScore]]),
      Median = median(.data[[colScore]]),
      StDev = sd(.data[[colScore]]),
      Min = min(.data[[colScore]]),
      Max = max(.data[[colScore]]),
      Range = max(.data[[colScore]]) - min(.data[[colScore]]),
      IQR = IQR(.data[[colScore]])
    ) %>%
    # Summary for all grades
    ungroup() %>%
    bind_rows(
      data.frame(
        Grade = "All",
        N = length(data[[colScore]]),
        Mean = mean(data[[colScore]]),
        Median = median(data[[colScore]]),
        StDev = sd(data[[colScore]]),
        Min = min(data[[colScore]]),
        Max = max(data[[colScore]]),
        Range = max(data[[colScore]]) - min(data[[colScore]]),
        IQR = IQR(data[[colScore]])
      )
    ) %>%
    # Rotate
    pivot_longer(cols = -Grade,
                 names_to = "Statistic",
                 values_to = "Value") %>%
    pivot_wider(names_from = Grade,
                values_from = Value) %>%
    # Create cols for grades that were not assigned
    add_column(!!!setNames(rep(NA, length(
      setdiff(grades, colnames(.))
    )), setdiff(grades, colnames(.)))) %>%
    select(c("Statistic", all_of(grades), "All"))
  # Return
  return(rtn)
}
