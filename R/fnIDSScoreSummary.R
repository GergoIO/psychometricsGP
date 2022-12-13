#' Create score summary tables for IDS analysis
#'
#' @param stage The stage of students (numeric) This variable will be used to indentify the relevant students.
#' @param dfDemographics A dataframe containing student demographics
#' @param dfRes A dataframe containing the results for students who sat the exam
#' @param dfResAbsent A dataframe containing the results for students who did not sit the exam
#' @param programmes Which programmes to consider
#'
#' @return A table of the score summaries is returned
#' @export
#'
#' @examples fnIDSScoreSummary(2, dfRes, dfResAbsent, c("BDS"))

################################################################################

fnIDSScoreSummary <- function(stage,
                              dfDemographics,
                              dfRes,
                              dfResAbsent,
                              programmes) {
  if (is.null(stage) |
      is.null(dfDemographics) |
      is.null(dfRes) |
      is.null(dfResAbsent) |
      is.null(programmes)) {
    stop("Please specify each of the required variables.")
  } else {
    # Get cohort size from original demographics file
    .fnNCohort = length(
      which(
        dfDemographics$Programme %in% programmes &
          dfDemographics$Stage == stage &
          dfDemographics$Year_Status != "Interrupted" &
          dfDemographics$Year_Status != "Withdrawn"
      )
    )
    # Get the col of pct scores for the requested programmes
    .fnPctScores = dfRes$pctScoreTotal[dfRes$Programme %in% programmes]
    # Calculate properties of the requested programmes
    .df <- as.data.frame(
      c(
        "Number in cohort" = format(.fnNCohort),
        "Number assessed" = format(dim(dfRes[dfRes$Programme %in% programmes, ])[1]),
        "Number absent" = format(dim(dfResAbsent[dfResAbsent$Programme %in% programmes, ])[1]),
        "Mean" = fnRound(mean(.fnPctScores), 2),
        "Median" = fnRound(median(.fnPctScores), 2),
        "SD" = fnRound(sd(.fnPctScores), 2),
        "Min" = fnRound(min(.fnPctScores), 2),
        "Max" = fnRound(max(.fnPctScores), 2),
        "Range" = fnRound(max(.fnPctScores) - min(.fnPctScores), 2),
        "IQR" = fnRound(IQR(.fnPctScores), 2)
      )
    )
    colnames(.df) <- c("Value")
    tibble::rownames_to_column(as.data.frame(.df), "Statistic")
  }
} # END
