#' Create score summary tables for IDS analysis
#'
#' @param lstOfDetails This variable should be a list containing as least the following defined variables: stage (stored as a numeric). This variable will be used to indentify the relevant students.
#' @param dfDemographics A dataframe containing student demographics
#' @param dfRes A dataframe containing the results for students who sat the exam
#' @param dfResAbsent A dataframe containing the results for students who did not sit the exam
#' @param programmes Which programmes to consider
#'
#' @return A table of the score summaries is returned
#' @export
#'
#' @examples fnIDSScoreSummary(cnst, dfRes, dfResAbsent, c("BDS"))

################################################################################

fnIDSScoreSummary <- function(lstOfDetails,
                              dfDemographics,
                              dfRes,
                              dfResAbsent,
                              programmes) {
  if (is.null(lstOfDetails) == TRUE |
      is.null(dfDemographics) == TRUE |
      is.null(dfRes) == TRUE |
      is.null(dfResAbsent) == TRUE |
      is.null(programmes) == TRUE) {
    stop("Please specify each of the required variables.")
  } else {
    # Get cohort size from original demographics file
    .fnNCohort = length(
      which(
        dfDemogOrig$Programme %in% programmes &
          dfDemogOrig$Stage == lstOfDetails$stage &
          dfDemogOrig$Year_Status != "Interrupted" &
          dfDemogOrig$Year_Status != "Withdrawn"
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
