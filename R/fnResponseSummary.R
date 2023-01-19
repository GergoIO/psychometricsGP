#' Response Summary Table
#'
#' @param stages A vector - denoting the stages to be considered. (stages = 2 or stages = c(2,3,4,5) etc.)
#' @param results A dataframe - containing results for students. Columns titled "Stage", "nCorr", "nIncorr", "nDontKnow", "pctCorr", "pctIncorr" and "pctDontKnow" must be included. These columns should correspond to the stage, number correct, number incorrect, number don't know, percentage correct, percentage incorrect, percentage don't know answers respectively. Each student should have one entry (row) in this dataframe. This will be used to determine the number of students assessed in each stage
#'
#' @return A dataframe is returned containing the calculated response summaries A new column is generated for each stage considered
#' @export
#'
#' @examples fnResponseSummary(stages = cnst$stages, results = dfResults)
#'
################################################################################
#'
fnResponseSummary <- function(stages = NULL,
                              results = NULL) {
  if (is.null(stages) |
      is.null(results)) {
    stop("One of the required variables for this function has not been specified.")
  } else{
    responseSummary <- data.frame(stringsAsFactors = FALSE)

    for (i in stages) {
      .stageCol <- glue("Stage {i}")

      responseSummary["Number Correct", .stageCol] <-
        mean(results$nCorr[results$Stage == i])
      responseSummary["Number Incorrect", .stageCol] <-
        mean(results$nIncorr[results$Stage == i])
      responseSummary["Number Don't Know", .stageCol] <-
        mean(results$nDontKnow[results$Stage == i])
      responseSummary["Percentage Correct (%)", .stageCol] <-
        mean(results$pctCorr[results$Stage == i])
      responseSummary["Percentage Incorrect (%)", .stageCol] <-
        mean(results$pctIncorr[results$Stage == i])
      responseSummary["Percentage Don't Know (%)", .stageCol] <-
        mean(results$pctDontKnow[results$Stage == i])
    }
    return(responseSummary)
  }
}
