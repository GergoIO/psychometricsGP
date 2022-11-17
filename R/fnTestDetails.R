#' Test Details Parameters (Stage Specific)
#'
#' @param stages A vector - denoting the stages to be considered. (stages = 2 or stages = c(2,3,4,5) etc.)
#' @param programme A string - the programme of the students (Must match what is in the "Programme" col in the demogData dataframe (see below) eg "BMBS" etc.)
#' @param demogData A dataframe - containing demographics data columns. Columns titled "Programme", "Stage" and "Year_Status" must be included. Make sure to use a df containing demographics data for all students and not one which has been filtered to include present students only
#' @param results A dataframe - containing results for students. A column titled "Stage" must be included. Each student should have one entry (row) in this dataframe. This will be used to determine the number of students assessed in each stage
#' @param resultsAbsent A dataframe - containing results for absent students. A column titled "Stage" must be included. Each absent student should have one entry (row) in this dataframe. This will be used to determine the number of absent students who were not assessed in each stage
#'
#' @return A dataframe is returned (testDetails) containing the calculated test details. A new column is generated for each stage considered
#' @export
#'
#' @examples fnTestDetails(stages = cnst$stages, programme = cnst$programme, demog = dfDemog, results = dfRes, resultsAbsent = dfResAbsent)

################################################################################

fnTestDetails <-
  function(stages = NULL,
           programme = NULL,
           demogData = NULL,
           results = NULL,
           resultsAbsent = NULL) {
    if (is.null(stages) == TRUE |
        is.null(programme) == TRUE |
        is.null(demogData) == TRUE |
        is.null(results) == TRUE | is.null(resultsAbsent) == TRUE) {
      stop("One of the required variables for this function has not been specified.")
    } else{
      testDetails <- data.frame(stringsAsFactors = FALSE)
      for (i in stages) {
        stage <- glue("Stage {i}")
        testDetails["Students in Stage", stage] <-
          length(
            which(
              demogData$Programme == programme &
                demogData$Stage == i &
                demogData$Year_Status %!in% c("Interrupted", "Withdrawn")
            )
          )
        testDetails["Students Assessed", stage] <-
          dim(results[results$Stage == i, ])[1]
        testDetails["Students Absent", stage] <-
          dim(resultsAbsent[resultsAbsent$Stage == i, ])[1]

        fnStatsScores <- function(x) {
          c(mean(x),
            median(x),
            sd(x),
            min(x),
            max(x),
            max(x) - min(x),
            IQR(x))
        }
        testDetails[c(
          "Mean Score (%)",
          "Median Score (%)",
          "Std. Dev",
          "Min Score (%)",
          "Max Score (%)",
          "Range",
          "IQR"
        ), stage] <-
          sapply(list(results[results$Stage == i, ]$pctScoreTotal), fnStatsScores)
      }
      return(testDetails)
    }
  }# END
