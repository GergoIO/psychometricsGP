#' Test Retest Dataframe Creation
#'
#' @param lstOfDetails This variable should be a list containing as least the following defined variables: assessment (the current assessment number - eg: PT36) and assessmentPrev (the previous assessment number - eg: PT35)
#' @param results A dataframe - containing results data for the current assessment with at least the following columns: "StudentID", "Stage", assessement score (of the form "assessement_Score" where assessment matches the variable defined in lstOfDetails) and assessment grade (of the form "assessment_Grade" where assessment grade matched the variable defined in lstOfDetails)
#' @param resultsPrevious A dataframe - containing results data for the previous assessment, in a similar fashion to the "results" dataframe
#'
#' @return A dataframe is returned. It contains the student ID, stage and current and previous assessment grades and scores.
#' @export
#'
#' @examples dfTestRetest <- fnTestRetest(lstOfDetails = cnst, results = dfRes, resultsPrevious = dfPrevTestData)

################################################################################

fnTestRetest <-
  function(lstOfDetails = NULL,
           results = NULL,
           resultsPrevious = NULL)
  {
    if (is.null(lstOfDetails) == TRUE |
        is.null(results) == TRUE |
        is.null(resultsPrevious) == TRUE) {
      stop("One of the required variables for this function has not been specified.")
    } else{
      testRetest <- results
      testRetest <-
        merge(resultsPrevious,
              testRetest,
              by = "StudentID",
              na.rm = TRUE)
      names(testRetest)[names(testRetest) == "Stage.x"] <-
        "Stage"
      #Remove any rows with NA for present of previous test score
      testRetest <-
        testRetest[!is.na(testRetest[[glue('{lstOfDetails$assessment}_Score')]]),]
      testRetest <-
        testRetest[!is.na(testRetest[[glue('{lstOfDetails$assessmentPrev}_Score')]]),]

      testRetest <-
        testRetest[, c(
          "StudentID",
          "Stage",
          glue('{lstOfDetails$assessment}_Score'),
          glue('{lstOfDetails$assessment}_Grade'),
          glue('{lstOfDetails$assessmentPrev}_Score'),
          glue('{lstOfDetails$assessmentPrev}_Grade')
        )]
      return(testRetest)
    }
  }
