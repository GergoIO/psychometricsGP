#' Test Retest Dataframe Creation
#'
#' @param assessment A string - the current assessment being analysed (eg ADK38)
#' @param assessmentPrev A string - the previous assessment (prior to the one being analysed - eg ADK37)
#' @param results A dataframe - containing results data for the current assessment with at least the following columns: "StudentID", "Stage", assessement score (of the form "assessement_Score" where assessment matches the variable defined in the "assessment" parameter) and assessment grade (of the form "assessment_Grade" where assessment matched the variable defined in the "assessment" parameter)
#' @param resultsPrevious A dataframe - containing results data for the previous assessment, in a similar fashion to the "results" dataframe
#'
#' @return A dataframe is returned. It contains the student ID, stage and current and previous assessment grades and scores.
#' @export
#'
#' @examples dfTestRetest <- fnTestRetest(assessment = cnst$assessment, assessmentPrev = cnst$assessmentPrev, results = dfRes, resultsPrevious = dfPrevTestData)

################################################################################
# Used in:
# AMK, ADK

fnTestRetest <-
  function(assessment = NULL,
           assessmentPrev = NULL,
           results = NULL,
           resultsPrevious = NULL)
  {
    if (is.null(assessment) == TRUE |
        is.null(assessmentPrev) == TRUE |
        is.null(results) == TRUE |
        is.null(resultsPrevious) == TRUE) {
      stop("One of the required variables for this function has not been specified.")
    } else{
      testRetest <- results
      # Merge previous and present assessment results (in that order, previous to the left)
      testRetest <-
        merge(resultsPrevious,
              testRetest,
              by = "StudentID",
              na.rm = TRUE)

      # The second column storing Stage becomes Stage.y and contains the present Stage of students
      # Use the Stage.y col going forwards
      # The Stage.x column contains the stage students were at the time of the previous assessment
      # Important to label this data with each students present Stage, especially in first test in year
      names(testRetest)[names(testRetest) == "Stage.y"] <-
        "Stage"

      #Remove any rows with NA for present or previous test score
      testRetest <-
        testRetest[!is.na(testRetest[[glue('{assessment}_Score')]]),]
      testRetest <-
        testRetest[!is.na(testRetest[[glue('{assessmentPrev}_Score')]]),]

      # Only keep the required columns
      testRetest <-
        testRetest[, c(
          "StudentID",
          "Stage",
          glue('{assessment}_Score'),
          glue('{assessment}_Grade'),
          glue('{assessmentPrev}_Score'),
          glue('{assessmentPrev}_Grade')
        )]
      return(testRetest)
    }
  }
