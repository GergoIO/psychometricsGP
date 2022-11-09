#' Test Retest Dataframe Creation
#'
#' @param listOfDetails This variable should be a list containing as least the following defined variables: assessment (the current assessment number - eg: PT36) and assessmentPrev (the previous assessment number - eg: PT35)
#' @param results A dataframe - containing results data for the current assessment with at least the following columns: "StudentID", "Stage", assessement score (of the form "assessement_Score" where assessment matches the variable defined in listOfDetails) and assessment grade (of the form "assessment_Grade" where assessment grade matched the variable defined in listOfDetails)
#' @param resultsPrevious A dataframe - containing results data for the previous assessment, in a similar fashion to the "results" dataframe
#'
#' @return A dataframe is returned. It contains the student ID, stage and current and previous assessment grades and scores.
#' @export
#'
#' @examples dfTestRetest <- fnTestRetest(listOfDetails = cnst, results = dfRes, resultsPrevious = dfPrevTestData)

################################################################################
# Used in:
# AMK

fnTestRetest <-
  function(listOfDetails = NULL,
           results = NULL,
           resultsPrevious = NULL)
  {
    if (is.null(listOfDetails) == TRUE |
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
        testRetest[!is.na(testRetest[[glue('{listOfDetails$assessment}_Score')]]),]
      testRetest <-
        testRetest[!is.na(testRetest[[glue('{listOfDetails$assessmentPrev}_Score')]]),]

      # Only keep the required columns
      testRetest <-
        testRetest[, c(
          "StudentID",
          "Stage",
          glue('{listOfDetails$assessment}_Score'),
          glue('{listOfDetails$assessment}_Grade'),
          glue('{listOfDetails$assessmentPrev}_Score'),
          glue('{listOfDetails$assessmentPrev}_Grade')
        )]
      return(testRetest)
    }
  }
