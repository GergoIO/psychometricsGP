#' Test Retest Dataframe Creation
#'
#' @param assessment A string - the current assessment being analysed (eg ADK38)
#' @param assessment_prev A string - the previous assessment (prior to the one being analysed - eg ADK37)
#' @param results A dataframe - containing results data for the current assessment with at least the following columns: "StudentID", "Stage", assessement score (of the form "assessement_Score" where assessment matches the variable defined in the "assessment" parameter) and assessment grade (of the form "assessment_Grade" where assessment matched the variable defined in the "assessment" parameter)
#' @param resultsPrevious A dataframe - containing results data for the previous assessment, in a similar fashion to the "results" dataframe
#'
#' @return A list of dataframes is returned. The results dataframe contains the student ID, stage and current and previous assessment grades and scores. The stagesResults list of dataframes contains the same data as the results dataframe with additional stage separation
#' @export
#'
#' @examples testRetest <- append(testRetest, fnTestRetest(assessment = cnst$assessment, assessment_prev = cnst$assessment_prev, results = dfRes, resultsPrevious = dfPrevTestData))

################################################################################
# Used in:
# AMK, ADK

fnTestRetest <-
  function(assessment = NULL,
           assessment_prev = NULL,
           results = NULL,
           resultsPrevious = NULL)
  {
    if (is.null(assessment) |
        is.null(assessment_prev) |
        is.null(results) |
        is.null(resultsPrevious)) {
      stop("One of the required variables for this function has not been specified.")
    } else{

      # Create list to save test retest data
      trtData <- list()

      #   ______________________________________________________________________
      #   trtData$results                                                ####

      # Merge previous and present assessment results (in that order, previous to the left)
      trtData$results <-
        merge(resultsPrevious,
              results,
              by = "StudentID",
              na.rm = TRUE)

      # The second column storing Stage becomes Stage.y and contains the present Stage of students
      # Use the Stage.y col going forwards
      # The Stage.x column contains the stage students were at the time of the previous assessment
      # Important to label this data with each students present Stage, especially in first test in year
      names(trtData$results)[names(trtData$results) == "Stage.y"] <-
        "Stage"

      #Remove any rows with NA for present or previous test score
      trtData$results <-
        trtData$results[!is.na(trtData$results[[glue('{assessment}_Score')]]),]
      trtData$results <-
        trtData$results[!is.na(trtData$results[[glue('{assessment_prev}_Score')]]),]

      # Only keep the required columns
      trtData$results <-
        trtData$results[, c(
          "StudentID",
          "Stage",
          glue('{assessment}_Score'),
          glue('{assessment}_Grade'),
          glue('{assessment_prev}_Score'),
          glue('{assessment_prev}_Grade')
        )]

      #   ______________________________________________________________________
      #   trtData$stagesResults                                          ####

      # Stage separate TRT data
      trtData$stagesResults <-
        split(trtData$results, trtData$results$Stage)
      names(trtData$stagesResults) <-
        glue("stage{sort(unique(trtData$results$Stage))}")
    }
    return(trtData)
  }
