#' Test Retest Data Matrix Creation
#'
#' @param stage A number or string (OPTIONAL). If a stage is defined, the stage will be appended to the saved list items.  If a stage is not defined, no stage info will be appended.
#' @param assessment A string - the current assessment being analysed (eg ADK38)
#' @param assessmentPrev A string - the previous assessment (prior to the one being analysed - eg ADK37)
#' @param dataRaw A dataframe - all the raw test retest data. When using this function for a specific stage, the input raw data must be for that stage only. No stage separation is performed as part of the function. The following columns must be present: "StudentID" and also the test grades for the current assessement and the previous assessment in the form: "####_Grade" (eg for analysis of PT36: "PT35_Grade", "PT36_Grade")
#'
#' @return A list is returned. The list contains all the test retest analysis. Analysis is specific to whatever data is input. Variable names will contain the given stage if it is specified. If saving this data to an existing list, use the append function (as shown in the example)
#' @export
#'
#' @examples testRetest <- append(testRetest, fnTestRetestMatrix(stage = 1, assessment = cnst$assessment, assessmentPrev = cnst$assessmentPrev, dataRaw = testRetest$resultsStages[["Stage 1"]]))

################################################################################
# Used in:
# AMK, ADK

fnTestRetestMatrix <-
  function(stage = NULL,
           assessment = NULL,
           assessmentPrev = NULL,
           dataRaw = NULL) {
    if (is.null(assessment) |
        is.null(assessmentPrev) |
        is.null(dataRaw)) {
      stop("One of the required variables for this function has not been specified.")
    } else{
      # Create list to save test retest data
      trtData <- list()

      #   ______________________________________________________________________
      #   No Stage Defined                                                  ####
      if (is.null(stage)) {
        message(
          "fnTestRetestMatrix: No stage is specified. Continuing without stage specificity"
        )
        testGrade <- glue('{assessment}_Grade')
        testGradePrev <- glue('{assessmentPrev}_Grade')

        # Construct Test Retest Grade Matrix
        trtData[["testGradePrev"]] <-
          factor(
            dataRaw[[testGradePrev]],
            levels = c(
              "Unsatisfactory",
              "Borderline",
              "Satisfactory",
              "Excellent"
            ),
            ordered = TRUE
          )
        trtData[["testGrade"]] <-
          factor(
            dataRaw[[testGrade]],
            levels = c(
              "Unsatisfactory",
              "Borderline",
              "Satisfactory",
              "Excellent"
            ),
            ordered = TRUE
          )

        testRetestMatrix <-
          as.data.frame.matrix(table(trtData[["testGradePrev"]], trtData[["testGrade"]]))
        rownames(testRetestMatrix) <- NULL

        testRetestMatrix <-
          cbind(
            c(
              "Unsatisfactory",
              "Borderline",
              "Satisfactory",
              "Excellent"
            ),
            rep(assessmentPrev, 4),
            testRetestMatrix
          )
        testRetestMatrix <-
          rbind(c("Grade ↓",
                  "Assessment ↓→",
                  rep(assessment, 4)),
                testRetestMatrix)
        colnames(testRetestMatrix) <-
          c(" ",
            "Grade →",
            "Unsatisfactory",
            "Borderline",
            "Satisfactory",
            "Excellent")
        trtData[["Matrix"]] <- testRetestMatrix
      } else {

        #   ____________________________________________________________________
        #   Stage Defined                                                   ####

        message(glue("fnTestRetestMatrix: A Stage ({stage}) is specified"))

        testGrade <- glue('{assessment}_Grade')
        testGradePrev <- glue('{assessmentPrev}_Grade')

        # Construct Test Retest Grade Matrix
        trtData[[glue('{testGradePrev}Stage{stage}')]] <-
          factor(
            dataRaw[[testGradePrev]],
            levels = c(
              "Unsatisfactory",
              "Borderline",
              "Satisfactory",
              "Excellent"
            ),
            ordered = TRUE
          )
        trtData[[glue('{testGrade}Stage{stage}')]] <-
          factor(
            dataRaw[[testGrade]],
            levels = c(
              "Unsatisfactory",
              "Borderline",
              "Satisfactory",
              "Excellent"
            ),
            ordered = TRUE
          )

        testRetestMatrix <-
          as.data.frame.matrix(table(trtData[[glue('{testGradePrev}Stage{stage}')]], trtData[[glue('{testGrade}Stage{stage}')]]))
        rownames(testRetestMatrix) <- NULL

        testRetestMatrix <-
          cbind(
            c(
              "Unsatisfactory",
              "Borderline",
              "Satisfactory",
              "Excellent"
            ),
            rep(assessmentPrev, 4),
            testRetestMatrix
          )
        testRetestMatrix <-
          rbind(c("Grade ↓",
                  "Assessment ↓→",
                  rep(assessment, 4)),
                testRetestMatrix)
        colnames(testRetestMatrix) <-
          c(" ",
            "Grade →",
            "Unsatisfactory",
            "Borderline",
            "Satisfactory",
            "Excellent")
        trtData[[glue('matrixStage{stage}')]] <-
          testRetestMatrix
      }
      return(trtData)
    }
  }
