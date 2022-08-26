#' Test Retest Data Matrix Creation
#'
#' @param stage A number or string (optional). If a stage is defined, the stage will be appended to the saved list items.  If a stage is not defined, no stage info will be appended.
#' @param lstOfDetails A list - containing as least the following defined variables: assessment (the current assessment number - eg: PT36) and assessmentPrev (the previous assessment number - eg: PT35)
#' @param dataAnalysis A list - the name of the variable storing all the test retest data analysis results. The output this function (fnTestRetestMatrix) should be the saved to same dataAnalysis variable. It is also defined as an input variable here so that prior data saved to this list is not overwritten. This list must be defined (even as an empty list eg: testRetest <- list()) in the main script prior to calling this function.
#' @param dataRaw A dataframe - all the raw test retest data. When using this function for a specific stage, the input raw data must be for that stage only. No stage separation is performed as part of the function. The following columns must be present: "StudentID" and also the test grades for the current assessement and the previous assessment in the form: "####_Grade" (eg for analysis of PT36: "PT35_Grade", "PT36_Grade")
#'
#' @return A list is returned. The list contains all the test retest analysis. Analysis is specific to whatever data is input. Variable names will contain the given stage if it is specified
#' @export
#'
#' @examples testRetest <- fnTestRetestMatrix(stage = 1, lstOfDetails = cnst, dataAnalysis = testRetest, dataRaw = dfResTestRetestStages[["Stage 1"]])

################################################################################

fnTestRetestMatrix <-
  function(stage = NULL,
           lstOfDetails = NULL,
           dataAnalysis = NULL,
           dataRaw = NULL) {
    if (is.null(lstOfDetails) == TRUE |
        is.null(dataAnalysis) == TRUE |
        is.null(dataRaw) == TRUE) {
      stop("One of the required variables for this function has not been specified.")
    }
    if (is.null(stage) == TRUE) {
      message(
        "fnTestRetestAnalysis: No stage is specified. Continuing without stage specificity"
      )

      assessment <- lstOfDetails$assessment
      assessmentPrev <- lstOfDetails$assessmentPrev

      testGrade <- glue('{assessment}_Grade')
      testGradePrev <- glue('{assessmentPrev}_Grade')

      # Construct Test Retest Grade Matrix
      dataAnalysis[["testGradePrev"]] <-
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
      dataAnalysis[["testGrade"]] <-
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
        as.data.frame.matrix(table(dataAnalysis[["testGradePrev"]], dataAnalysis[["testGrade"]]))
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
      dataAnalysis[["Matrix"]] <- testRetestMatrix
      return(dataAnalysis)

    } else {
      message(glue("fnTestRetestAnalysis: A Stage ({stage}) is specified"))

      assessment <- lstOfDetails$assessment
      assessmentPrev <- lstOfDetails$assessmentPrev

      testGrade <- glue('{assessment}_Grade')
      testGradePrev <- glue('{assessmentPrev}_Grade')

      # Construct Test Retest Grade Matrix
      dataAnalysis[[glue('{testGradePrev}Stage{stage}')]] <-
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
      dataAnalysis[[glue('{testGrade}Stage{stage}')]] <-
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
        as.data.frame.matrix(table(dataAnalysis[[glue('{testGradePrev}Stage{stage}')]], dataAnalysis[[glue('{testGrade}Stage{stage}')]]))
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
      dataAnalysis[[glue('MatrixStage{stage}')]] <- testRetestMatrix
      return(dataAnalysis)
    }
  }
