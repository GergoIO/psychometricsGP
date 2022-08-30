#' Test Retest Data Analysis
#'
#' @param stage A number or string (optional). If a stage is defined, the stage will be appended to the saved list items.  If a stage is not defined, no stage info will be appended.
#' @param lstOfDetails A list - containing as least the following defined variables: assessment (the current assessment number - eg: PT36) and assessmentPrev (the previous assessment number - eg: PT35)
#' @param dataAnalysis A list - the name of the variable storing all the test retest data analysis results. The output this function (fnTestRetestAnalysis) should be saved to the same dataAnalysis variable. It is also defined as an input variable here so that prior data saved to this list is not overwritten. This list must be defined (even as an empty list eg: testRetest <- list()) in the main script prior to calling this function.
#' @param dataRaw A dataframe - all the raw test retest data. When using this function for a specific stage, the input raw data must be for that stage only. No stage separation is performed as part of the function. The following columns must be present: "StudentID" and also the test scores and grades for the current assessement and the previous assessment in the form: "####_Score" and (eg for analysis of PT36: "PT35_Score", PT36_Score")
#'
#' @return A list is returned. The list contains all the test retest analysis. Analysis is specific to whatever data is input. Variable names will contain the given stage if it is specified
#' @export
#'
#' @examples testRetest <- fnTestRetestAnalysis(stage = 1, lstOfDetails = cnst, dataAnalysis = testRetest, dataRaw = dfResTestRetestStages[["Stage 1"]])

################################################################################

fnTestRetestAnalysis <-
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

      testScore <- glue('{assessment}_Score')
      testScorePrev <- glue('{assessmentPrev}_Score')

      # Test Retest Cov
      # Save number of students considered
      dataAnalysis[["nTestRetest"]] <-
        length(dataRaw$StudentID)
      # Perform correlation test
      .looptestRetestCor <-
        dataAnalysis[["corrTest"]] <-
        cor.test(dataRaw[[testScorePrev]], dataRaw[[testScore]])

      # Save correlation test results
      dataAnalysis[["corrVal"]] <-
        .looptestRetestCor$estimate[[1]]
      dataAnalysis[["corrLowCI"]] <-
        .looptestRetestCor$conf.int[1]
      dataAnalysis[["corrHighCI"]] <-
        .looptestRetestCor$conf.int[2]

      return(dataAnalysis)

    } else {
      message(glue("fnTestRetestAnalysis: A Stage ({stage}) is specified"))

      assessment <- lstOfDetails$assessment
      assessmentPrev <- lstOfDetails$assessmentPrev

      testScore <- glue('{assessment}_Score')
      testGrade <- glue('{assessment}_Grade')
      testScorePrev <- glue('{assessmentPrev}_Score')
      testGradePrev <- glue('{assessmentPrev}_Grade')

      # Test Retest Cov
      # Save number of students considered
      dataAnalysis[[glue('nTestRetestStage{stage}')]] <-
        length(dataRaw$StudentID)
      # Perform correlation test
      .looptestRetestCor <-
        dataAnalysis[[glue('corrTestStage{stage}')]] <-
        cor.test(dataRaw[[testScorePrev]], dataRaw[[testScore]])

      # Save correlation test results
      dataAnalysis[[glue('corrValStage{stage}')]] <-
        .looptestRetestCor$estimate[[1]]
      dataAnalysis[[glue('corrLowCIStage{stage}')]] <-
        .looptestRetestCor$conf.int[1]
      dataAnalysis[[glue('corrHighCIStage{stage}')]] <-
        .looptestRetestCor$conf.int[2]

      return(dataAnalysis)
    }
  }