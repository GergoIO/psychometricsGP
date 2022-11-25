#' Test Retest Data Analysis
#'
#' @param stage A number or string (OPTIONAL). If a stage is defined, the stage will be appended to the saved list items.  If a stage is not defined, no stage info will be appended.
#' @param assessment A string - the current assessment being analysed (eg ADK38)
#' @param assessmentPrev A string - the previous assessment (prior to the one being analysed - eg ADK37)
#' @param dataRaw A dataframe - all the raw test retest data. When using this function for a specific stage, the input raw data must be for that stage only. No stage separation is performed as part of the function. The following columns must be present: "StudentID" and also the test scores and grades for the current assessement and the previous assessment in the form: "####_Score" and (eg for analysis of PT36: "PT35_Score", PT36_Score")
#'
#' @return A list is returned. The list contains all the test retest analysis. Analysis is specific to whatever data is input. Variable names will contain the given stage if it is specified. If saving this data to an existing list, use the append function (as shown in the example)
#' @export
#'
#' @examples testRetest <- append(testRetest, fnTestRetestAnalysis(stage = 1, assessment = cnst$assessment, assessmentPrev = cnst$assessmentPrev, dataRaw = dfResTestRetestStages[["Stage 1"]])

################################################################################

fnTestRetestAnalysis <-
  function(stage = NULL,
           assessment = NULL,
           assessmentPrev = NULL,
           dataRaw = NULL) {
    if (is.null(assessment) == TRUE |
        is.null(assessmentPrev) == TRUE |
        is.null(dataRaw) == TRUE) {
      stop("One of the required variables for this function has not been specified.")
    } else {
      # Create list to save test retest data
      trtData <- list()

      #   ______________________________________________________________________
      #   No Stage Defined                                                  ####
      if (is.null(stage) == TRUE) {
        message(
          "fnTestRetestAnalysis: No stage is specified. Continuing without stage specificity"
        )

        .testScore <- glue('{assessment}_Score')
        .testScorePrev <- glue('{assessmentPrev}_Score')

        # Test Retest Cov
        # Save number of students considered
        trtData[["nTestRetest"]] <-
          length(dataRaw$StudentID)
        # Perform correlation test
        .looptestRetestCor <-
          trtData[["corrTest"]] <-
          cor.test(dataRaw[[.testScorePrev]], dataRaw[[.testScore]])

        # Save correlation test results
        trtData[["corrVal"]] <-
          .looptestRetestCor$estimate[[1]]
        trtData[["corrLowCI"]] <-
          .looptestRetestCor$conf.int[1]
        trtData[["corrHighCI"]] <-
          .looptestRetestCor$conf.int[2]
      } else {
        #   ____________________________________________________________________
        #   Stage Defined                                                   ####
        message(glue("fnTestRetestAnalysis: A Stage ({stage}) is specified"))

        .testScore <- glue('{assessment}_Score')
        .testScorePrev <- glue('{assessmentPrev}_Score')

        # Test Retest Cov
        # Save number of students considered
        trtData[[glue('nTestRetestStage{stage}')]] <-
          length(dataRaw$StudentID)
        # Perform correlation test
        .looptestRetestCor <-
          trtData[[glue('corrTestStage{stage}')]] <-
          cor.test(dataRaw[[.testScorePrev]], dataRaw[[.testScore]])

        # Save correlation test results
        trtData[[glue('corrValStage{stage}')]] <-
          .looptestRetestCor$estimate[[1]]
        trtData[[glue('corrLowCIStage{stage}')]] <-
          .looptestRetestCor$conf.int[1]
        trtData[[glue('corrHighCIStage{stage}')]] <-
          .looptestRetestCor$conf.int[2]
      }
      return(trtData)
    }
  }
