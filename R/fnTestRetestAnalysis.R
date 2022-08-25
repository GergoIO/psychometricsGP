#' Test Retest Data Analysis
#'
#' @param lstOfDetails This variable should be a list containing as least the following defined variables: assessment (the current assessment number - eg: PT36) and assessmentPrev (the previous assessment number - eg: PT35)
#' @param dataAnalysis A list - the name of the variable storing all the test retest data analysis results
#' @param dataRaw A dataframe - all the raw test retest data. The following columns must be present:
#'
#' @return todo
#' @export
#'
#' @examples todo

################################################################################

# TODO Finish descriptions etc. test fcn

fnTestRetestAnalysis <-
  function(lstOfDetails,
           dataAnalysis,
           dataRaw) {
    if (is.null(lstOfDetails) == TRUE |
        is.null(dataAnalysis) == TRUE |
        is.null(dataRaw) == TRUE) {
      stop("One of the required variables for this function has not been specified.")
    } else{
      testScore <- glue('{lstOfDetails$assessment}_Score')
      testGrade <- glue('{lstOfDetails$assessment}_Grade')

      testScorePrev <- glue('{lstOfDetails$assessmentPrev}_Score')
      testGradePrev <- glue('{lstOfDetails$assessmentPrev}_Grade')

      # TRT Cov
      dataAnalysis[[glue('nTestRetestStage{i}')]] <-
        length(dataRaw$StudentID)
      .looptestRetestCor <-
        dataAnalysis[[glue('corrTestStage{i}')]] <-
        cor.test(dataRaw[[testScorePrev]], dataRaw[[testScore]])

      dataAnalysis[[glue('corrValStage{i}')]] <-
        .looptestRetestCor$estimate[[1]]
      dataAnalysis[[glue('corrLowCIStage{i}')]] <-
        .looptestRetestCor$conf.int[1]
      dataAnalysis[[glue('corrHighCIStage{i}')]] <-
        .looptestRetestCor$conf.int[2]

      # Matrix
      dataAnalysis[[glue('{testGradePrev}Stage{i}')]] <-
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
      dataAnalysis[[glue('{testGrade}Stage{i}')]] <-
        factor(
          .loopDfResTRT[[.loopTestGrade]],
          levels = c(
            "Unsatisfactory",
            "Borderline",
            "Satisfactory",
            "Excellent"
          ),
          ordered = TRUE
        )

      testRetestMatrix <-
        as.data.frame.matrix(table(dataAnalysis[[glue('{testGradePrev}Stage{i}')]], dataAnalysis[[glue('{testGrade}Stage{i}')]]))

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
      dataAnalysis[[glue('MatrixStage{i}')]] <- testRetestMatrix
      return(dataAnalysis)
    }
  }
