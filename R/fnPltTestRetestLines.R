#' Add Grade Boundary Lines to Test Retest Plots
#'
#' @param plot A plot - a scatterplot of the stage specific test retest data
#' @param stage Numeric (integer) - the current stage being plotted
#' @param testInYear Numeric (integer) - the test in the academic year of this assessment. 1 for the first assessment
#' @param gradeBounds A dataframe of the grade boundaries for the current assessment. Cols should be named "Stage 1" etc. Rows should be named "Unsatisfactory", "Borderline", "Satisfactory", "Excellent"
#' @param gradeBoundsPrev A dataframe of the grade boundaries for the previous assessment. Cols should be named "Stage 1" etc. Rows should be named "Unsatisfactory", "Borderline", "Satisfactory", "Excellent"
#' @param nBounds The number of grade boundaries to add for the current assessment (3 means plot the B, S and E boundaries, 1 means plot only the S boundary)
#' @param nBoundsPrev The number of grade boundaries to add for the previous assessment
#'
#' @return An updated plot is returned. The new plot should be saved under the old plot (see examples)
#' @export
#'
#' @examples plt[[glue('testRetestStage{i}')]] <- fnPltTestRetestLines(plot = plt[[glue('testRetestStage{i}')]], stage = i, testInYear = cnst$testInYear, gradeBounds = tab$gradeBoundaries, gradeBoundsPrev = tab$gradeBoundariesPrev, nBounds = 3, nBoundsPrev = 3)
#' Use ifelse when setting nBounds and nBoundsPrev to avoid having multiple loops in the main script dependent on the test in year
#'
################################################################################
#'
fnPltTestRetestLines <- function(plot = NULL,
                                 stage = NULL,
                                 testInYear = NULL,
                                 gradeBounds = NULL,
                                 gradeBoundsPrev = NULL,
                                 nBounds = NULL,
                                 nBoundsPrev = NULL) {
  if (is.null(plot) == TRUE |
      is.null(stage) == TRUE |
      is.null(testInYear) == TRUE |
      is.null(gradeBounds) == TRUE |
      is.null(gradeBoundsPrev) == TRUE |
      is.null(nBounds) == TRUE |
      is.null(nBoundsPrev) == TRUE) {
    stop("One of the required variables for this function has not been specified.")
  } else {
    .stage <- glue("stage{stage}")
    .stageCol <- glue("Stage {stage}")
    .stagePrevCol <- glue("Stage {stage-1}")
    if (testInYear == 1) {
      # Test in Year = 1. Use the previous stage for previous assessment grade bounds.
      # eg A current Stage 3 student's prev grade bound data is the Stage 2 data in the prev grade bound data
      if (nBounds == 3 && nBoundsPrev == 3) {
        # Example (TiY 1): AMK Stages2/3/4/5, ADK Stages3/4/5
        plot <- plot +
          geom_hline(yintercept = gradeBounds["Excellent", .stageCol],
                     colour = "blue") +
          geom_hline(yintercept = gradeBounds["Satisfactory", .stageCol],
                     colour = "green") +
          geom_hline(yintercept = gradeBounds["Borderline", .stageCol],
                     colour = "orange") +
          geom_vline(xintercept = gradeBoundsPrev["Excellent", .stagePrevCol],
                     colour = "blue") +
          geom_vline(xintercept = gradeBoundsPrev["Satisfactory", .stagePrevCol],
                     colour = "green") +
          geom_vline(xintercept = gradeBoundsPrev["Borderline", .stagePrevCol],
                     colour = "orange")
      } else if (nBound == 1 && nBoundsPrev == 3) {
        # Example (TiY 1): AMK Stage5
        plot <- plot +
          geom_hline(yintercept = gradeBounds["Satisfactory", .stageCol],
                     colour = "green") +
          geom_vline(xintercept = gradeBoundsPrev["Excellent", .stagePrevCol],
                     colour = "blue") +
          geom_vline(xintercept = gradeBoundsPrev["Satisfactory", .stagePrevCol],
                     colour = "green") +
          geom_vline(xintercept = gradeBoundsPrev["Borderline", .stagePrevCol],
                     colour = "orange")
      }
    } else{
      # Test in Year = 2/3/4. Use the same stage for current and previous assessment grade bounds.
      # eg A current Stage 3 student's prev grade bound data is the Stage 3 data in the prev grade bound data because everyone has already sat an assessment in their current stage.
      if (nBounds == 3 && nBoundsPrev == 3) {
        # Example (TiY 2/3/4): AMK Stages1/2/3/4, ADK Stages2/3/4/5
        plot <- plot +
          geom_hline(yintercept = gradeBounds["Excellent", .stageCol],
                     colour = "blue") +
          geom_hline(yintercept = gradeBounds["Satisfactory", .stageCol],
                     colour = "green") +
          geom_hline(yintercept = gradeBounds["Borderline", .stageCol],
                     colour = "orange") +
          geom_vline(xintercept = gradeBoundsPrev["Excellent", .stageCol],
                     colour = "blue") +
          geom_vline(xintercept = gradeBoundsPrev["Satisfactory", .stageCol],
                     colour = "green") +
          geom_vline(xintercept = gradeBoundsPrev["Borderline", .stageCol],
                     colour = "orange")
      } else if (nBounds == 1 && nBoundsPrev == 1) {
        # Example (TiY 2/3/4): AMK Stage5
        plot <- plot +
          geom_hline(yintercept = gradeBounds["Satisfactory", .stageCol],
                     colour = "green") +
          geom_vline(xintercept = gradeBoundsPrev["Satisfactory", .stageCol],
                     colour = "green")
      }
    }
    return(plot)
  }
}

# *** Note: Can return to this and use scale_x_continuous(limits= c(-10,100)) for example if any x vals are negative. Could look to round to the next lowest (most negative) 10 for example. Something similar using a function was done previous for the grade box plotss
