#' Add Grade Boundary Lines to Test Retest Plots
#'
#' @description A function for adding grade boundary lines to existing scatter plots of test retest data. The function handles changing the stage for the previous test when the current test is the first test in the year. If used in a loop, the number of bounds (previous and current) must be correct for each possible stage to be considered. If there are stages with a different number of grade bounds, use ifelse statements to account for that when setting nBounds and nBoundsPrev
#'
#' NEW FUNCTIONALITY: If the assessment type is defined (along with the stage and the test in year), the number of current and previous grade boundaries to apply is automatically set.
#'
#' @param plot A plot - a scatterplot of the stage specific test retest data
#' @param stage Numeric (integer) - the current stage being plotted
#' @param testInYear Numeric (integer) - the test in the academic year of this assessment. 1 for the first assessment
#' @param gradeBounds A dataframe of the grade boundaries for the current assessment. Cols should be named "Stage 1" etc. Rows should be named "Unsatisfactory", "Borderline", "Satisfactory", "Excellent"
#' @param gradeBoundsPrev A dataframe of the grade boundaries for the previous assessment. Cols should be named "Stage 1" etc. Rows should be named "Unsatisfactory", "Borderline", "Satisfactory", "Excellent"
#' @param nBounds (OPTIONAL - if assessmentType is set) The number of grade boundaries to add for the current assessment (3 means plot the B, S and E boundaries, 1 means plot only the S boundary)
#' @param nBoundsPrev (OPTIONAL - if assessmentType is set) The number of grade boundaries to add for the previous assessment
#' @param assessmentType The assessment type. Currently configured options are: "ADK", "ADTK", "AMK"
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
                                 nBoundsPrev = NULL,
                                 assessmentType = NULL) {
  if (is.null(plot) |
      is.null(stage) |
      is.null(testInYear) |
      is.null(gradeBounds) |
      is.null(gradeBoundsPrev)) {
    stop("One of the required variables for this function has not been specified.")
  } else {
    # Set variable to later retrieve data programatically
    .stage <- glue("stage{stage}")
    .stageCol <- glue("Stage {stage}")
    .stagePrevCol <- glue("Stage {stage-1}")

    if (is.null(assessmentType)) {
      #   ______________________________________________________________________
      #   Assessment Type NOT Set                                           ####

      if (testInYear == 1) {
        # Test in Year = 1. Use the previous stage for previous assessment grade bounds.
        # eg A current Stage 3 student's prev grade bound data is the Stage 2 data in the prev grade bound data
        if (nBounds == 3 && nBoundsPrev == 3) {
          # Example (TiY 1): AMK Stages2/3/4/5, ADK Stages3/4/5, ADTK Stages2/3
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
        } else if (nBounds == 1 && nBoundsPrev == 3) {
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
          # Example (TiY 2/3/4): AMK Stages1/2/3/4, ADK Stages2/3/4/5, ADTK Stages2/3
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
    } else {
      #   ______________________________________________________________________
      #   Assessment Type IS Set                                            ####

      # Doing the same as above but not requiring manually set current and previous number of grade bounds

      if (testInYear == 1) {
        # Test in Year = 1. Use the previous stage for previous assessment grade bounds.
        # eg A current Stage 3 student's prev grade bound data is the Stage 2 data in the prev grade bound data

        # Example (TiY 1): AMK Stages2/3/4, ADK Stages3/4/5, ADTK Stages2/3
        if ((assessmentType == "AMK" && stage %in% c(2, 3, 4)) ||
            (assessmentType == "ADK" && stage %in% c(3, 4, 5)) ||
            (assessmentType == "ADTK" && stage %in% c(2, 3))) {
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
        } else if (assessmentType == "AMK" && stage == 5) {
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

        if ((assessmentType == "AMK" && stage %in% c(1, 2, 3, 4)) ||
            (assessmentType == "ADK" && stage %in% c(2, 3, 4, 5)) ||
            (assessmentType == "ADTK" && stage %in% c(2, 3))) {
          # Example (TiY 2/3/4): AMK Stages1/2/3/4, ADK Stages2/3/4/5, ADTK Stages2/3
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
        } else if (assessmentType == "AMK" && stage == 5) {
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
}
# *** Note: Can return to this and use scale_x_continuous(limits= c(-10,100)) for example if any x vals are negative. Could look to round to the next lowest (most negative) 10 for example. Something similar using a function was done previous for the grade box plots
