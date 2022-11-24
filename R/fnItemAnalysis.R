#' Item Analysis
#'
#' @param stages A vector - denoting the stages to be considered. (stages = 2 or stages = c(2,3,4,5) etc.). It makes sense to set all stages that sat the assessment.
#' @param scoresAll Stage separated (each list item should be stage1 etc. with ALL (including excluded items) items showing non-negatively marked item (col) scores for each student (row) in that stage (lResults$stagesScoresAll)
#' @param excludedItems A vector - denoting the items to remove decided post-test. If empty, the data marked 'all' will be the same as data without the 'all' demarkation
#' @param facilityBounds A two-element vector (usually c(0.2, 0.8)) containing the thresholds where item facilities are deemed low or high respectively. (0.2 corresponds to a 20% item facility etc.)
#'
#' @return A list of dataframes is returned containing all the item analysis info. If saving this list to an existing list of dataframes, make sure to use the append function. Otherwise, this returned list can be saved directly to an empty variable.
#' @export
#'
#' @examples itemAnalysis <- fnItemAnalysis(stages = cnst$stages, scoresAll = lResults$stagesScoresAll, excludedItems = cnst$itemsExcl, facilityBounds = c(0.2,0.8))
#'
################################################################################
#'
fnItemAnalysis <- function(stages = NULL,
                           scoresAll = NULL,
                           excludedItems = NULL,
                           facilityBounds = NULL) {
  if (is.null(stages) == TRUE |
      is.null(scoresAll) == TRUE |
      is.null(excludedItems) == TRUE |
      is.null(facilityBounds) == TRUE) {
    stop("One of the required variables for this function has not been specified.")
  } else{
    itemAnalysis <- list()

    for (i in stages) {
      .stage <- glue("stage{i}")
      # .stageCol <- glue("Stage {i}")
      .stageScoresAll <- scoresAll[[.stage]]
      .stageScores <-
        fnRmColsByName(.stageScoresAll, excludedItems)

      matrixStageAll <- na.exclude(as.matrix(.stageScoresAll))
      matrixStage <- na.exclude(as.matrix(.stageScores))

      #   ______________________________________________________________________
      #   Facility: Stage Separated                                         ####
      # Calculate stage-specific facility

      itemAnalysis$stagesFacilityAll[[.stage]] <-
        apply(matrixStageAll, 2, mean)
      itemAnalysis$stagesFacility[[.stage]] <-
        apply(matrixStage, 2, mean)

      #   ______________________________________________________________________
      #   Facility: Combined                                                ####
      # Combined calculated stage-specific facility data

      itemAnalysis$facilityAll <-
        as.data.frame(do.call("cbind", itemAnalysis$stagesFacilityAll))
      colnames(itemAnalysis$facilityAll) <-
        gsub("stage", "Stage ", colnames(itemAnalysis$facilityAll), fixed = TRUE)
      itemAnalysis$facility <-
        as.data.frame(do.call("cbind", itemAnalysis$stagesFacility))
      colnames(itemAnalysis$facility) <-
        gsub("stage", "Stage ", colnames(itemAnalysis$facility), fixed = TRUE)

      #   ______________________________________________________________________
      #   PtBis                                                             ####

      matrixTotAll <- apply(matrixStageAll, 1, sum)
      matrixTotNoIndexAll <- matrixTotAll - (matrixStageAll)
      itemAnalysis$stagesPtBisAll[[.stage]] <-
        suppressWarnings(diag(cor(matrixStageAll, matrixTotNoIndexAll)))

      matrixTot <- apply(matrixStage, 1, sum)
      matrixTotNoIndex <- matrixTot - (matrixStage)
      itemAnalysis$stagesPtBis[[.stage]] <-
        suppressWarnings(diag(cor(matrixStage, matrixTotNoIndex)))

      #   ______________________________________________________________________
      #   TAB: Facility Summary                                             ####
      # Create a summary of the stage separated high and low facility items
      # Set constants for the low and high facility boundary variables
      .facLow = facilityBounds[1]
      .facHigh = facilityBounds[2]

      .tabFacSummary <-
        data.frame(rbind(length(
          which(itemAnalysis$stagesFacility[[.stage]] < .facLow)
        ), length(
          which(itemAnalysis$stagesFacility[[.stage]] > .facHigh)
        )))
      rownames(.tabFacSummary) <-
        c(glue("Facility < {.facLow}"),
          glue("Facility > {.facHigh}"))
      colnames(.tabFacSummary) <- c(glue("Stage {i}"))

      # new_df[,ncol(new_df)+1]
      if (i == cnst$stages[1]) {
        itemAnalysis$facilitySummary <- .tabFacSummary
      } else{
        itemAnalysis$facilitySummary <-
          cbind(itemAnalysis$facilitySummary, .tabFacSummary)
      }
    }
    return(itemAnalysis)
  }
}
