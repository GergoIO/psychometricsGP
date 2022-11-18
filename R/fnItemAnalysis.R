#' Item Analysis
#'
#' @param stages ***
#' @param scores *** Stage separated (each list item should be stage1 etc. with all items showing non-negatively marked item (col) scores for each student (row) in that stage lResults$stagesScoresAll
#' @param excludedItems ***
#'
#' @return ***
#' @export
#'
#' @examples ***
#'
################################################################################
#'
fnItemAnalysis <- function(stages = NULL,
                           scores = NULL,
                           excludedItems = NULL) {
  itemAnalysis <- list()

  for (i in stages) {
    .stage <- glue("stage{i}")
    # .stageCol <- glue("Stage {i}")
    .stageScoresAll <- scores[[.stage]]
    .stageScores <- fnRmColsByName(.stageScoresAll, excludedItems)

    matrixStageAll <- na.exclude(as.matrix(.stageScoresAll))
    matrixStage <- na.exclude(as.matrix(.stageScores))

    #   ____________________________________________________________________________
    #   Facilities                                                              ####

    itemAnalysis$stagesFacilityAll[[.stage]] <-
      apply(matrixStageAll, 2, mean)
    itemAnalysis$stagesFacility[[.stage]] <-
      apply(matrixStage, 2, mean)

    #   ____________________________________________________________________________
    #   PtBis                                                                   ####

    matrixTotAll <- apply(matrixStageAll, 1, sum)
    matrixTotNoIndexAll <- matrixTotAll - (matrixStageAll)
    itemAnalysis$stagesPtBisAll[[.stage]] <-
      diag(cor(matrixStageAll, matrixTotNoIndexAll))

    matrixTot <- apply(matrixStage, 1, sum)
    matrixTotNoIndex <- matrixTot - (matrixStage)
    itemAnalysis$stagesPtBis[[.stage]] <-
      diag(cor(matrixStage, matrixTotNoIndex))

    #   ____________________________________________________________________________
    #   TAB: Facility Summary                                                   ####
    # Create a summary of the stage separated high and low facility items

    .tabFacSummary <-
      data.frame(rbind(length(
        which(itemAnalysis$stagesFacility[[.stage]] < 0.2)
      ), length(
        which(itemAnalysis$stagesFacility[[.stage]] > 0.8)
      )))
    rownames(.tabFacSummary) <-
      c("Facility < 0.2", "Facility > 0.8")
    colnames(.tabFacSummary) <- c(glue("Stage {i}"))

    # new_df[,ncol(new_df)+1]
    if (i == cnst$stages[1]) {
      itemAnalysis$facilitySummary <- .tabFacSummary
    } else{
      itemAnalysis$facilitySummary <-
        cbind(itemAnalysis$facilitySummary, .tabFacSummary)
    }
  }
  return(listOfItemAnalysis)
}
