#' Detect Items to Review
#'
#' @param assessmentType A string. The type of assessment being analysed. This will determine the selection process for finding and returning items to review
#' @param itemAnalysisData A dataframe. This dataframe must contain at least the following columns: "Item" (the items number), "Stage 5 Facility" (for ADK), "Stage 5 vs 2 Growth" (for ADK), "Stage 5 PtBis" (for ADK) or "Stage 3 Facility" (for ADTK), "Stage 3 vs 2 Growth" (for ADTK), "Stage 3 PtBis" (for ADTK)
#'
#' @return A list of the detected items to review is returned.
#' @export
#'
#' @examples #For ADK/ADTK:
#'  cnst <- append(
#'  cnst,
#'   fnItemReview(assessmentType = cnst$assessmentType, itemAnalysisData = tab$itemAnalysis)
#'   )
#'
################################################################################
#'
fnItemReview <- function(assessmentType = NULL,
                         itemAnalysisData = NULL) {
  if (is.null(assessmentType) |
      is.null(itemAnalysisData)) {
    stop("One of the required variables for this function has not been specified.")
  } else{
    listOfItemReview <- list()

    #   ________________________________________________________________________
    #   ADK                                                                 ####
    if (assessmentType %in% c("ADK")) {
      # Low (<20%) Facility
      listOfItemReview$itemReviewLowFac <-
        unique(sort(as.numeric(c(
          na.omit(itemAnalysisData$Item[itemAnalysisData[["Stage 5 Facility"]] < 0.2])
        ))))
      listOfItemReview$itemReviewLowFac <-
        toString(listOfItemReview$itemReviewLowFac, sep = ",")

      # Negative Growth Items
      listOfItemReview$itemReviewNegGrowth <-
        unique(sort(as.numeric(c(
          na.omit(itemAnalysisData$Item[itemAnalysisData[["Stage 5 vs 2 Growth"]] < 0])
        ))))
      listOfItemReview$itemReviewNegGrowth <-
        toString(listOfItemReview$itemReviewNegGrowth, sep = ",")

      # Negative PtBis Items
      listOfItemReview$itemReviewNegPtBis <-
        unique(sort(as.numeric(c(
          na.omit(itemAnalysisData$Item[itemAnalysisData[["Stage 5 PtBis"]] < 0])
        ))))
      listOfItemReview$itemReviewNegPtBis <-
        toString(listOfItemReview$itemReviewNegPtBis, sep = ",")
    } else if (assessmentType %in% c("ADTK")) {
      #   ______________________________________________________________________
      #   ADTK                                                              ####
      # Low (<20%) Facility
      listOfItemReview$itemReviewLowFac <-
        unique(sort(as.numeric(c(
          na.omit(itemAnalysisData$Item[itemAnalysisData[["Stage 3 Facility"]] < 0.2])
        ))))
      listOfItemReview$itemReviewLowFac <-
        toString(listOfItemReview$itemReviewLowFac, sep = ",")

      # Negative Growth Items
      listOfItemReview$itemReviewNegGrowth <-
        unique(sort(as.numeric(c(
          na.omit(itemAnalysisData$Item[itemAnalysisData[["Stage 3 vs 2 Growth"]] < 0])
        ))))
      listOfItemReview$itemReviewNegGrowth <-
        toString(listOfItemReview$itemReviewNegGrowth, sep = ",")

      # Negative PtBis Items
      listOfItemReview$itemReviewNegPtBis <-
        unique(sort(as.numeric(c(
          na.omit(itemAnalysisData$Item[itemAnalysisData[["Stage 3 PtBis"]] < 0])
        ))))
      listOfItemReview$itemReviewNegPtBis <-
        toString(listOfItemReview$itemReviewNegPtBis, sep = ",")
    }
    return(listOfItemReview)
  }
}
