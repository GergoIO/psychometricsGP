#' Detect Items to Review
#'
#' @param assessmentType A string. The type of assessment being analysed. This will determine the selection process for finding and returning items to review
#' @param itemAnalysisData A dataframe. This dataframe must contain at least the following columns: "Item" (the items number), "Stage 5 Facility" (for ADK), "Stage 5 vs 2 Growth" (for ADK), "Stage 5 PtBis" (for ADK) OR "Stage 3 Facility" (for ADTK), "Stage 3 vs 2 Growth" (for ADTK), "Stage 3 PtBis" (for ADTK) OR "Stage 2 Facility" (for PAPT), "Stage 2 vs 1 Growth" (for PAPT), "Stage 2 PtBis" (for PAPT) OR "Facility" (for IDS/Y1KT), "PtBis" (for IDS/Y1KY)
#' @param testInYear (OPTIONAL unless AMK) - Required for AMK assessments so the stage to use for item review can be determined
#'
#' @return A list of the detected items to review is returned.
#' @export
#'
#' @examples #For ADK/ADTK/PAPT/IDS/Y1KT/AMK/AKT:
#'  cnst <- append(
#'  cnst,
#'   fnItemReview(assessmentType = cnst$assessmentType, itemAnalysisData = tab$itemAnalysis)
#'   )
#'
#' # For AMK:
#'  cnst <- append(
#'  cnst,
#'   fnItemReview(assessmentType = cnst$assessmentType, itemAnalysisData = tab$itemAnalysis, testInYear = cnst$testInYear)
#'   )
#'
################################################################################
#'
fnItemReview <- function(assessmentType = NULL,
                         itemAnalysisData = NULL,
                         testInYear = NULL) {
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
    } else if (assessmentType %in% c("AMK")) {
      #   ______________________________________________________________________
      #   AMK                                                               ####

      if (is.null(testInYear)) {
        stop("For AMK assessments, the test in year (testInYear) variable must be defined.")
      } else {
        if (testInYear %in% c(1, 2)) {
          # Low (<20%) Facility - Stage 5
          listOfItemReview$itemReviewLowFac <-
            unique(sort(as.numeric(c(
              na.omit(itemAnalysisData$Item[itemAnalysisData[["Stage 5 Facility"]] < 0.2])
            ))))
          # Negative PtBis Items - Stage 5
          listOfItemReview$itemReviewNegPtBis <-
            unique(sort(as.numeric(c(
              na.omit(itemAnalysisData$Item[itemAnalysisData[["Stage 5 PtBis"]] < 0])
            ))))
        } else {
          # Low (<20%) Facility - Stage 4
          listOfItemReview$itemReviewLowFac <-
            unique(sort(as.numeric(c(
              na.omit(itemAnalysisData$Item[itemAnalysisData[["Stage 4 Facility"]] < 0.2])
            ))))
          # Negative PtBis Items - Stage 4
          listOfItemReview$itemReviewNegPtBis <-
            unique(sort(as.numeric(c(
              na.omit(itemAnalysisData$Item[itemAnalysisData[["Stage 4 PtBis"]] < 0])
            ))))

        }
        # Convert to strings
        listOfItemReview$itemReviewLowFac <-
          toString(listOfItemReview$itemReviewLowFac, sep = ",")
        listOfItemReview$itemReviewNegPtBis <-
          toString(listOfItemReview$itemReviewNegPtBis, sep = ",")
      }
    } else if (assessmentType %in% c("PAPT")) {
      #   ______________________________________________________________________
      #   PAPT                                                              ####

      # Low (<20%) Facility
      listOfItemReview$itemReviewLowFac <-
        unique(sort(as.numeric(c(
          na.omit(itemAnalysisData$Item[itemAnalysisData[["Stage 2 Facility"]] < 0.2])
        ))))
      listOfItemReview$itemReviewLowFac <-
        toString(listOfItemReview$itemReviewLowFac, sep = ",")

      # Negative Growth Items
      listOfItemReview$itemReviewNegGrowth <-
        unique(sort(as.numeric(c(
          na.omit(itemAnalysisData$Item[itemAnalysisData[["Stage 2 vs 1 Growth"]] < 0])
        ))))
      listOfItemReview$itemReviewNegGrowth <-
        toString(listOfItemReview$itemReviewNegGrowth, sep = ",")

      # Negative PtBis Items
      listOfItemReview$itemReviewNegPtBis <-
        unique(sort(as.numeric(c(
          na.omit(itemAnalysisData$Item[itemAnalysisData[["Stage 2 PtBis"]] < 0])
        ))))
      listOfItemReview$itemReviewNegPtBis <-
        toString(listOfItemReview$itemReviewNegPtBis, sep = ",")

      # Statistically Significant Negative PtBis Items
      listOfItemReview$itemReviewSigNegPtBis <-
        unique(sort(as.numeric(c(
          na.omit(itemAnalysisData$Item[itemAnalysisData[["Stage 2 Sig PtBis"]] < 0])
        ))))
      listOfItemReview$itemReviewSigNegPtBis <-
        toString(listOfItemReview$itemReviewSigNegPtBis, sep = ",")

    } else if (assessmentType %in% c("IDS", "Y1KT", "AKT")) {
      #   ______________________________________________________________________
      #   IDS/Y1KT                                                          ####

      # Low (<20%) Facility
      listOfItemReview$itemReviewLowFac <-
        unique(sort(as.numeric(c(
          na.omit(itemAnalysisData$Item[itemAnalysisData[["Facility"]] < 0.2])
        ))))
      listOfItemReview$itemReviewLowFac <-
        toString(listOfItemReview$itemReviewLowFac, sep = ",")

      # Negative PtBis Items
      listOfItemReview$itemReviewNegPtBis <-
        unique(sort(as.numeric(c(
          na.omit(itemAnalysisData$Item[itemAnalysisData[["PtBis"]] < 0])
        ))))
      listOfItemReview$itemReviewNegPtBis <-
        toString(listOfItemReview$itemReviewNegPtBis, sep = ",")
    }
    return(listOfItemReview)
  }
}
