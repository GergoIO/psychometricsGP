#' Test Angoff Correlation
#'
#' @param angoffScores A vector (numeric elements) - a vector of all the included angoff scores for each item. (Must be the same length as testFacility)
#' @param testFacility A vector (numeric elements) - a vector of all the included item scores (facility). Usually, the highest stage is used. (Must be the same length as angoffScores)
#'
#' @return A list of constants is returned. These include the correlation coefficient and the bounds of the 95 confidence interval.
#' @export
#'
#' @examples cnst <- append(
#' cnst,
#'  fnTestAngoffCorr(
#'  angoffScores = fnRmRowsByName(dfAngoff$Score, cnst$itemsExcl),
#'  testFacility = tab$itemAnalysis[["Stage 5 Facility"]]
#'  )
#'  )
#'
################################################################################
#'
fnTestAngoffCorr <- function(angoffScores = NULL,
                             testFacility = NULL) {
  if (is.null(angoffScores) == TRUE |
      is.null(testFacility) == TRUE) {
    stop(
      "One of the required variables for this function has not been specified."
    )
  } else{
    testAngoffList = list()

    # Run the correlation test of the Angoff and achieved item scores
    corrTest <-
      cor.test(angoffScores,
               testFacility)

    testAngoffList$testAngoffCorrVal <- .corrTest$estimate[[1]]
    testAngoffList$testAngoffCorrLowCI <- .corrTest$conf.int[1]
    testAngoffList$testAngoffCorrHighCI <- .corrTest$conf.int[2]
  }
}
