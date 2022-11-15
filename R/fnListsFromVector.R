#' Create Empty Lists From a Vector of Strings
#'
#' @description This function requires an input of a vector of strings. Each element of the vector is checked. If the element does not already exist as a variable, then a new variable (with the name of the vector element) is created. The variable is assigned as an empty list.
#'
#' @param listsToCreate A vector of strings.
#'
#' @return Creates the required lists from the input vector in the main (global) environment
#' @export
#'
#' @examples fnListsFromVector(listsToCreate = c("itemAnalysis", "lReliab", "lResults", "plt", "rpt", "rpt$tabAll", "tab", "tabPulse", "testRetest", "testAngoff", "tabDemog"))
#'
################################################################################
#'
fnListsFromVector <- function(listsToCreate = NULL) {
  if (in.null(listsToCreate) == TRUE) {
    stop(
      "fnListsFromVector: One of the required variables for this function has not been specified."
    )
  } else{
    # Check all elements of input vector
    for (i in 1:length(listsToCreate)) {
      # Only run for elements of the input vector where a corresponding variable does not already exist
      if (exists(as.character(listsToCreate[i])) == FALSE) {
        assign(listsToCreate[i], list(), envir = globalenv())
      }
    }
  }
}
