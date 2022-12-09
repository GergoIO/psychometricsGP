#' Remove negative marking
#'
#' @description Input a dataframe of scores where negative marking is to be removed. By default, all cells that are -0.25 are replaced with 0. Thought the exact replacement can be configured by defining the 'markOld' and 'markNew' optional variables.
#'
#' @param scoresData A single dataframe containing scores to be replaced. More columns may be present in the dataframe, but in all instances (including extra columns) all occurences of 'markOld' cells are replaced with 'markNew' cells
#' @param markOld (OPTIONAL) Defaults to '-0.25'. The cell value to be replaced (by the value of 'markNew')
#' @param markNew (OPTIONAL) Defaults to '0'. The cell value to go in the place of the cell value to replace ('markOld')
#'
#' @return The scoresData dataframe is returned with all the 'markOld' cells replaced with 'markNew'
#' @export
#'
#' @examples test <- fnRmNegMarking(scoresData = data.frame(c(1,1,-0.25), c(-0.25, 0, -0.25)))
#'
################################################################################
#'
fnRmNegMarking <- function(scoresData = NULL,
                           markOld = -0.25,
                           markNew = 0) {
  if (is.null(scoresData)) {
    stop("One of the required variables for this function has not been specified.")
  } else{
    # Duplicate the input scoresData dataframe
    newScoresData <- scoresData
    # Find all instances of 'markOld' and replace with 'markNew'
    newScoresData[newScoresData == markOld] <- markNew
    # Return the new dataframe with the replaced marks
    return(newScoresData)
  }
}
