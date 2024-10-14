#' Remove negative marking
#'
#' @description Input a dataframe of scores where negative marking is to be removed. By default, all cells that are -0.25 are replaced with 0. Thought the exact replacement can be configured by defining the 'mark_old' and 'mark_new' optional variables.
#'
#' @param scores_data A single dataframe containing scores to be replaced. More columns may be present in the dataframe, but in all instances (including extra columns) all occurences of 'mark_old' cells are replaced with 'mark_new' cells
#' @param mark_old (OPTIONAL) Defaults to '-0.25'. The cell value to be replaced (by the value of 'mark_new')
#' @param mark_new (OPTIONAL) Defaults to '0'. The cell value to go in the place of the cell value to replace ('mark_old')
#'
#' @return The scores_data dataframe is returned with all the 'mark_old' cells replaced with 'mark_new'
#' @export
#'
#' @examples test <- fn_remove_neg_marking(scores_data = data.frame(c(1,1,-0.25), c(-0.25, 0, -0.25)))
#'
################################################################################
#'
fn_remove_neg_marking <- function(scores_data = NULL,
                           mark_old = -0.25,
                           mark_new = 0) {
  if (is.null(scores_data)) {
    stop("One of the required variables for this function has not been specified.")
  } else{
    # Duplicate the input scores_data dataframe
    new_scores_data <- scores_data
    # Find all instances of 'mark_old' and replace with 'mark_new'
    new_scores_data[newscores_data == mark_old] <- mark_new
    # Return the new dataframe with the replaced marks
    return(new_scores_data)
  }
}
