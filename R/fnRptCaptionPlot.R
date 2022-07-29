#' Add a new caption to accompany a plot and return the value of the total number of plots.
#'
#' @param rptVar The variable name of the report
#' @param caption The caption to add (string)
#' @param plotCount The variable name in the document which stores the current number of plots already in the report (integer, usually named 'countPlt')
#'
#' @return The new value for the total number of plots is returned. The output of this function should be saved to match whatever that varialbe is stored as in the main script. In addition to returning this, the formatted caption is also added the variable saving the report
#' @export
#'
#' @examples fnRptCaptionPlot(rptVar = rpt$All, caption = "This caption", plotCount = countPlt)

################################################################################

fnRptCaptionPlot <-
  function(rptVar = NULL,
           caption = NULL,
           plotCount = NULL) {
    if (is.null(rptVar) == TRUE |
        is.null(caption) == TRUE | is.null(plotCount) == TRUE)
    {
      stop("One of the required variables for this function has not been specified.")
    } else{
      rptVar <-
        body_add_par(rptVar,
                     glue('Figure {plotCount}: {caption}'),
                     style = "caption")
      plotCount <- plotCount + 1
      return(plotCount)
    }
  }
