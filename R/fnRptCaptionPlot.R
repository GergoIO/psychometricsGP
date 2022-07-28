#' Add a new caption to accompany a plot
#'
#' @param rptVar The variable name of the report
#' @param caption The caption to add (string)
#' @param plotCount The current number of plots already in the report (integer)
#'
#'
#' @return Nothing is explicitly returned, rather a formatted caption is added to the report
#' @export
#'
#' @examples fnRptCaptionPlot(rptVar = rpt$All, caption = "This caption", plotCount = 2)
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
    }
  }
