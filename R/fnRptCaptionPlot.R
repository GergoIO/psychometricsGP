#' Add a new caption to accompany a plot and increment the value of the total number of plots.
#'
#' @param report The variable name of the report
#' @param plotCount The variable name in the document which stores the current number of plots already in the report (integer, usually named 'countPlt')
#' @param caption The caption to add (string)
#'
#' @return The formatted caption is added to the configured report. The variable storing the number of plots in the reported is incremented by 1
#' @export
#'
#' @examples fnRptCaptionPlot(report = rpt$All, plotCount = countPlt, caption = "This caption")

################################################################################

fnRptCaptionPlot <-
  function(report = NULL,
           plotCount = NULL,
           caption = NULL) {
    if (is.null(report) == TRUE |
        is.null(caption) == TRUE | is.null(plotCount) == TRUE)
    {
      stop("One of the required variables for this function has not been specified.")
    } else{
      report <-
        body_add_par(report,
                     glue('Figure {plotCount}: {caption}'),
                     style = "caption")
      # Increments and stores the variable tracking the number of plots in the report
      # The assign fn requires the variable as a string
      assign(deparse(substitute(plotCount)), plotCount + 1, envir = globalenv())
    }
  } # END
