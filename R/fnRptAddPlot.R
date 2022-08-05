#' Add a plot with accompanying caption
#'
#' @param report The variable name of the report
#' @param plot The variable name of the plot
#' @param plotCount The variable name in the document which stores the current number of plots already in the report (integer, usually named 'countPlt')
#' @param caption The caption to add (string)
#' @param stopTrailingLine TRUE/FALSE - decides whether to add an empty line below the plot (default is FALSE if undeclared)
#'
#' @return The plot is added with an accompanying formatted caption to the configured report. The variable storing the number of plots in the report is incremented by 1 and a single trailing line is added following the caption (if requested)
#' @export
#'
#' @examples fnRptAddPlot(report = rpt$All, plot = plot1, plotCount = countPlt, caption = "This caption", addTrailingLine = TRUE)

################################################################################

fnRptAddPlot <-
  function(report = NULL,
           plot = NULL,
           plotCount = NULL,
           caption = NULL,
           stopTrailingLine = FALSE) {
    if (is.null(report) == TRUE | is.null(plot) == TRUE |
        is.null(caption) == TRUE |
        is.null(plotCount) == TRUE)    {
      stop("One of the required variables for this function has not been specified.")
    } else if (is.logical(report)) {
      stop("The variable 'addTrailingLine' must be set to either 'TRUE' or 'FALSE'.")
    }
    else{
      report <- body_add_gg(report,
                            value = plot,
                            height = 4,
                            width = 6)
      # Caption goes below the plot
      fnRptCaptionPlot(report = report,
                       plotCount = plotCount,
                       caption = caption)
      # Ensure that the variable fed through as 'plotCount' is updated in main script
      # The below line already appears in 'fnRptCaptionPlot' - however it must be repeated in the top level function so that the 'plotCount' variable is updated in the main script too
      assign(deparse(substitute(plotCount)), plotCount + 1, envir = globalenv())
      if (stopTrailingLine == FALSE) {

      } else if (stopTrailingLine %in% c(NULL, FALSE)) {
        fnRptAddParagraph(report)
      }
    }
  } # END