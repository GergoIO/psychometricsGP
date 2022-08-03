#' Add a table with accompanying caption
#'
#' @param report The variable name of the report
#' @param table The variable name of the table
#' @param tableCount The variable name in the document which stores the current number of tables already in the report (integer, usually named 'countTab')
#' @param caption The caption to add (string)
#' @param addTrailingLine TRUE/FALSE
#'
#' @return The table is added with an accompanying formatted caption to the configured report. The variable storing the number of tables in the report is incremented by 1 and a single trailing line is added following the caption (if requested)
#' @export
#'
#' @examples fnRptAddTable(report = rpt$All, table = table1, tableCount = countTab, caption = "This caption", addTrailingLine = TRUE)

################################################################################

fnRptAddPlot <-
  function(report = NULL,
           table = NULL,
           tableCount = NULL,
           caption = NULL,
           addTrailingLine = NULL) {
    if (is.null(report) == TRUE | is.null(table) == TRUE |
        is.null(caption) == TRUE |
        is.null(tableCount) == TRUE |
        is.null(addTrailingLine) == TRUE)
    {
      stop("One of the required variables for this function has not been specified.")
    } else if (is.logical(report)) {
      stop("The variable 'addTrailingLine' must be set to either 'TRUE' or 'FALSE'.")
    }
    else{
      # Caption goes above table
      fnRptCaptionTable(report = report,
                        tableCount = tableCount,
                        caption = caption)
      # Ensure that the variable fed through as 'tableCount' is updated in main script
      # The below line already appears in 'fnRptCaptionTable' - however it must be repeated in the top level function so that the 'tableCount' variable is updated in the main script too
      assign(deparse(substitute(tableCount)), tableCount + 1, envir = globalenv())

      rpt$All <-
        body_add_flextable(x = report,
                           value = qflextable(table))
      if (addTrailingLine == TRUE) {
        fnRptAddParagraph(report)
      }
    }
  } # END
