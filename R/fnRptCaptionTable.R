#' Add a new caption to accompany a table and increment the value of the total number of tables.
#'
#' @param report The variable name of the report
#' @param tableCount The variable name in the document which stores the current number of variables already in the report (integer, usually named 'countTab')
#' @param caption The caption to add (string)
#'
#' @return The formatted caption is added to the configured report. The variable storing the number of tables in the reported is incremented by 1
#' @export
#'
#' @examples fnRptCaptionTable(rptVar = rpt$All, tableCount = countTab, caption = "This caption")

################################################################################

fnRptCaptionTable <-
  function(report = NULL,
           tableCount = NULL,
           caption = NULL
  ) {
    if (is.null(report) == TRUE |
        is.null(caption) == TRUE | is.null(tableCount) == TRUE)
    {
      stop("One of the required variables for this function has not been specified.")
    } else{
      report <-
        body_add_par(report,
                     glue('Table {tableCount}: {caption}'),
                     style = "caption")
      # Increments and stores the variable tracking the number of plots in the report
      # The assign fn requires the variable as a string
      assign(deparse(substitute(tableCount)), tableCount + 1, envir = globalenv())
    }
  } # END
