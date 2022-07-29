#' Add a new caption to accompany a table and return the value of the total number of tables.
#'
#' @param report The variable name of the report
#' @param tableCount The variable name in the document which stores the current number of tables already in the report (integer, usually named 'countTab')
#' @param caption The caption to add (string)
#'
#' @return The new value for the total number of tables is returned. The output of this function should be saved to match whatever that varialbe is stored as in the main script. In addition to returning this, the formatted caption is also added the variable saving the report
#' @export
#'
#' @examples countTab <- fnRptCaptionTab(rptVar = rpt$All, tableCount = countTab, caption = "This caption")

################################################################################

fnRptCaptionTab <-
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
      tableCount <- tableCount + 1
      return(tableCount)
    }
  } # END
