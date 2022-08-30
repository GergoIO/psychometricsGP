#' Add Text to a Report
#'
#' @param rptVar The variable name of the report
#' @param text The text to add to the report
#'
#' @return Nothing is explicitly returned, rather the configured text is added to the configured report
#' @export
#'
#' @examples fnRptAddText(rptVar = rpt$All, text = "Test text")

################################################################################

fnRptAddText <- function(rptVar = NULL, text = NULL) {
  if (is.null(rptVar) == TRUE | is.null(test) == TRUE)
  {
    stop("One of the required variables for this function has not been specified.")
  } else{
    rptVar <- body_add_par(rptVar,
                           text,
                           style = "Normal")
  }
}
