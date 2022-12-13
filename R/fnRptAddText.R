#' Add Text to a Report
#'
#' @param report The variable name of the report
#' @param text The text to add to the report
#'
#' @return Nothing is explicitly returned, rather the configured text is added to the configured report
#' @export
#'
#' @examples fnRptAddText(report = rpt$All, text = "Test text")

################################################################################

fnRptAddText <- function(report = NULL, text = NULL) {
  if (is.null(report) |
      is.null(text))
  {
    stop("One of the required variables for this function has not been specified.")
  } else{
    report <- body_add_par(report,
                           text,
                           style = "Normal")
  }
}
