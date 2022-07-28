#' Add a new paragraph (1 line of vertical space) to a report
#'
#' @param rptVar The variable name of the report
#'
#' @return Nothing is explicitly returned, rather a line of space is added to the configured report
#' @export
#'
#' @examples fnRptAddParagraph(rptVar = rpt$All)
fnRptAddParagraph <- function(rptVar = NULL) {
  if (is.null(rptVar) == TRUE)
  {
    stop("One of the required variables for this function has not been specified.")
  } else{
    rptVar <- body_add_par(rptVar, "")
  }
}
