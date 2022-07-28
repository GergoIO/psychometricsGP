#' Add a section heading to a report
#'
#' @param rptVar The variable name of the report
#' @param headingTitle This string will be the added section heading
#'
#' @return Nothing is explicitly returned, rather the desired section heading is added to the report
#' @export
#'
#' @examples fnRptSectionHeading(rptVar = rpt$All, headingTitle = "Grade Distributions")
fnRptSectionHeading <- function(rptVar = NULL,
                                headingTitle = NULL) {
  if (is.null(rptVar) == TRUE |
      is.null(headingTitle) == TRUE)
  {
    stop("One of the required variables for this function has not been specified.")
  } else{
    rptVar <- body_add_par(rptVar, headingTitle, style = "heading 3")
  }
}
