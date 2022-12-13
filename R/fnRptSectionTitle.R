#' Add a section title (level 2) to a report
#'
#' @param rptVar The variable name of the report
#' @param sectionTitle This string will be the added section title
#'
#' @return Nothing is explicitly returned, rather the desired section title is added to the report (this is one level higher than fnRptSectionHeading)
#' @export
#'
#' @examples fnRptSectionTitle(rptVar = rpt$All, sectionTitle = "Grade Distributions")
fnRptSectionTitle <- function(rptVar = NULL,
                                sectionTitle = NULL) {
  if (is.null(rptVar) |
      is.null(sectionTitle))
  {
    stop("One of the required variables for this function has not been specified.")
  } else{
    rptVar <- body_add_par(rptVar, sectionTitle, style = "heading 2")
  }
}
