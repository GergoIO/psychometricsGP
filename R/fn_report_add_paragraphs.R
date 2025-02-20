#' Add Multiple Paragraphs to a Word Document
#'
#' This function adds multiple paragraphs to a Word document report object.
#'
#' @param report An `officer::rdocx` object representing the Word document.
#' @param content_list A list or vector of character strings, where each element
#'   represents a paragraph to be added to the document.
#'
#' @return The modified `rdocx` object with the added paragraphs.
#' @export
#'
#' @examples
#' \dontrun{
#' library(officer)
#' doc <- read_docx()
#' content <- c("This is the first paragraph.", "This is the second paragraph.")
#' doc <- fn_report_add_paragraphs(doc, content)
#' print(doc, target = "output.docx")
#' }
fn_report_add_paragraphs <- function(report, content_list) {
  for (content in content_list) {
    if (!is.null(content)) {
      report <- body_add_par(report, content, style = "Normal")
    }
  }
  return(report)
}
