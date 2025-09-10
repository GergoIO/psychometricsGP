#' Add Plot to Officer Report with Automatic Sequential Numbering
#'
#' Adds a ggplot2 plot and caption to an officer report object.
#' Automatically increments the specified plot count variable in the caller's workspace,
#' so subsequent plots are sequentially numbered. Intended for building professional
#' assessment and performance reports.
#'
#' @param report officer report object to be modified.
#' @param plot ggplot2 plot object to be embedded in the report.
#' @param plot_count Integer; the sequential figure number. This should be an integer variable in the calling scope and will be incremented automatically.
#' @param caption Character string; description placed beneath the plot.
#' @param stop_trailing_line Logical; if FALSE or NULL, adds a blank line for spacing after the plot and caption. Defaults to FALSE.
#' @param dimensions Numeric vector of length two specifying plot width and height in centimetres. Defaults to c(15.24, 10.16).
#'
#' @details
#' This function adds a ggplot2 graphic and caption to an officer report,
#' with automatic figure numbering using a local plot count variable.
#' It is designed to be run with a mutable integer variable, typically called \code{n_plot} or \code{plot_count},
#' in the caller's environment, which will be incremented as plots are added.
#'
#' @section Usage Example:
#'
#' \dontrun{
#' library(officer)
#' library(ggplot2)
#'
#' my_report <- read_docx()
#' n_plot <- 1
#'
#' p <- ggplot(mtcars, aes(mpg)) + geom_histogram()
#'
#' my_report <- fn_report_add_plot(
#'   report = my_report,
#'   plot = p,
#'   plot_count = n_plot,
#'   caption = "Distribution of Miles Per Gallon in mtcars dataset."
#' )
#' # n_plot is automatically incremented after each call
#'
#' # Add another plot (shows figure number increasing)
#' p2 <- ggplot(mtcars, aes(hp)) + geom_histogram()
#' my_report <- fn_report_add_plot(
#'   report = my_report,
#'   plot = p2,
#'   plot_count = n_plot,
#'   caption = "Distribution of Horsepower."
#' )
#' }
#'
#' @return Officer report object with plot and caption appended.
#' @seealso \code{\link[officer]{body_add_gg}}, \code{\link[officer]{body_add_par}}
#' @importFrom officer body_add_gg body_add_par
#' @export
fn_report_add_plot <- function(report = NULL, plot = NULL, plot_count = NULL, caption = NULL,
                         stop_trailing_line = FALSE, dimensions = c(15.24, 10.16)) {
  if (is.null(report) | is.null(plot) | is.null(caption) | is.null(plot_count)) {
    stop("One of the required variables for this function has not been specified.")
  } else {
    report <- body_add_gg(report, value = plot, width = as.numeric(dimensions[1] / 2.54),
                          height = as.numeric(dimensions[2] / 2.54))
    caption_text <- paste("Figure", plot_count, ":", caption)
    report <- body_add_par(report, caption_text, style = "caption")
    assign(deparse(substitute(plot_count)), plot_count + 1, envir = parent.frame())
    if (stop_trailing_line %in% c(NULL, FALSE)) {
      report <- body_add_par(report, "")
    }
    return(report)
  }
}
