#' Run the document command for the PsychometricsGP package
#'
#' @description Use this function to run the document command when updates are made in the PyschometricsGP package. The required packages (devtools, roxygen2) are both loaded and then the document() command is run.
#'
#' @author Gergo Pinter, \email{gergo.pinter@@plymouth.ac.uk}, \url{https://www.gergo.io}
#'
#' @return Nothing is returned, a message alerts the user when documenting is complete
#' @import devtools
#' @import roxygen2

#' @export
#'
#' @examples fn_document()
#'
################################################################################
#'
fn_document <- function() {
  if (!require("devtools")) {
    stop("devtools is not installed. Please install devtools to use this function.")
  }
  if (!require("roxygen2")) {
    stop("roxygen2 is not installed. Please install roxygen2 to use this function.")
  }

  # Load the two packages needed for documenting
  library(devtools)
  library(roxygen2)

  # Run the document command
  suppressWarnings(document())

  message("fn_document: Documenting complete")
}
