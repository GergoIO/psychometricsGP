#' Run the document command for the PsychometricsGP package
#'
#' @description Use this function to run the document command when updates are made in the PyschometricsGP package. The required packages (devtools, roxygen2) are both loaded and then the document() command is run.
#'
#' @return Nothing is returned, a message alerts the user when documenting is complete
#' @export
#'
#' @examples fnReload()
#'
################################################################################
#'
fnDocument <- function() {
  # Load the two packages needed for documenting
  library(devtools)
  library(roxygen2)

  # Run the document command
  document()

  message("fnReload: Documenting complete")
}
