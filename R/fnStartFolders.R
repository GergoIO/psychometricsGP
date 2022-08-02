#' Run functions to create and set variables for save folders
#'
#' @param workingDirectory Define the working directory, where save folders will be created
#' @param verNum Set a version number - appended to save folders and file names
#' @param createOutputFolder (TRUE/FALSE/NULL) Create an outputs folder in the working directory (with appended version number) if TRUE
#' @param createPulseFolder (TRUE/FALSE/NULL) Create a folder called Pulse in the Output folder if TRUE
#' @param createImagesFolder (TRUE/FALSE/NULL) Create an Images folder in the working directory (with appended version number) if TRUE
#'
#' @return The working directory is set to the location of the script file and save as variable folderWD.
#' Two folders for figures and other outputs are also created and saved as folderOutput and folderPlots
#' @export
#'
#' @examples fnStartFolders(workingDirectory = getwd(), verNum = "v1", createOutputFolder = TRUE, createPulseSubfolder = FALSE, createImagesFolder = TRUE)

################################################################################

# NOTE: The working directory is no longer determined here. Instead it is set in the original file along with the manually configured parameters.

fnStartFolders <- function(workingDirectory = NULL,
                           verNum = NULL,
                           createOutputFolder = NULL,
                           createPulseSubfolder = NULL,
                           createImagesFolder = NULL) {
  if (is.null(workingDirectory) == TRUE) {
    stop(
      "fnStartFolders: Please specify the variable 'workingDirectory' to set the working directory."
    )
  } else if (is.null(verNum) == TRUE) {
    stop(
      "fnStartFolders: Please specify the variable 'verNum' - a version number for file naming."
    )
  } else if (is.null(createOutputFolder) == TRUE |
             is.null(createImagesFolder) == TRUE |
             is.null(createPulseSubfolder) == TRUE) {
    warning(
      "fnStartFolders: Please specify the booleans createOutputFolder, createImagesFolder and createPulseSubfolder to determine which folders should be created. By default, folders will be created for any undefined booleans."
    )
  }

  # Create folders, if they don't exist
  # https://stackoverflow.com/questions/40714190/use-apply-functions-dir-exist-and-dir-create
  if (createOutputFolder != FALSE) {
    lapply(c(glue('Output ({verNum})')), function(x)
      if (!dir.exists(x))
        dir.create(
          x,
          showWarnings = TRUE,
          recursive = FALSE,
          mode = "0777"
        ))
    folderOutput <<-
      file.path(glue('{workingDirectory}\\Output ({verNum})'))
  }

  if (createImagesFolder != FALSE) {
    lapply(c(glue('Images ({verNum})')), function(x)
      if (!dir.exists(x))
        dir.create(
          x,
          showWarnings = TRUE,
          recursive = FALSE,
          mode = "0777"
        ))
    folderImages <<-
      file.path(glue('{workingDirectory}\\Images ({verNum})'))
  }

  if (createPulseSubfolder != FALSE) {
    lapply(c(glue('Output ({verNum})\\Pulse')), function(x)
      if (!dir.exists(x))
        dir.create(
          x,
          showWarnings = TRUE,
          recursive = FALSE,
          mode = "0777"
        ))
    folderOutputPulse <<-
      file.path(glue('{workingDirectory}\\Output ({verNum})\\Pulse'))
  }
} # END