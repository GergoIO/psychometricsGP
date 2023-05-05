#' Run functions to create and set variables for save folders
#'
#' @param workingDirectory Define the working directory, where save folders will be created
#' @param verNum Set a version number - appended to save folders and file names
#' @param createOutputFolder (TRUE/FALSE/NULL) Create an outputs folder in the working directory (with appended version number) if TRUE
#' @param createPulseFolder (TRUE/FALSE/NULL) Create a folder called Pulse in the Output folder if TRUE
#' @param createImagesFolder (TRUE/FALSE/NULL) Create an Images folder in the Outpiut folder (with appended version number) if TRUE
#'
#' @return A list of the paths to the created folders (for Outputs, Images and Pulse Files) is returned.
#' @export
#'
#' @examples listOfPaths <- fnStartFolders(workingDirectory = getwd(), verNum = "v1", createOutputFolder = TRUE, createPulseSubfolder = FALSE, createImagesFolder = TRUE)

################################################################################

# NOTE: The working directory is no longer determined here. Instead it is set in the original file along with the manually configured parameters.

fnStartFolders <- function(workingDirectory = NULL,
                           verNum = NULL,
                           createOutputFolder = NULL,
                           createPulseSubfolder = NULL,
                           createImagesFolder = NULL) {
  if (is.null(workingDirectory)) {
    stop(
      "fnStartFolders: Please specify the variable 'workingDirectory' to set the working directory."
    )
  } else if (is.null(verNum)) {
    stop(
      "fnStartFolders: Please specify the variable 'verNum' - a version number for file naming."
    )
  } else if (is.null(createOutputFolder) |
             is.null(createImagesFolder) |
             is.null(createPulseSubfolder)) {
    warning(
      "fnStartFolders: Please specify the booleans createOutputFolder, createImagesFolder and createPulseSubfolder to determine which folders should be created. By default, folders will be created for any undefined booleans."
    )
  }

  listOfPaths <- list()

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
    listOfPaths$pathOutput <-
      file.path(glue('{workingDirectory}\\Output ({verNum})'))
  }

  if (createImagesFolder != FALSE) {
    lapply(c(glue('Output ({verNum})\\Images')), function(x)
      if (!dir.exists(x))
        dir.create(
          x,
          showWarnings = TRUE,
          recursive = FALSE,
          mode = "0777"
        ))
    listOfPaths$pathImages <-
      file.path(glue('{workingDirectory}\\Output ({verNum})\\Images'))
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
    listOfPaths$pathOutputPulse <-
      file.path(glue('{workingDirectory}\\Output ({verNum})\\Pulse'))
  }
  # Return the list of paths at the end
  return(listOfPaths)
} # END

# Old method below

# # Create folders, if they don't exist
# # https://stackoverflow.com/questions/40714190/use-apply-functions-dir-exist-and-dir-create
# if (createOutputFolder != FALSE) {
#   lapply(c(glue('Output ({verNum})')), function(x)
#     if (!dir.exists(x))
#       dir.create(
#         x,
#         showWarnings = TRUE,
#         recursive = FALSE,
#         mode = "0777"
#       ))
#   folderOutput <<-
#     file.path(glue('{workingDirectory}\\Output ({verNum})'))
# }
#
# if (createImagesFolder != FALSE) {
#   lapply(c(glue('Images ({verNum})')), function(x)
#     if (!dir.exists(x))
#       dir.create(
#         x,
#         showWarnings = TRUE,
#         recursive = FALSE,
#         mode = "0777"
#       ))
#   folderImages <<-
#     file.path(glue('{workingDirectory}\\Images ({verNum})'))
# }
#
# if (createPulseSubfolder != FALSE) {
#   lapply(c(glue('Output ({verNum})\\Pulse')), function(x)
#     if (!dir.exists(x))
#       dir.create(
#         x,
#         showWarnings = TRUE,
#         recursive = FALSE,
#         mode = "0777"
#       ))
#   folderOutputPulse <<-
#     file.path(glue('{workingDirectory}\\Output ({verNum})\\Pulse'))
# }
# } # END
