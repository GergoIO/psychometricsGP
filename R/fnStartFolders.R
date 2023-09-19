#' Create Output, Pulse and Images folders.
#'
#' @param workingDirectory Define the working directory, where save folders will be created
#' @param versionNumber Set a version number - appended to save folders and file names
#' @param createOutputFolder (TRUE/FALSE/NULL) Create an outputs folder in the working directory (with appended version number) if TRUE
#' @param createPulseFolder (TRUE/FALSE/NULL) Create a folder called Pulse in the Output folder if TRUE
#' @param createImagesFolder (TRUE/FALSE/NULL) Create an Images folder in the Output folder if TRUE
#'
#' @return A list of the paths to the created folders (for Outputs, Images and Pulse Files) is returned.
#' @export
#'
#' @examples listOfPaths <- fnStartFolders(workingDirectory = lFiles$pathWorkingDir #or getwd(), versionNumber = "v1", createOutputFolder = TRUE, createPulseSubfolder = FALSE, createImagesFolder = TRUE)

################################################################################

# NOTE: The working directory is no longer determined here. Instead it is set in the original file along with the manually configured parameters.

# This function creates the output, pulse and images folders under a specified working directory
# and version number. If the directory already exists, it will not create it again.
# Returns a list of paths to the created directories.
fnStartFolders <- function(workingDirectory = NULL,
                           versionNumber = NULL,
                           createOutputFolder = TRUE,
                           createPulseSubfolder = TRUE,
                           createImagesFolder = TRUE) {
  # Check that the working directory and version number are not NULL
  if (is.null(workingDirectory)) {
    stop(
      "fnStartFolders: The 'workingDirectory' variable must be specified to set the working directory."
    )
  }
  if (is.null(versionNumber)) {
    stop("fnStartFolders: The 'versionNumber' variable must be specified to name files.")
  }

  # Check that createOutputFolder, createPulseSubfolder, and createImagesFolder are all booleans
  if (!is.logical(createOutputFolder) ||
      !is.logical(createPulseSubfolder) ||
      !is.logical(createImagesFolder)) {
    stop(
      "fnStartFolders: The 'createOutputFolder', 'createPulseSubfolder', and 'create_images_folder' variables must be booleans."
    )
  }

  # Create the output, pulse, and images folders if they do not exist
  # and createOutputFolder, createPulseSubfolder, and createImagesFolder are TRUE

  #   __________________________________________________________________________
  #   Output Folder                                                         ####
  if (createOutputFolder &&
      !dir.exists(file.path(workingDirectory,
                            paste0("Output (", versionNumber, ")")))) {
    dir.create(file.path(workingDirectory,
                         paste0("Output (", versionNumber, ")")),
               recursive = TRUE)

  }
  #   __________________________________________________________________________
  #   Pulse Folder                                                          ####
  if (createPulseSubfolder &&
      !dir.exists(file.path(
        workingDirectory,
        paste0("Output (", versionNumber, ")"),
        "Pulse"
      ))) {
    dir.create(file.path(
      workingDirectory,
      paste0("Output (", versionNumber, ")"),
      "Pulse"
    ),
    recursive = TRUE)
  }
  #   __________________________________________________________________________
  #   Images Folder                                                         ####
  if (createImagesFolder &&
      !dir.exists(file.path(
        workingDirectory,
        paste0("Output (", versionNumber, ")"),
        "Images"
      ))) {
    dir.create(file.path(
      workingDirectory,
      paste0("Output (", versionNumber, ")"),
      "Images"
    ),
    recursive = TRUE)
  }

  #   __________________________________________________________________________
  #   Return Folder Paths                                                   ####

  # Save the file paths to a list
  listOfPaths <- setNames(
    list(
      file.path(workingDirectory, paste0("Output (", versionNumber, ")")),
      file.path(
        workingDirectory,
        paste0("Output (", versionNumber, ")"),
        "Pulse"
      ),
      file.path(
        workingDirectory,
        paste0("Output (", versionNumber, ")"),
        "Images"
      )
    ),
    c("pathOutput", "pathPulse", "pathImages")
  )

  # Return a list of paths to the created directories
  return(listOfPaths)
}

# fnStartFolders <- function(workingDirectory = NULL,
#                            verNum = NULL,
#                            createOutputFolder = NULL,
#                            createPulseSubfolder = NULL,
#                            createImagesFolder = NULL) {
#   if (is.null(workingDirectory)) {
#     stop(
#       "fnStartFolders: Please specify the variable 'workingDirectory' to set the working directory."
#     )
#   } else if (is.null(verNum)) {
#     stop(
#       "fnStartFolders: Please specify the variable 'verNum' - a version number for file naming."
#     )
#   } else if (is.null(createOutputFolder) |
#              is.null(createImagesFolder) |
#              is.null(createPulseSubfolder)) {
#     warning(
#       "fnStartFolders: Please specify the booleans createOutputFolder, createImagesFolder and createPulseSubfolder to determine which folders should be created. By default, folders will be created for any undefined booleans."
#     )
#   }
#
#   listOfPaths <- list()
#
#   # Create folders, if they don't exist
#   # https://stackoverflow.com/questions/40714190/use-apply-functions-dir-exist-and-dir-create
#   if (createOutputFolder != FALSE) {
#     lapply(c(glue('Output ({verNum})')), function(x)
#       if (!dir.exists(x))
#         dir.create(
#           x,
#           showWarnings = TRUE,
#           recursive = FALSE,
#           mode = "0777"
#         ))
#     listOfPaths$pathOutput <-
#       file.path(glue('{workingDirectory}\\Output ({verNum})'))
#   }
#
#   if (createImagesFolder != FALSE) {
#     lapply(c(glue('Output ({verNum})\\Images')), function(x)
#       if (!dir.exists(x))
#         dir.create(
#           x,
#           showWarnings = TRUE,
#           recursive = FALSE,
#           mode = "0777"
#         ))
#     listOfPaths$pathImages <-
#       file.path(glue('{workingDirectory}\\Output ({verNum})\\Images'))
#   }
#
#   if (createPulseSubfolder != FALSE) {
#     lapply(c(glue('Output ({verNum})\\Pulse')), function(x)
#       if (!dir.exists(x))
#         dir.create(
#           x,
#           showWarnings = TRUE,
#           recursive = FALSE,
#           mode = "0777"
#         ))
#     listOfPaths$pathOutputPulse <-
#       file.path(glue('{workingDirectory}\\Output ({verNum})\\Pulse'))
#   }
#   # Return the list of paths at the end
#   return(listOfPaths)
# } # END

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
