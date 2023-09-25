#' Create a Subfolder in the Outputs Folder
#'
#' @param folderName (String) Name of the subfolder to create (if it doesn't exist already)
#' @param versionNumber (String) Version number appended to the Output folder name
#' @param workingDirectory (Path) Define the working directory, where the Output folder is
#'
#' @return The path of the created subfolder
#' @export
#'
#' @examples pathSubfolder <- fnCreateSubfolder(folderName = "Historic", versionNumber = cnst$verNum, workingDirectory = lFiles$pathWorkingDir)
#'
################################################################################
#'
fnCreateSubfolder <- function(folderName = NULL,
                              versionNumber = NULL,
                              workingDirectory = NULL) {
  # Check that the folder name is not NULL
  if (is.null(folderName)) {
    stop(
      "fnCreateSubfolder: The 'folderName' variable must be specified to set the working directory."
    )
  }
  # Check that the working directory and version number are not NULL
  if (is.null(workingDirectory)) {
    stop(
      "fnCreateSubfolder: The 'workingDirectory' variable must be specified to set the working directory."
    )
  }
  if (is.null(versionNumber)) {
    stop("fnCreateSubfolder: The 'versionNumber' variable must be specified to name files.")
  }

  #   __________________________________________________________________________
  #   Output Folder                                                         ####
  # Create the output folder if it doesn't already exist
  # It goes in the working directory titled "Output (***)" where *** is the version number
  if (!dir.exists(file.path(workingDirectory,
                            paste0("Output (", versionNumber, ")")))) {
    dir.create(file.path(workingDirectory,
                         paste0("Output (", versionNumber, ")")),
               recursive = TRUE)
  }

  #   __________________________________________________________________________
  #   Subfolder                                                             ####
  # Create the desired subfolder in the output folder if it doesn't already exist
  if (!dir.exists(file.path(
    workingDirectory,
    paste0("Output (", versionNumber, ")"),
    folderName
  ))) {
    dir.create(file.path(
      workingDirectory,
      paste0("Output (", versionNumber, ")"),
      folderName
    ),
    recursive = TRUE)
  }

  #   __________________________________________________________________________
  #   Return Folder Path                                                    ####
  # Return the path of the created subfolder
  return(file.path(
    workingDirectory,
    paste0("Output (", versionNumber, ")"),
    folderName
  ))
}
