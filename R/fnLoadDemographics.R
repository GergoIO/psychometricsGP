#' Function to load demographics file
#'
#' @description Load in the demographics file. If the demographics file is in the shared folder and VPN is not configured, the demographics file is sometimes inaccessible. When the VPN is functional but the shared folder has not yet been opened in the session, R cannot load the demographics file. This functions tries to open the root shared folder if the demographics file is inaccessible. If, after opening the shared folder, the demographics file is still inaccessible a stop error is thrown. Otherwise, messages relay the stage at which the demographics file was loaded (the shared folder is not opened if the demographics file is immediately accessible.)
#'
#' @param pathDemographics Full path to the demographics file (eg "P:/2223/Demographics/2223 Demographics.xlsx")
#' @param pathSharedFolder Root path to shared folder (defaults to "P:")
#'
#' @return Returns the demographics file if it could be loaded or an error message otherwise
#' @export
#'
#' @examples .dfDemogOrig <- fnLoadDemographics(pathDemographics = "P:/2223/Demographics/2223 Demographics.xlsx", pathSharedFolder = "P:")
#'
fnLoadDemographics <-
  function(pathDemographics, pathSharedFolder = "P:") {
    # Input validation
    if (!exists("pathDemographics")){
      stop("pathDemographics not defined.")
    }
    if (!exists("pathSharedFolder")){
      stop("pathSharedFolder not defined.")
    }
    if (!is.character(pathDemographics)){
      stop("pathDemographics must be a character string.")
    }
    if (!is.character(pathSharedFolder)){
      stop("pathSharedFolder must be a character string.")
    }

    # Check if demographics file originates from shared folder
    if (grepl(pathSharedFolder, pathDemographics)) {
      # Demographics file originates from shared folder
      message("Reading demographics file from shared folder")
      # Check if shared folder is accessible
      tryCatch({
        # If shared folder accessible, read in demographics file
        .dfDemogOrig <-
          read_excel(pathDemographics)
        message("Demographics file successfully loaded, first try")
      }, error = function(e) {
        # If shared folder not accessible, try opening the shared folder
        # (If VPN is connected but shared folder has not been opened in session, folder will show as inaccessible. Opening folder makes it accessible)
        message("Demographics file path inaccessible, trying to open shared folder")
        utils::browseURL(pathSharedFolder)
        # Try to read the demographics file again
        tryCatch({
          .dfDemogOrig <-
            read_excel(pathDemographics)
          message("Demographics file successfully loaded, after opening shared folder")
        }, error = function(e2) {
          # Demographics file still inaccessible even after trying to open shared folder.
          # Likely that VPN is not working
          stop(
            glue(
              "Demographics file still inaccessible. Make sure path {pathDemographics} points to an accessible demographics file and try again."
            )
          )
        })
      })
    } else {
      # Demographics file does not originate from shared folder
      message("Reading demographics file from outside shared folder")
      .dfDemogOrig <-
        read_excel(pathDemographics)
    }
    return(.dfDemogOrig)
  }
