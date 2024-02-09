#' LEGACY FUNCTION - NOW SPLIT TO fnStartPackages and fnStartFolders Run functions to install and load required packages + set and save folders
#'
#' @param githubToken The authorization token needed to access the relevant github repos
#' @param verNum Set a version number - appended to save folders and file names
#' @param createOutputFolder (TRUE/FALSE/NULL) Create an outputs folder in the working directory (with appended version number) if TRUE
#' @param createPulseFolder (TRUE/FALSE/NULL) Create a folder called Pulse in the Output folder if TRUE
#' @param createImagesFolder (TRUE/FALSE/NULL) Create an Images folder in the working directory (with appended version number) if TRUE
#' #'
#' @return Required packages are installed and loaded if they are not already.
#' The working directory is set to the location of the script file and save as variable folderWD.
#' Two folders for figures and other outputs are also created and saved as folderOutput and folderPlots
#' @export
#'
#' @examples fnStartup()

# Example of IDS usage
# if (exists("hasRun") == FALSE) {
#   fnStartup(
#     githubToken = .githubToken,
#     verNum = cnst$verNum,
#     createOutputFolder = TRUE,
#     createPulseSubfolder = TRUE,
#     createImagesFolder = TRUE
#   ) # Run startup function once per session
#   hasRun <- TRUE
# }

################################################################################
### DEPRACATED #################################################################
################################################################################

################################################################################
### DEPRACATED #################################################################
################################################################################

################################################################################
### DEPRACATED #################################################################
################################################################################

fnStartup <- function(githubToken = NULL,
                      verNum = NULL,
                      createOutputFolder = NULL,
                      createPulseSubfolder = NULL,
                      createImagesFolder = NULL) {

  #   ____________________________________________________________________________
  #   Install Packages                                                        ####

  if (is.null(githubToken) |
      is.null(verNum)) {
    stop(
      "Please specify the github personal access token for file retrieval and a version number for file naming."
    )
  } else if (is.null(createOutputFolder) |
             is.null(createImagesFolder) |
             is.null(createPulseSubfolder)) {
    warning(
      "Please specify the booleans createOutputFolder, createImagesFolder and createPulseSubfolder to determine which folders should be created. No folders will be created for any undefined booleans."
    )
  } else {
    # Install psychometricsTNG packages from private Github
    devtools::install_github("GergoIO/psychometricsTNG",
                             auth_token = githubToken,
                             quiet = TRUE) # So no warning if skipping
    # psychometric package no longer on CRAN so hosting it myself
    devtools::install_github("GergoIO/psychometric",
                             auth_token = githubToken,
                             quiet = TRUE)
  }
  if (!require("librarian")) {
    install.packages("librarian", dependencies = TRUE)
  }

  library("librarian")
  librarian::shelf(
    Amelia,
    dplyr,
    english,
    flextable,
    forcats,
    ggplot2,
    ggThemeAssist,
    ggtext, # for element_textbox
    # glue,
    tidyverse / glue,
    hrbrthemes,
    ltm,
    lme4,
    officer,
    openxlsx,
    plyr,
    psychometric,
    # psychometricsGP, # No longer needed since this is loaded prior to this function running
    # psychometricsTNG,
    # reshape,
    readxl,
    reshape2,
    rlang,
    cvarrichio / rowr,
    rstudioapi,
    spatstat,
    lorenzwalthert / strcode,
    stringi,
    tidyverse,
    viridis,
    tjmahr / WrapRmd
  )

  #   ____________________________________________________________________________
  #   Working Directory and File Structuring                                  ####

  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  folderWD <<- getwd()

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
    folderOutput <<- file.path(glue('{getwd()}\\Output ({verNum})'))
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
    folderImages <<- file.path(glue('{getwd()}\\Images ({verNum})'))
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
      file.path(glue('{getwd()}\\Output ({verNum})\\Pulse'))
  }
} # END
