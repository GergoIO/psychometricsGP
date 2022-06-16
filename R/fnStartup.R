#' Run functions to install and load required packages + set and save folders
#'
#' @param githubToken The authorization token needed to access the relevant github repos
#' @param verNum Set a version number - appended to save folders and file names
#' @param createPulseFolder (TRUE/FALSE/NULL) Create a folder called Pulse in the Output folder if TRUE
#' #'
#' @return Required packages are installed and loaded if they are not already.
#' The working directory is set to the location of the script file and save as variable folderWD.
#' Two folders for figures and other outputs are also created and saved as folderOutput and folderPlots
#' @export
#'
#' @examples fnStartup()

################################################################################

fnStartup <- function(githubToken = NULL,
                      verNum = NULL, createPulseFolder = NULL) {
  #   ____________________________________________________________________________
  #   Install Packages                                                        ####

  if (is.null(githubToken) == TRUE | is.null(verNum) == TRUE) {
    stop(
      "Please specify the github personal access token for file retrieval and a version number for file naming."
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
    flextable,
    forcats,
    ggplot2,
    ggThemeAssist,
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
    psychometricsTNG,
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

  # Create folders, if they don't exist https://stackoverflow.com/questions/40714190/use-apply-functions-dir-exist-and-dir-create
  lapply(c(glue('Output ({verNum})'), glue('Figures ({verNum})')), function(x)
    if (!dir.exists(x))
      dir.create(
        x,
        showWarnings = TRUE,
        recursive = FALSE,
        mode = "0777"
      ))

  folderOutput <<- file.path(glue('{getwd()}\\Output ({verNum})'))
  folderPlots <<- file.path(glue('{getwd()}\\Figures ({verNum})'))

  if(createPulseFolder == TRUE){
    lapply(c(glue('Output ({verNum})\\Pulse')), function(x)
      if (!dir.exists(x))
        dir.create(
          x,
          showWarnings = TRUE,
          recursive = FALSE,
          mode = "0777"
        ))
  folderOutputPulse <<- file.path(glue('{getwd()}\\Output ({verNum})\\Pulse'))
  }
} # END
