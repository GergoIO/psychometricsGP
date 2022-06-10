#' Run functions to install and load required packages + set and save folders
#'
#' @return Required packages are installed and loaded if they are not already.
#' The working directory is set to the location of the script file and save as variable folderWD.
#' Two folders for figures and other outputs are also created and saved as folderOutput and folderPlots
#' @export
#'
#' @examples fnStartup()
fnStartup <- function() {
#   ____________________________________________________________________________
#   Install Packages                                                        ####
# Run package installer/loader once only

  # Install psychometricsTNG packages from private Github
  devtools::install_github("GergoIO/psychometricsTNG",
                           auth_token = "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",
                           quiet = TRUE) # So no warning if skipping
  devtools::install_github("GergoIO/psychometricsGP",
                           auth_token = "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",
                           quiet = TRUE) # So no warning if skipping
  # psychometric package no longer on CRAN so hosting it myself
  devtools::install_github("GergoIO/psychometric",
                           auth_token = "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",
                           quiet = TRUE)

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
    psychometricsGP,
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
}
