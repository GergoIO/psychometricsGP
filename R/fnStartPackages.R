#' Run functions to install and load required packages
#'
#' @param githubToken The authorization token needed to access the relevant github repos
#'
#' @return Required packages are installed and loaded if they are not already.
#' @export
#'
#' @examples fnStartPackages(githubToken = .githubToken)

################################################################################

fnStartPackages <- function(githubToken = NULL) {
  if (is.null(githubToken) == TRUE) {
    stop("Please specify the github personal access token for file retrieval.")
  } else {
    # Install psychometricsTNG packages from private Github
    devtools::install_github("GergoIO/psychometricsTNG",
                             auth_token = githubToken,
                             quiet = TRUE) # So no warning if skipping
    # psychometric package no longer on CRAN so hosting it myself
    devtools::install_github("GergoIO/psychometric",
                             auth_token = githubToken,
                             quiet = TRUE)
    if (!require("librarian")) {
      #Needed for loading the other packages
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
      tjmahr / WrapRmd,
      xlsx
    )
  }
} # END
