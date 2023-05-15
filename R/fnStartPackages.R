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
  if (is.null(githubToken)) {
    stop("Please specify the github personal access token for file retrieval.")
  } else {

    # Define the 'not in' operator
    `%!in%` <- Negate(`%in%`)

    # Install psychometricsTNG packages from private Github
    # devtools::install_github("GergoIO/psychometricsGPTNG",
    #                          auth_token = githubToken,
    #                          quiet = TRUE) # So no warning if skipping
    # # psychometric package no longer on CRAN so hosting it myself
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
      beepr, # beep sound
      flextable,
      forcats,
      ggplot2,
      ggThemeAssist,
      # glue,
      tidyverse / glue,
      hrbrthemes,
      janitor,
      ltm,
      lme4,
      mirt,
      officer,
      openxlsx,
      plyr,
      psychometric,
      # psychometricsGP, # No longer needed since this is loaded prior to this function running
      psychometricsTNG,
      purrr,
      # reshape,
      readxl,
      reshape2,
      rlang,
      cvarrichio / rowr,
      rstudioapi,
      spatstat,
      spatstat.linnet,
      lorenzwalthert / strcode,
      stringi,
      tictoc,
      tidyverse,
      viridis,
      tjmahr / WrapRmd,
      usethis,
      dplyr # Need to load dplyr LAST, plyr and ggplot2 have to be BEFORE IT
    )
    # Stop message when using dplyr - summarise (to do with grouping)
    # https://rstats-tips.net/2020/07/31/get-rid-of-info-of-dplyr-when-grouping-summarise-regrouping-output-by-species-override-with-groups-argument/
    options(dplyr.summarise.inform = FALSE)

  }
} # END

# ### SCORING
# detach(package:plyr)
# detach(package:dplyr)
# library(plyr)
# library(dplyr)
