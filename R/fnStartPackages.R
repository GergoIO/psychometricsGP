#' Run functions to install and load required packages
#'
#' @return Required packages are installed and loaded if they are not already.
#' @export
#'
#' @examples fnStartPackages()

################################################################################

fnStartPackages <- function() {
    if (!require("librarian")) {
      #Needed for loading the other packages
      install.packages("librarian", dependencies = TRUE)
    }
    library("librarian")
    librarian::shelf(
      Amelia,
      beepr, # beep sound
      english,
      flextable,
      forcats,
      ggplot2,
      ggThemeAssist,
      ggprism, # For minor ticks
      # glue,
      hrbrthemes,
      janitor,
      ltm,
      lme4,
      mirt,
      officer,
      openxlsx,
      patchwork, # For minor ticks https://cran.r-project.org/web/packages/ggprism/vignettes/axes.html
      pipebind, # https://github.com/bwiernik/pipebind
      plyr,
      psych, # For CAPT PtBis
      psychometric,
      purrr,
      randomcoloR, # for Clinical Area colouring
      ragg, # potentially threw errors when adding plots to reports without it (flextable dependency?)
      # reshape,
      readxl,
      reshape2,
      rlang,
      cvarrichio / rowr,
      rstudioapi,
      scales, # for col_numeric
      spatstat,
      spatstat.linnet,
      lorenzwalthert / strcode,
      stringi,
      tictoc,
      viridis,
      tjmahr / WrapRmd,
      usethis,
      tidyverse/glue,
      tidyverse # tidyverse should load dplyr
      # dplyr # Need to load dplyr LAST, plyr and ggplot2 have to be BEFORE IT
    )
    # Stop message when using dplyr - summarise (to do with grouping)
    # https://rstats-tips.net/2020/07/31/get-rid-of-info-of-dplyr-when-grouping-summarise-regrouping-output-by-species-override-with-groups-argument/
    options(dplyr.summarise.inform = FALSE)
  }

# ### SCORING
# detach(package:plyr)
# detach(package:dplyr)
# library(plyr)
# library(dplyr)
