#' Install and Load Required Packages
#'
#' This function installs (if not already installed) and loads the necessary packages for the project.
#'
#' @return Invisibly returns TRUE if all packages are successfully loaded.
#' @export
#'
#' @examples
#' fn_startup_packages()
fn_startup_packages <- function() {
  # Install and load librarian if not already installed
  if (!requireNamespace("librarian", quietly = TRUE)) {
    install.packages("librarian", dependencies = TRUE)
  }
  library(librarian)

  # Install and load required packages using librarian
  librarian::shelf(
    Amelia,
    beepr, # beep sound
    conflicted, # allow setting a default fns from a package
    english,
    emmeans,
    flextable,
    forcats,
    gdata,
    ggplot2,
    ggThemeAssist,
    ggprism, # For minor ticks
    # glue,
    hrbrthemes,
    janitor,
    ltm,
    lme4,
    minpack.lm, # For nlsLM fn
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
    reshape,
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

  # Suppress dplyr summarise messages
  options(dplyr.summarise.inform = FALSE)

  # Use the psychometric package for alpha
  # Use the reshape2 package for melt
  conflicts_prefer(psychometric::alpha,
                   reshape2::melt,
                   .quiet = TRUE)

  # Prefer all dplyr fns
  conflict_prefer_all("dplyr", quiet = FALSE)

  invisible(TRUE)
}

