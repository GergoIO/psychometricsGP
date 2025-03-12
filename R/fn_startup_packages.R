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
  # Before doing anything, load this specific dll file to fix a no text issue with file.choose
  # See here https://github.com/r-lib/textshaping/issues/36
  # Check if the file exists and try to load it
  dll_path <- "C:\\Windows\\System32\\TextShaping.dll"
  if (file.exists(dll_path)) {
    tryCatch({
      dyn.load(dll_path)
      # message("TextShaping.dll loaded successfully.")
    }, error = function(e) {
      message("See fn_startup_packages: Failed to load TextShaping.dll: ",
              e$message)
    })
  } else {
    message("See fn_startup_packages: TextShaping.dll not found at the specified path.")
  }



  if (!requireNamespace("librarian", quietly = TRUE)) {
    install.packages("librarian", dependencies = TRUE)
  }
  library(librarian)

  if (!requireNamespace("librarian", quietly = TRUE)) {
    install.packages("librarian", dependencies = TRUE)
  }
  library(conflicted)

  # Set specific function preferences
  conflicts_prefer(
    psychometric::alpha,
    reshape2::melt,
    lme4::fixef,
    patchwork::area,
    psych::rescale,
    tidyr::smiths,
    .quiet = TRUE
  )

  # Prefer all dplyr fns
  conflict_prefer_all("dplyr", quiet = TRUE)

  # Suppress dplyr summarise messages
  options(dplyr.summarise.inform = FALSE)

  # Set further preferences
  conflict_prefer_all("ggplot2", "reshape2", quiet = TRUE)
  conflict_prefer_all("lme4", "mirt", quiet = TRUE)

  # Install and load required packages using librarian
  librarian::shelf(
    Amelia,
    beepr,
    # beep sound
    conflicted,
    # allow setting a default fns from a package
    english,
    emmeans,
    flextable,
    forcats,
    gdata,
    ggplot2,
    ggnewscale,
    #for new_scale
    ggThemeAssist,
    ggprism,
    ggtext,
    # For minor ticks
    # glue,
    hrbrthemes,
    janitor,
    ltm,
    lme4,
    minpack.lm,
    # For nlsLM fn
    mirt,
    officer,
    openxlsx,
    patchwork,
    # For minor ticks https://cran.r-project.org/web/packages/ggprism/vignettes/axes.html
    pipebind,
    # https://github.com/bwiernik/pipebind
    plyr,
    psych,
    # For CAPT PtBis
    psychometric,
    purrr,
    randomcoloR,
    # for Clinical Area colouring
    ragg,
    # potentially threw errors when adding plots to reports without it (flextable dependency?)
    RColorBrewer,
    reshape,
    readxl,
    reshape2,
    rlang,
    cvarrichio / rowr,
    rstudioapi,
    scales,
    # for col_numeric
    spatstat,
    spatstat.linnet,
    lorenzwalthert / strcode,
    stringi,
    tcltk,
    # For getting prepopulated file path popups
    tictoc,
    tjmahr / WrapRmd,
    usethis,
    viridis,
    writexl,
    # For saving xlsx files
    tidyverse / glue,
    tidyverse # tidyverse should load dplyr
    # dplyr # Need to load dplyr LAST, plyr and ggplot2 have to be BEFORE IT
  )

  invisible(TRUE)
}
#   # Suppress dplyr summarise messages
#   options(dplyr.summarise.inform = FALSE)
#
#   # Use the psychometric package for alpha
#   # Use the reshape2 package for melt
#   conflicts_prefer(psychometric::alpha,
#                    reshape2::melt,
#                    .quiet = TRUE)
#
#   # Prefer all dplyr fns
#   conflict_prefer_all("dplyr", quiet = TRUE)
#
#   invisible(TRUE)
# }
