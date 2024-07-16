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
  packages <- c(
    "Amelia",
    "beepr",
    "english",
    "flextable",
    "forcats",
    "gdata",
    "ggplot2",
    "ggThemeAssist",
    "ggprism",
    "hrbrthemes",
    "janitor",
    "ltm",
    "lme4",
    "minpack.lm",
    "mirt",
    "officer",
    "openxlsx",
    "patchwork",
    "pipebind",
    "plyr",
    "psych",
    "psychometric",
    "purrr",
    "randomcoloR",
    "ragg",
    "reshape",
    "readxl",
    "reshape2",
    "rlang",
    "rowr",
    "rstudioapi",
    "scales",
    "spatstat",
    "spatstat.linnet",
    "strcode",
    "stringi",
    "tictoc",
    "viridis",
    "WrapRmd",
    "usethis",
    "glue",
    "tidyverse"
  )

  # Install and load librarian if not already installed
  if (!requireNamespace("librarian", quietly = TRUE)) {
    install.packages("librarian", dependencies = TRUE)
  }
  library(librarian)

  # Install and load required packages using librarian
  librarian::shelf(packages)

  # Suppress dplyr summarise messages
  options(dplyr.summarise.inform = FALSE)

  invisible(TRUE)
}
