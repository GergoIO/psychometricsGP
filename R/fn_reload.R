#' Reload the PsychometricsGP package
#'
#' @description Use this function to reload the current package (psychometricsGP). This function is useful when changes have been made and uploaded to the package repository. This function takes no arguments.
#'
#' @param repo The GitHub repository from which to install the package. Defaults to "GergoIO/psychometricsGP".
#'
#' @return Nothing is returned, a message alerts the user when reloading is complete.
#' @export
#'
#' @examples fn_reload()
fn_reload <- function(repo = "GergoIO/psychometricsGP") {
  message("fn_reload: Starting reloading, wait for message to confirm completion")

  has_beepr <- requireNamespace("beepr", quietly = TRUE)
  if (!has_beepr) {
    message("fn_reload: 'beepr' package not installed, please install it to enable sound notifications")
  } else {
    beepr::beep(10)
  }

  tryCatch({
    # Safely detach the package if loaded with error handling and namespace unloading
    if ("package:psychometricsGP" %in% search()) {
      try({
        detach("package:psychometricsGP", unload = TRUE, character.only = TRUE)
        message("fn_reload: Package 'psychometricsGP' detached")
      }, silent = FALSE)
    } else {
      message("fn_reload: Package 'psychometricsGP' not loaded, no need to detach")
    }

    # Reinstall the package from GitHub with dependencies to true for safety
    devtools::install_github(repo, quiet = TRUE, dependencies = TRUE)
    message("fn_reload: Package 'psychometricsGP' reinstalled from GitHub")

    # Load the package, but give a user warning in case loading fails
    if (!suppressWarnings(requireNamespace("psychometricsGP", quietly = TRUE))) {
      stop("fn_reload: Failed to load 'psychometricsGP' after installation.")
    }
    suppressPackageStartupMessages(library(psychometricsGP, quietly = TRUE))
    message("fn_reload: Package 'psychometricsGP' loaded successfully")

    message("fn_reload: Reloading complete")

    if (has_beepr) {
      beepr::beep(10)
    }

  }, warning = function(w) {
    message("fn_reload: Warning - ", w$message)
  }, error = function(e) {
    message("fn_reload: Error - ", e$message)
  })
}
#' #' fn_reload()
#' fn_reload <- function(repo = "GergoIO/psychometricsGP") {
#'   message("fn_reload: Starting reloading, wait for message to confirm completion")
#'
#'   # Attempt to make a system noise using beep
#'   if (!requireNamespace("beepr", quietly = TRUE)) {
#'     message("fn_reload: 'beepr' package not installed, please install it to enable sound notifications")
#'   } else {
#'     beepr::beep(10)
#'   }
#'
#'   tryCatch({
#'     # Detach the package if it is loaded
#'     if ("package:psychometricsGP" %in% search()) {
#'       detach("package:psychometricsGP", unload = TRUE, character.only = TRUE)
#'       message("fn_reload: Package 'psychometricsGP' detached")
#'     } else {
#'       message("fn_reload: Package 'psychometricsGP' not loaded, no need to detach")
#'     }
#'
#'     # Reinstall the package from GitHub
#'     devtools::install_github(repo, quiet = TRUE, dependencies = FALSE)
#'     message("fn_reload: Package 'psychometricsGP' reinstalled from GitHub")
#'
#'     # Load the package
#'     suppressWarnings(library(psychometricsGP, quietly = TRUE))
#'     message("fn_reload: Package 'psychometricsGP' loaded successfully")
#'
#'     # Notify the user of completion
#'     message("fn_reload: Reloading complete")
#'
#'     # Attempt to make a system noise using beep
#'     if (!requireNamespace("beepr", quietly = TRUE)) {
#'       message("fn_reload: 'beepr' package not installed, please install it to enable sound notifications")
#'     } else {
#'       beepr::beep(10)
#'     }
#'   }, warning = function(w) {
#'     message("fn_reload: Warning - ", w$message)
#'   }, error = function(e) {
#'     message("fn_reload: Error - ", e$message)
#'   })
#' }
