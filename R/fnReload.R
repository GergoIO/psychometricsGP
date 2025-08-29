#' Reload the PsychometricsGP package
#'
#' @description Use this function to reload the current package (psychometricsGP). This function is useful when changes have been made and uploaded to the package repository. This function takes no arguments. NOTE: the authorisation token must be defined under the hidden variable '.githubToken'
#'
#' @return Nothing is returned, a message alerts the user when reloading is complete
#' @export
#'
#' @examples fnReload()
#'
################################################################################

fnReload <- function() {
  message("fnReload: Starting reloading, wait for message to confirm completion")

  # Detach
  detach("package:psychometricsGP", unload = TRUE)
  # Reinstall
  devtools::install_github(
    "GergoIO/psychometricsGP",
    # auth_token = .githubToken,
    quiet = TRUE,
    dependencies = FALSE
  )
  # Load
  suppressWarnings(library(psychometricsGP, quietly = TRUE))

  # Message
  message("fnReload: Reloading complete")
  beep(10) # Make a system noise
}

# OLD
# fnReload <- function() {
#   tryCatch({
#     # Detach the package first
#     detach("package:psychometricsGP", unload = TRUE)
#     # Reload the package from the online repo
#     devtools::install_github("GergoIO/psychometricsGP",
#                              auth_token = .githubToken,
#                              quiet = TRUE)
#     # Start using the custom package
#     library(psychometricsGP)
#
#     message("fnReload: Reloading complete")
#   }, warning = function(w) {
#     message("fnReload: Warning - ", w$message)
#   }, error = function(e) {
#     message("fnReload: Error - ", e$message)
#   })
# }

# OLD - OLD
# fnReload <- function() {
#   # Detach the package first
#   detach("package:psychometricsGP", unload = TRUE)
#   # Reload the package from the online repo
#   devtools::install_github("GergoIO/psychometricsGP",
#                            # Hidden variable from the secrets file
#                            auth_token = .githubToken,
#                            quiet = TRUE)
#   # Start using the custom package
#   library(psychometricsGP)
#
#   message("fnReload: Reloading complete")
# }
