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
#'
fnReload <- function() {
  # Detach the package first
  detach("package:psychometricsGP", unload = TRUE)
  # Reload the package from the online repo
  devtools::install_github("GergoIO/psychometricsGP",
                           # Hidden variable from the secrets file
                           auth_token = .githubToken,
                           quiet = TRUE)
  # Start using the custom package
  library(psychometricsGP)

  message("fnReload: Reloading complete")
}
