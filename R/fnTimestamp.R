#' Return a custom formatted timestamp
#'
#' @return Returns a timestamp string
#' @export
#'
#' @examples fnTimestamp()

################################################################################

fnTimestamp <- function()  {
  strftime(Sys.time(), "%Y%m%d_%H%M%S")
}
