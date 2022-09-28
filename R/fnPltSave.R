#' Save a plot
#'
#' @param savePlot (Optional) A boolean (TRUE/FALSE) determines whether the plot will be saved. Useful for controlling overall save behaviour.
#' @param plot The variable name of the plot to save - this must exist in your environment
#' @param dimensions (Optional) A vector containing the width, height and units of the dimensions of the saved image (in that order). If not set, defaults to 15cm x 15cm
#' @param filePath The file path where the plot is saved
#'
#' @return Nothing is explicitly returned, a plot is saved
#' @export
#'
#' @examples fnPltSave(savePlot = TRUE, plot = plt1, dimensions = c(15, 15, "cm"), filePath = glue('{some path}\\filename.jpg'))

################################################################################

fnPltSave <-
  function(savePlot = NULL,
           plot = NULL,
           dimensions = c(15, 15, "cm"),
           filePath = NULL) {
    if (is.null(plot) == TRUE |
        is.null(filePath) == TRUE) {
      stop("One of the required variables for this function has not been specified.")
    } else if (is.null(savePlot) == TRUE) {
      message("fnPltSave: The 'savePlot' variable has not been set. Defaulting to saving the plot.")
      savePlot <- TRUE
    } else if (is.null(filePath) == TRUE){
      message("fnPltSave: The 'filePath' variable has not been set. Defaulting to not saving any plots")
      savePlot <- FALSE
    }
    else if (savePlot == TRUE) {
      ggsave(
        plot,
        file = filePath,
        width = as.numeric(dimensions[1]),
        height = as.numeric(dimensions[2]),
        units = c(dimensions[3])
      )
    }
  }

# OLD version
# fnPltSave <-
#   function(controls = NULL,
#            plot = NULL,
#            dimensions = NULL,
#            filePath = NULL) {
#     if (is.null(plot) == TRUE |
#         is.null(filePath) == TRUE | is.null(dimensions) == TRUE)    {
#       stop("One of the required variables for this function has not been specified.")
#     } else if (is.null(controls) == TRUE) {
#       message("fnPltSave: The controls variable has not been set. Proceeding with default values")
#       controls = list()
#       controls$stopAllOutputs <- FALSE
#       controls$stopImageSaving <- FALSE
#     }
#     else if (is.null(controls$stopAllOutputs) == TRUE) {
#       message(
#         "fnPltSave: The control list does not contain a 'stopAllOutputs' variable. Proceeding with the default of FALSE"
#       )
#       conrols$stopAllOutputs <- FALSE
#     }
#     else if (is.null(controls$stopImageSaving) == TRUE) {
#       message(
#         "fnPltSave: The control list does not contain a 'stopAllOutputs' variable. Proceeding with the default of FALSE"
#       )
#       conrols$stopImageSaving <- FALSE
#     }
#     else if (controls$stopAllOutputs == FALSE &&
#              controls$stopImageSaving == FALSE) {
#       ggsave(
#         plot,
#         file = filePath,
#         width = as.numeric(dimensions[1]),
#         height = as.numeric(dimensions[2]),
#         units = c(dimensions[3])
#       )
#     }
#   }
