#' Save a plot
#'
#' @param controls A list containing at least the defined (TRUE/FALSE) variables 'stopAllOutputs' and 'stopImageSaving'. If either of the variables are TRUE, this function will not save an image. Otherwise, if both variables are FALSE, an image will be saved. If 'controls' is not defined, the default behaviour is to proceed and save an image if possible. The user will be notified in this instance.
#' @param plot The variable name of the plot
#' @param dimensions A vector containing the width, height and units of the dimensions of the saved image
#' @param filePath The file path where the plot is saved
#'
#' @return Nothing is explicitly returned, a plot is saved
#' @export
#'
#' @examples fnPltSave(controls = list(stopAllOutputs = TRUE, stopImageSaving = FALSE), plot = plt1, filePath = glue('{some path}\\filename.jpg'))

################################################################################

fnPltSave <-
  function(controls = NULL,
           plot = NULL,
           filePath = NULL) {
    if (is.null(plot) == TRUE |
        is.null(filePath) == TRUE | is.null(dimensions) == TRUE)    {
      stop("One of the required variables for this function has not been specified.")
    } else if (is.null(controls) == TRUE) {
      message("fnPltSave: The controls variable has not been set. Proceeding with default values")
      controls = list()
      controls$stopAllOutputs <- FALSE
      controls$stopImageSaving <- FALSE
    }
    else if (is.null(controls$stopAllOutputs) == TRUE) {
      message(
        "fnPltSave: The control list does not contain a 'stopAllOutputs' variable. Proceeding with the default of false"
      )
      conrols$stopAllOutputs <- FALSE
    }
    else if (is.null(controls$stopImageSaving) == TRUE) {
      message(
        "fnPltSave: The control list does not contain a 'stopAllOutputs' variable. Proceeding with the default of false"
      )
      conrols$stopImageSaving <- FALSE
    }
    else if (controls$stopAllOutputs == FALSE &&
             controls$stopImageSaving == FALSE) {
      ggsave(
        plot,
        file = filePath,
        width = as.numeric(dimensions[1]),
        height = as.numeric(dimensions[2]),
        units = c(dimensions[3])
      )
    }
  }
