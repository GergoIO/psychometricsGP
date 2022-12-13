#' Create a Histogram Plot
#'
#' @param data The data to be plotted (usually a series of numbers - the series is converted to a dataframe as part of this function)
#' @param binWidth The bin width for plotting the histrogram
#' @param labelX The x-label of the accent
#' @param labelY The y-label of the accent
#'
#' @return A histrogram plot is returned
#' @export
#'
#' @examples
#'
#' dataTest <- c(1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4)
#' fnPltHistogram(data = dataTest, binWidth = 0.5, labelX = "X Axis", labelY = "Y Axis")

################################################################################

fnPltHistogram <-
  function(data = NULL,
           binWidth = NULL,
           labelX = NULL,
           labelY = NULL) {
    if (is.null(data) |
        is.null(binWidth) |
        is.null(labelX) |
        is.null(labelY))    {
      stop("One of the required variables for this function has not been specified.")
    } else {
      plot <- ggplot(data.frame(data),
                     aes(x = data)) +
        geom_histogram(colour = "black",
                       fill = "#990033",
                       binwidth = binWidth) +
        xlab(labelX) +
        ylab(labelY) +
        theme_psmd()
    }
  } # END
