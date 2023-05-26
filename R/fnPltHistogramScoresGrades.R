#' fnPltHistogramScoresGradeFreq
#'
#' @param data Dataframe containing the score and grade cols
#' @param colScore A string - name of the score col
#' @param colGrade A string - name of the grade col
#' @param scoreMax Numeric - The maximum possible score
#' @param scorePass Numeric - The pass score (added to plot with a line and annotation)
#'
#' @return The plot is returned
#' @export
#'
#' @examples fnPltHistogramScoresGradeFreq(  data = dfRes,  colScore = "Score", colGrade = "Outcome",  scoreMax = scoreMax,  scorePass = passMark)
#'
################################################################################
#'
fnPltHistogramScoresGradeFreq <- function(data = NULL,
                                          colScore = NULL,
                                          colGrade = NULL,
                                          scoreMax = NULL,
                                          scorePass = NULL) {
  #   ________________________________________________________________________
  #   Check all required variables are defined                            ####

  if (is.null(data) |
      is.null(colScore) |
      is.null(colGrade) |
      is.null(scoreMax) |
      is.null(scorePass))
  {
    stop("One of the required variables for this function has not been specified.")
  } else {
    plot <-
      ggplot(dfRes, aes(x = .data[[colScore]], fill = .data[[colGrade]])) +
      geom_histogram(colour = "black", binwidth = 1) +
      xlab("Score") +
      ylab("Frequency") +
      scale_fill_manual(values = fnGPColours("PF"), guide = guide_legend(reverse = TRUE)) +
      geom_vline(xintercept = ceiling(scorePass) - 0.5,
                 linetype = "dashed") +
      annotate(
        geom = "text",
        x = ceiling(scorePass) + 0.1,
        y = mean(data[[colScore]]),
        label = "Pass Boundary",
        angle = 90,
        hjust = 0,
        fontface = 2
      ) +
      scale_y_continuous(expand = expansion(add = c(0, 1))) +
      scale_x_continuous(limits = c(-1, scoreMax + 1),
                         expand = expansion(add = c(-1, -0.4))) +
      theme(aspect.ratio = 2) +
      themeGP()
    # Return the plot
    return(plot)
  }
}
