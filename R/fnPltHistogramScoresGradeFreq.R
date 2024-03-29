#' fnPltHistogramScoresGradeFreq
#'
#' @param data Dataframe containing the score and grade cols
#' @param gradeScheme PF/PFE are the only two supported currently
#' @param colScore A string - name of the score col
#' @param colGrade A string - name of the grade col
#' @param scoreMax Numeric - The maximum possible score
#' @param scorePass Numeric - The pass score (added to plot with a line and annotation)
#' @param scoreExcellent (OPTIONAL) Numeric - The score for excellent (added to plot with a line and annotation)
#'
#' @return The plot is returned
#' @export
#'
#' @examples fnPltHistogramScoresGradeFreq(data = dfRes, gradeScheme = "PF", colScore = "Score", colGrade = "Outcome",  scoreMax = scoreMax,  scorePass = passMark)
#'
################################################################################
#'
fnPltHistogramScoresGradeFreq <- function(data = NULL,
                                           gradeScheme = NULL,
                                           colScore = NULL,
                                           colGrade = NULL,
                                           scoreMax = NULL,
                                           scorePass = NULL,
                                           scoreExcellent = NULL) {
  if (is.null(data) ||
      is.null(gradeScheme) ||
      is.null(colScore) ||
      is.null(colGrade) ||
      is.null(scoreMax) ||
      is.null(scorePass))
  {
    stop("One of the required variables for this function has not been specified.")
  } else {
    # Set the desired binwidth
    binwidth <- 1

    # Calc height of max frequency bar - for setting height of pass boundary text
    max_frequency <-
      max(hist(
        data[[colScore]],
        breaks = seq(min(data[[colScore]]), max(data[[colScore]]) + binwidth, binwidth),
        plot = FALSE
      )$counts)

    if (gradeScheme == "PF") {
      plot <-
        ggplot(data, aes(x = .data[[colScore]], fill = .data[[colGrade]])) +
        geom_histogram(colour = "black", binwidth = binwidth) +
        xlab("Score") +
        ylab("Frequency") +
        scale_fill_manual(values = fnGPColours("PF"),
                          guide = guide_legend(reverse = FALSE)) +
        geom_vline(xintercept = ceiling(scorePass) - 0.5,
                   linetype = "dashed") +
        annotate(
          geom = "text",
          x = ceiling(scorePass) + 0.1,
          y = max_frequency * 0.65,
          label = "Pass Boundary",
          angle = 90,
          hjust = 0,
          fontface = 2
        ) +
        scale_y_continuous(expand = expansion(add = c(0, 1)),
                           limits = c(0, max_frequency + 5)) +
        scale_x_continuous(limits = c(-1, scoreMax + 1),
                           expand = expansion(add = c(-1, -0.4))) +
        theme(aspect.ratio = 2) +
        themeGP()
    } else if (gradeScheme == "PFE") {
      # For PFE, the excellent boundary is also included
      plot <-
        ggplot(data, aes(x = .data[[colScore]], fill = .data[[colGrade]])) +
        geom_histogram(colour = "black", binwidth = binwidth) +
        xlab("Score") +
        ylab("Frequency") +
        scale_fill_manual(values = fnGPColours("PFE")[c(3,1,2)],
                          guide = guide_legend(reverse = TRUE)) +
        geom_vline(xintercept = ceiling(scorePass) - 0.5,
                   linetype = "dashed") +
        geom_vline(xintercept = ceiling(scoreExcellent) - 0.5,
                   linetype = "dashed") +
        annotate(
          geom = "text",
          x = ceiling(scorePass) + 0.1,
          y = max_frequency,
          label = "Pass Boundary",
          angle = 90,
          hjust = 0,
          fontface = 2
        ) +
        annotate(
          geom = "text",
          x = ceiling(scoreExcellent) + 0.1,
          y = max_frequency,
          label = "Excellent Boundary",
          angle = 90,
          hjust = 0,
          fontface = 2
        ) +
        scale_y_continuous(expand = expansion(add = c(0, 1)),
                           limits = c(0, max_frequency + 5)) +
        scale_x_continuous(limits = c(-1, scoreMax + 1),
                           expand = expansion(add = c(-1, -0.4))) +
        theme(aspect.ratio = 2) +
        themeGP()
    }
    return(plot)
  }
}
