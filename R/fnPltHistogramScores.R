#' Histogram of student assessment scores with optional boxplot and background shading by grade ranges
#'
#' @description fnPltHistogramScores creates a histogram of scores. Optional arguments draw a boxplot above the histogram and shade the plot background according to supplied grade boundaries.
#' The output from fnPltHistogramScores is a ggplot object which can then be saved or customised as needed.
#' See Examples for more details.
#'
#' @usage fnPltHistogramScores(x,lo=0,hi=100,gradeScheme="",gradeBounds=c(),main="",ylab="Frequency (N students)",plotBox=TRUE,pcScore=TRUE)
#'
#' @param x A vector of assessment scores (usually percentages but may exceed 100 or be negative).
#' @param lo Minimum value for plotting the x axis, best given as an appropriate multiple of 10 because the axis labels are spaced in tens. If min(x)<lo then lo is adjusted downwards (in multiples of 10)
#' @param hi Maximum value for plotting the x axis, If max(x)>hi then hi is adjusted upwards (in multiples of 10)
#' @param gradeScheme A character string giving the first letter of the grades, ordered from lowest to highest. e.g. "UBSE" or "FP". Letters other than U, B, S, E, F or P will be displayed but not shaded. If gradeScheme is not specified then the background will be unshaded
#' @param gradeBounds A numeric vector of grade thresholds. e.g. c(40,50,60). NOTE: length(gradeBounds) must equal nchar(gradeScheme) - 1, otherwise the background will be unshaded
#' @param main Title for the plot, if required.
#' @param ylab Label for the y axis.
#' @param plotBox If TRUE (the default) adds a boxplot with mean point above the histogram.
#' @param pcScore If TRUE (the default) changes the x axis label from "Score" to "Score (percent)".
#'
#' @note
#' This function uses \code{\link[ggplot2]{ggplot2}} for the underlying graphics.
#'
#' @return A ggplot object which can be saved or customised as needed.
#'
#' @examples
#' Basic Usage (examples employ randomly generated data so the resulting plots are variable)
#' 	fnPltHistogramScores(x=rnorm(86,30,10),lo=-10,hi=80,gradeScheme="UBSE",gradeBounds=c(20,25,45))
#'
#' Ability to accommodate NA values and to adjust lo and hi (set to 0 and 80 in this example) to fit data
#' 	fnPltHistogramScores(x=c(-1,2,rnorm(86,30,10),65,93,100,NA),lo=0,hi=80,gradeScheme="USE",gradeBounds=c(18.25,45.62))
#'
#' If length(gradeBounds) not equal to nchar(gradeScheme) - 1 then no background shading is carried out
#' 	fnPltHistogramScores(x=rnorm(86,30,10),gradeScheme="FP",gradeBounds=c(18.25,25.62))
#'
#' Scores need not be percentages and the boxplot can be removed
#' 	fnPltHistogramScores(x=rnorm(86,150,20),lo=100,hi=200,gradeScheme="FP",gradeBounds=c(135),plotBox=FALSE,pcScore=FALSE)
#'
#' Outputs a ggplot object, which can them be edited as necessary.
#' 	plotExample<-fnPltHistogramScores(x=rnorm(86,50,15),gradeScheme="US",gradeBounds=c(44))
#' 	plotExample  # plot output
#' 	plotExample+ggtitle("Example Distribution of Assessment Scores") # plot output plus title
#'
#' @source Written by Martin Roberts (psychometrics@plymouth.ac.uk)
#'
#' @export
#
################################################################################
#
fnPltHistogramScores <-
  function(x,
           lo = 0,
           hi = 100,
           gradeScheme = "",
           gradeBounds = c(),
           main = "",
           ylab = "Frequency (N students)",
           plotBox = TRUE,
           pcScore = TRUE) {
    # Written by: Martin Roberts
    # Last updated: 16/01/2018
    # Edited by Gergo Pinter 05/10/22, given a new name and moved to a new package (PsychometricsGP)
    ################################################
    x <- na.omit(x)
    if (min(c(x, gradeBounds), na.rm = TRUE) < lo) {
      lo <-
        10 * floor(min(c(x, gradeBounds), na.rm = TRUE) / 10)
    }	# If min(c(x,gradeBounds))<lo then lo is adjusted accordingly (in multiples of 10)
    if (max(c(x, gradeBounds), na.rm = TRUE) > hi) {
      hi <-
        10 * ceiling(max(c(x, gradeBounds), na.rm = TRUE) / 10)
    }	# If max(c(x,gradeBounds))>hi then hi is adjusted accordingly (in multiples of 10)
    maxfreq <-
      max(hist(x, breaks = seq(lo, hi, 1), plot = FALSE)$counts)	# maxfreq is the height of the tallest bar in the histogram
    gradeLabels <-
      strsplit(gradeScheme, "")[[1]]	#Converts the gradeScheme string into a vector of single characters
    gradeCols = c(
      U = '#D92120',
      B = '#E68B33',
      S = '#86BB6A',
      E = '#3D52A1',
      F = '#D92120',
      P = '#86BB6A'
    )[gradeLabels]
    Ngrades <- nchar(gradeScheme)
    plotGrades <-
      (Ngrades > 0 &
         Ngrades == length(gradeBounds) + 1) 	# If TRUE, background is shaded according to grade boundaries
    if (plotGrades) {
      rectsGB <-
        data.frame(
          xstart = c(lo, gradeBounds),
          xend = c(gradeBounds, hi + 1),
          Grade = gradeLabels
        )
    } # Dataframe of x coordinates to draw shaded rectangle
    rectsBox <-
      data.frame(
        xstart = boxplot.stats(x)$stats[-5],
        xend = boxplot.stats(x)$stats[-1],
        ystart = c(maxfreq * 1.1, maxfreq * 1.05, maxfreq * 1.05, maxfreq * 1.1),
        yend = c(maxfreq * 1.1, maxfreq * 1.15, maxfreq *
                   1.15, maxfreq * 1.1)
      )	# Dataframe of x,y coordinates to draw boxplot
    outliers <-
      boxplot.stats(x)$out 	#Vector of x coordinates to add outliers to boxplot
    ggplot() +
      themeGP() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.length = unit(4, "pt")
      ) +
      scale_x_continuous(
        name = paste("Score", if (pcScore) {
          " (%)"
        }),
        breaks = seq(lo, hi, 10),
        limits = c(lo, hi + 1),
        expand = c(0, 0)
      ) +
      annotate(
        geom = "segment",
        x = seq(lo, hi + 1, 1),
        xend = seq(lo, hi + 1, 1),
        y = 0,
        yend = -0.1
      ) +  # Adds minor ticks
      scale_y_continuous(
        name = ylab,
        breaks = seq(0, maxfreq, ifelse(maxfreq > 20, 10 * ceiling(maxfreq / 100), 2)),
        expand = c(0, 0)
      ) +
      {
        if (plotGrades)
          geom_rect(
            data = rectsGB,
            aes(
              xmin = xstart,
              xmax = xend,
              ymin = 0,
              ymax = maxfreq * 1.24,
              fill = Grade
            ),
            fill = gradeCols,
            col = "grey40",
            alpha = 0.5
          )
      } +
      {
        if (plotGrades)
          annotate(
            geom = "text",
            x = (rectsGB$xstart + rectsGB$xend) / 2,
            y = rep(maxfreq * 1.2, Ngrades),
            label = gradeLabels,
            col = gradeCols,
            alpha = 0.75,
            size = 6,
            fontface = 2
          )
      } +
      geom_histogram(
        aes(x),
        breaks = seq(lo, hi + 1, 1),
        col = "black",
        fill = ifelse(plotGrades, "grey40", "maroon"),
        na.rm = TRUE,
        closed = "right"
      ) +
      theme(axis.line = element_line(colour = "black")) +
      labs(title = main) +
      {
        if (plotBox)
          geom_rect(
            data = rectsBox,
            aes(
              xmin = xstart,
              xmax = xend,
              ymin = ystart,
              ymax = yend
            ),
            fill = ifelse(plotGrades, "grey80", "maroon"),
            col = "grey20",
            size = 1
          )
      } +
      {
        if (plotBox)
          geom_point(
            data = data.frame(x = outliers, y = rep(maxfreq * 1.1, length(outliers))),
            aes(x = x, y = y),
            shape = 8,
            col = "grey20",
            size = 1.5
          )
      } +
      {
        if (plotBox)
          geom_point(
            data = data.frame(x = mean(x), y = maxfreq * 1.1),
            aes(x = x, y = y),
            shape = 9,
            col = "grey10",
            size = 3
          )
      } +
      {
        if (plotBox)
          geom_point(
            data = data.frame(x = lo, y = maxfreq * 1.16),
            aes(x = x, y = y),
            shape = 1,
            col = "black",
            size = 0
          )
      }
  }
