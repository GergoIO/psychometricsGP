#' Plot of the Borderline Regression method.
#'
#' @description fnPltBorderlineRegression creates a scatterplot assessment scores against global grades.
#' A regression line is added to the plot and the intersection of this line with the 'borderline group' is used to calculate a pass mark.
#' The main output from fnPltBorderlineRegression is a ggplot object which can then be saved or customised as needed.
#' Additionally the function passes the linear regression model and the pass mark into the global environment (see Examples).
#'
#' @usage fnPltBorderlineRegression(x,gradeCol="Grade",scoreCol="Score")
#'
#' @param x A data frame containing the global grade and score data.
#' @param gradeCol Name of the column containing the global grades.
#' @param scoreCol Name of the column containing the assessment scores.
#'
#' @note
#' This function uses \code{\link[ggplot2]{ggplot2}} for the underlying graphics,
#' and uses the str_wrap function from the \code{\link[stringr]{stringr}} package to wrap the x axis labels.
#'
#' @return A ggplot object which can be saved or customised as needed.
#' Additionally the linear regression model (bRegLine) and the pass mark (passMark) are passed to the global environment.
#'
#' @examples
#' library("ggplot2")
#' Data<-data.frame(Grade = factor(c(rep("Unsatisfactory",5),rep("Borderline",10),rep("Low Satisfactory",20),
#'        rep("High Satisfactory",25),rep("Excellent",10)),ordered=TRUE,
#'        levels = c("Unsatisfactory","Borderline","Low Satisfactory","High Satisfactory","Excellent")),
#'        Score = c(5,8:10,10, 10:14,11:15, 12:16,13:17,13:17,14:18, 13:17,14:18,15:19,16:20,17:21, 18:22,20:23,25),
#'        Grade4 = factor(c(rep("Unsatisfactory",5),rep("Borderline",20),rep("Satisfactory",32),rep("Excellent",13)),
#'        ordered = TRUE, levels = c("Unsatisfactory","Borderline","Satisfactory","Excellent")),
#'        Score4 = c(5,6,8:10, 9:13,10:14, 10:14,11:15,11:15,12:16, 12:16,13:17,13:17,14:18,14:18,15:20, 18:20,20))
#'
#' fnPltBorderlineRegression(Data)
#' bRegLine  # Details of the regression model
#' summary(bRegLine)   # Summary of the regression model
#' car::outlierTest(bRegLine)   # Test for significant outliers in the model
#' passMark  # The pass mark obtained from the model
#'
#' fnPltBorderlineRegression(Data,"Grade4","Score4")
#'
#'
#' @source Written by Martin Roberts (psychometrics@plymouth.ac.uk)
#'
#' @export

fnPltBorderlineRegression <- function(x,
                       gradeCol = "Grade",
                       scoreCol = "Score") {
  # Written by: Martin Roberts
  # Last updated: 07/03/2018
  # Required packages: "ggplot2","psychometricsPSMD","stringr"
  # Plots the borderline regression method, reporting the equation, RÂ² and pass mark
  # Passes the linear model and the pass mark to the global environment as bRegLine and passMark
  ################################################
  plotData <- x[, c(gradeCol, scoreCol)]
  colnames(plotData) <- c("Grade", "Score")
  # Checking for data format errors
  if (!is.ordered(plotData$Grade)) {
    stop(paste("Error:", gradeCol, " is not an ordered factor."))
  }
  if (!is.numeric(plotData$Score)) {
    stop(paste("Error:", scoreCol, " is not a numeric vector of scores."))
  }
  Ngrades <-
    length(levels(plotData$Grade))    # 5 for medical, 4 for dental
  maxScore <- Ngrades * 5    # 25 for medical, 20 for dental
  plotData$nGrade <-
    as.numeric(plotData$Grade)  # Converts Grade to numeric for plotting
  assign("bRegLine", lm(Score ~ nGrade, data = plotData), pos = .GlobalEnv)    # Saves the linear regression model to bRegLine
  yValues <-
    c(bRegLine$coefficients[1],
      bRegLine$coefficients[1] + bRegLine$coefficients[2] * Ngrades) # Extreme y values for plot
  cutPoint <-
    predict.lm(bRegLine, newdata = data.frame(nGrade = c(2)))	#Intersection of regression line with borderline group
  assign("passMark", fnGPRound(cutPoint, 2), pos = .GlobalEnv)	# Defines the pass mark accurately to 2dp and passes to the global environment
  bRegEquation <-
    paste0(
      "y = ",
      fnGPRound(bRegLine$coefficients[1], 3),
      " + ",
      fnGPRound(bRegLine$coefficients[2], 3),
      "x"
    ) #Reg equation as text
  bRegText1 <-
    paste0(
      "Regression line\n",
      bRegEquation,
      "\n(RÂ² = ",
      fnGPRound(summary(bRegLine)[[8]], 3),
      ")\nintersects the\nborderline group \nat score of ",
      fnGPRound(cutPoint, 4)
    )        # Text for plot
  bRegText2 <-
    paste0("Pass mark = ", formatC(passMark, digits = 2, format = "f"))        # Text for plot
  ggplot(data = plotData, aes(x = nGrade, y = Score)) + theme_psmd() +
    scale_shape_manual(values = 18) +
    geom_count(colour = "gray20", alpha = 0.7) +
    scale_size_area(max_size = 8) +   # Plot duplicate points as bubbles, area ~ number of multiple points
    annotate(
      geom = "segment",
      x = 0,
      xend = 0.05,
      y = seq(0, maxScore) ,
      yend = seq(0, maxScore)
    ) +  # Minor tick marks
    geom_segment(
      aes(
        x = 0,
        y = bRegLine$coefficients[1],
        xend = Ngrades,
        yend = bRegLine$coefficients[1] + bRegLine$coefficients[2] * Ngrades
      ),
      colour = "black",
      size = 0.7
    ) +  # Plot regression line
    geom_segment(
      aes(
        x = 0,
        y = cutPoint,
        xend = 2,
        yend = cutPoint
      ),
      colour = "red",
      linetype = 2,
      size = 0.9
    ) +  # Join intersection point to y axis
    annotate(
      geom = "text",
      x = 1,
      y = cutPoint + 0.5,
      label = bRegText1,
      size = 3,
      color = "red",
      vjust = "bottom"
    ) +  # Add text above join line
    annotate(
      geom = "text",
      x = 1,
      y = cutPoint - 0.3,
      label = bRegText2,
      fontface = 2,
      color = "red",
      vjust = "top"
    ) +  # Add text below join line
    scale_x_continuous(
      limits = c(0, Ngrades + 0.5),
      breaks = seq(1, Ngrades),
      labels = stringr::str_wrap(levels(plotData$Grade), width = 1),
      expand = c(0, 0)
    ) +
    scale_y_continuous(limits = c(min(c(0, yValues)), max(c(maxScore, yValues))),
                       breaks = seq(0, maxScore, 5)) +
    xlab("Global grade") + ylab("Score")
}
