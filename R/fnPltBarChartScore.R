#' Produces a bar chart of score distributions.
#'
#' @description This function plots the distribution of a column of scores, colouring the bars by outcome grade if data is available.
#'
#' @usage   fnPltBarChartScore(data, "scoreVar",  maxScore, "outcomeVar")
#'
#' @param data A data frame containing the scores to be plotted.
#' @param scoreVar The name of the specific column containing the scores (numeric or integer). Must be in quotation marks.
#' @param maxScore The maximum score possible on the assessment.
#' @param outcomeVar The name of the column containing the outcome grades if present. Can be a factor or character string. Must be in quotation marks. Leave this empty if no outcome grouping is required.
#'
#' @author Lizzi Gabe-Thomas (elizabeth.gabe-thomas@plymouth.ac.uk)
#'
#' @examples
#'
#' dataExample<-dataExample
#' dataExample$outcomePFE[dataExample$Score.Restricted>= 24] <- "Excellent"
#' dataExample$outcomePFE[dataExample$Score.Restricted>= 13 &  dataExample$Score.Restricted < 24] <- "Pass"
#' dataExample$outcomePFE[dataExample$Score.Restricted< 13 ] <- "Fail"
#' fnPltBarChartScore(dataExample, "Score.Restricted",  25, "outcomePFE")
#'
#'
#' dataExample<-dataExample
#' dataExample$outcomePF[dataExample$Score.Restricted>= 13] <- "Pass"
#' dataExample$outcomePF[dataExample$Score.Restricted< 13 ] <- "Fail"
#' fnPltBarChartScore(dataExample, "Score.Restricted", 28, "outcomePF")
#'
#'
#' dataExample<-dataExample
#' fnPltBarChartScore(dataExample, "Score.Restricted", 30)
#'
#'
#' @export

# Version 1.02

fnPltBarChartScore = function (data = NULL,
                               scoreVar = NULL,
                               maxScore = NULL,
                               outcomeVar = NULL)
{
  if (!is.null(outcomeVar)) {
    # if there is a grade outcome present
    whichScheme <-
      fnGPSchemeDetect(data[[outcomeVar]])$Scheme #detect correct scheme
    data <-
      data.frame(xAxis = data[[scoreVar]], fill = factor(data[[outcomeVar]],
                                                         levels = c("Fail", "Pass", "Excellent"))) #make dataframe for plot

    legPos <- c(0.2, 0.8) # set legend position

    cols <-
      fnGPColours(DataOrScheme = whichScheme[length(whichScheme)]) #extract the theme colours
    cols <-
      strsplit(cols, " ") # spli the character string into a list

    colours <-
      c("Fail" = cols[[1]],
        "Pass" = cols[[2]],
        "Excellent" = cols[[3]]) #assign the list elements to levels of factor

  }
  else {
    #if there is not a grade outcome
    whichScheme <- "Maroon" #set colour
    data <-
      data.frame(xAxis = data[[scoreVar]], fill = as.factor(0)) #make dataframe for plot
    legPos <- "none" # no legend
    colours <- whichScheme
  }

  data <- data[complete.cases(data),]

  # draw the plot
  ggplot(data, aes(x = xAxis, fill = fill)) +
    geom_bar(colour = "black", width = 1) +
    theme_psmd() +
    scale_fill_manual(values = colours) +
    scale_y_continuous(name = "Frequency", breaks = seq(0, maxScore, 2)) +
    scale_x_continuous(
      name = "Score",
      breaks = seq(0, maxScore, 1),
      limits = c(0, maxScore + 1),
      expand = c(0, 0)
    ) +
    theme(legend.title = element_blank(), legend.position = legPos)
}
