#' Time series plot of progress test statistics against test number, grouped by cohort
#'
#' #RENAMED from fnPlotHistoricStats to fnPltHistoricStats by Gergo Pinter (GP)
#' #26/09/2022
#' #No further changes by GP thus far
#'
#' @description fnPltHistoricStats creates a time series plot of progress test statistics against test number grouped by cohort.
#' The output from fnHistogramScore is a ggplot object which can then be saved or customised as needed.
#' See Examples for more details.
#'
#' @usage fnPltHistoricStats<-function(x,statistic,lo=NULL,hi=NULL,Ntests=24)
#'
#' @param x A data frame containing the historic progress test statistics.
#' @param statistic Name of the statistic to be plotted. Must be one of the following:
#' "Correct","Incorrect","DontKnow","Mean","Min","Max","SD","Alpha","Phi","RelativeSEM","AbsoluteSEM","Borderline","Satisfactory","Excellent".
#' @param lo Minimum value for plotting the y axis (optional). If specified will overide the automatic calculation of the lower axis limit.
#' @param hi Maximum value for plotting the y axis (optional). If specified will overide the automatic calculation of the upper axis limit.
#' @param Ntests Number of tests (counting from the most recent) that are included in the plot (default = 24). Avoids creating confused plots when the number of tests is large.
#'
#' @note
#' This function uses \code{\link[ggplot2]{ggplot2}} for the underlying graphics.
#'
#' @return A ggplot object which can be saved or customised as needed.
#'
#' @examples
#' library("xlsx")
#' filename<-c("X:\\1718\\BMBS\\AMK\\PT17\\Reports\\M_1718_PT17_V2 Historic Stats.xlsx",
#' "X:\\1718\\BDS\\ADK\\ADK23\\Reports\\D_1718_ADK23_V2 Historic Stats.xlsx",
#' "X:\\1718\\BScDTH\\ADTK\\ADTK07\\Reports\\TH_1718_ADTK07_V2 Historic Stats.xlsx",
#' "X:\\Plymouth\\PgDipPAS\\AMK\\1617\\PAPT12\\Reports\\P_1617_PAPT12_v1.1 Historic Stats.xlsx")
#' historicStats<-read.xlsx(filename[1],sheetIndex=1,stringsAsFactors=FALSE)
#'
#' fnPltHistoricStats(historicStats,"Mean")
#' fnPltHistoricStats(historicStats,"Incorrect",lo=0,hi=100)
#'
#' Outputs a ggplot object, which can them be edited as necessary.
#' 	plotExample<-fnPltHistoricStats(historicStats,"Alpha")
#' 	plotExample  # plot output
#' 	plotExample+ggtitle("Time Series Plot of Cronbach's Alpha") # plot output plus title
#'
#' @source Written by Martin Roberts (psychometrics@plymouth.ac.uk)
#'
#' @export

fnPltHistoricStats <-
  function(x,
           statistic,
           lo = NULL,
           hi = NULL,
           Ntests = 24) {
    # Written by: Martin Roberts
    # Last updated: 09/01/2018
    ################################################
    plotLabels <-
      data.frame(
        # Dataframe of main and y axis titles to be used depending on the chosen statistic
        title = c(
          "Correct responses",
          "Incorrect responses",
          "Don't know responses",
          rep("Student progress", 3),
          "Student variation",
          rep("Test reliability", 2),
          rep("Standard Error of Measurement", 2),
          paste(
            c("Borderline", "Satisfactory", "Excellent"),
            "Grade Boundary"
          )
        ),
        yLabel = c(
          rep("% of test items", 3),
          paste(c("Mean", "Min", "Max", "SD of"), "score (%)"),
          paste(c("Cronbach's Alpha", "Phi"), "coefficient"),
          "Relative SEM",
          "Absolute SEM",
          rep("Test score (%)", 3)
        ),
        row.names = c(
          "Correct",
          "Incorrect",
          "DontKnow",
          "Mean",
          "Min",
          "Max",
          "SD",
          "Alpha",
          "Phi",
          "RelativeSEM",
          "AbsoluteSEM",
          "Borderline",
          "Satisfactory",
          "Excellent"
        )
      )
    # ERROR TRAPPING
    if (!is.data.frame(x)) {
      stop("Please supply a data.frame as the first argument to fnPltHistoricStats.")
    }
    if (!any(statistic == row.names(plotLabels))) {
      stop(paste(
        "The 'statistic' argument must be one of the following:\n",
        paste(row.names(plotLabels), collapse = ", ")
      ))
    }
    # DATA MANIPULATION
    plotData <- x[, c("Test", "Cohort", statistic)]
    plotData$Cohort <-
      factor(plotData$Cohort)	# Ensures that Cohort is a factor
    plotData$Test <-
      as.numeric(gsub("\\D", "", plotData$Test))	# Ensures that Test is numeric (removes character prefix)
    plotData <-
      plotData[plotData$Test > max(plotData$Test) - Ntests, ]	# Ensures that only the most recent Ntests are plotted
    plotData[, statistic] <-
      as.numeric(plotData[, statistic])		# Ensures that the chosen statistic is numeric
    if (is.null(lo)) {
      lo <-
        min(plotData[, statistic], na.rm = TRUE)
    }	# If lo not specified ensures min data point is plotted
    if (is.null(hi)) {
      hi <-
        max(plotData[, statistic], na.rm = TRUE)
    }	# If hi not specified ensures max data point is plotted
    # PLOTTING
    plotCols <-
      rep(fnColours("Cohort"), 5)	# Plotting colour for each cohort
    plotShapes <-
      rep(c(19, 5, 17, 15, 25, 8, 1, 18, 24, 11, 0), 3)	# Plotting shape for each cohort
    ggplot(data = plotData,
           aes(
             x = Test,
             y = plotData[, statistic],
             group = Cohort,
             colour = Cohort,
             shape = Cohort
           )) +
      themeGP() + theme(plot.title = element_text(
        size = 12,
        face = "bold",
        hjust = 0.5
      )) +
      geom_line(size = 1) + geom_point(size = 3) +
      scale_x_continuous(breaks = seq(1, max(plotData$Test))) +
      scale_y_continuous(limits = c(lo, hi), breaks = pretty(c(lo, hi), n =
                                                               10)) +
      theme(
        legend.position = "bottom",
        legend.key = element_rect(
          fill = "white",
          size = 0,
          linetype = 0
        )
      ) +
      xlab("Progress test") + ylab(plotLabels[statistic, "yLabel"]) +
      ggtitle(plotLabels[statistic, "title"]) +
      scale_color_manual(values = plotCols) + scale_shape_manual(values =
                                                                   plotShapes)
  }
