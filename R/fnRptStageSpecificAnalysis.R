#' Report: Stage Specific Analysis
#'
#' @description This function will perform all relevant stage specific analysis (currently tested for BMBS AMK Assessments). The stage specific analysis is performed, generated data, plots and dataframes are saved to variables and also added to the specified report along with surround text and other formatting (heading, captions, explanations etc.)
#'
#' @param stage Numeric (integer) - the stage to compile analysis for
#' @param report String - the variable name of the report which the analysis should be added to
#' @param listOfDetails A list - containing at least the following variables must be defined as part of that list:
#' testInYear (an integer, which test in the years is this, 1 for 1st, 2 for 2nd etc.), assessment (the current assessment number - eg: PT36) and assessmentPrev (the previous assessment number - eg: PT35)
#' @param listOfPlots A list - to save any generated plots to
#' @param listOfTables A list - to save any generated tables to
#' @param listOfDemographics A list - to save any generated demographics tables
#' @param listOfTestRetest A list - to save any generated test retest analysis (values, lists and dataframes)
#' @param stageSpecificScores A list - Each list item must be a dataframe titled 'Stage #' where # is the stage. Each list item must contain only number data of the scores for each student (rows) on each item (columns). No other information (studentIDs etc.) should be included
#' @param tableCount Numeric (integer) - the current number of tables in the report (will be incremented as more tables are added)
#' @param plotCount Numeric (integer) - the current number of plots in the report (will be incremented as more plots are added)
#'
#' @return The latest values of the plot and table counts are returned as a vector (with the table count as the first item). This is to enable those values to be updated in the main script while this function is used inside a loop
#' @export
#'
#' @examples ***

################################################################################

# Currently only testing on AMK

fnRptStageSpecificAnalysis <- function(stage = NULL,
                                       report = NULL,
                                       listOfDetails = NULL,
                                       listOfPlots = NULL,
                                       listOfTables = NULL,
                                       listOfDemographics = NULL,
                                       listOfTestRetest = NULL,
                                       stageSpecificScores = NULL,
                                       plotCount = NULL,
                                       tableCount = NULL) {
  if (is.null(stage) |
      is.null(report) |
      is.null(listOfDetails) |
      is.null(listOfPlots) |
      is.null(listOfTables) |
      is.null(listOfDemographics) |
      is.null(listOfTestRetest) |
      is.null(stageSpecificScores) |
      is.null(plotCount) |
      is.null(tableCount)) {
    stop(
      "fnRptStageSpecificAnalysis: One of the required variables for this function has not been specified."
    )
  } else{
    message(
      glue(
        "fnRptStageSpecificAnalysis: Adding stage specific analysis to the specified report for Stage {stage}"
      )
    )
    # NOTE on increment plotCount and tableCount:
    # The relevant functions (fnRptAddPlot and fnRptAddTable) increment the counts in the global environment of the script which call this function
    # The counts are not incremented within the scope of this script so it must be manually done here after each use of fnRptAddPlot and fnRptAddTable

    fnRptSectionHeading(report, glue("Stage {stage} Score Distribution"))

    fnRptAddText(
      report = report,
      text = glue(
        "Scores for the {dim(stageSpecificScores[[glue('stage{stage}')]])[1]} Stage {stage} students were{ifelse(listOfTables[[glue('shapiroStage{stage}')]]$ShapiroP<0.05, ' not', '')} normally distributed (Shapiro-Wilk test, W={sprintf('%.3f', listOfTables[[glue('shapiroStage{stage}')]]$ShapiroW)}, p={sprintf('%.3f', listOfTables[[glue('shapiroStage{stage}')]]$ShapiroP)})."
      )
    )
    fnRptAddParagraph(report)

    # listOfPlots Score Distr. for STAGES B
    fnRptAddPlot(
      report = report,
      plot = listOfPlots[[glue('histogramScoresStage{stage}')]],
      plotCount = plotCount,
      caption = glue('Distribution of Stage {stage} test scores')
    )
    # Manually increment plot count
    plotCount <- plotCount + 1

    fnRptSectionHeading(report, glue("Stage {stage} Test-Retest Statistics"))

    if (listOfDetails$testInYear == 1 && stage == 1) {
      fnRptAddText(report = report,
                   text = "As this is the first test for this cohort, there are no test-retest statistics available.")
    } else{
      fnRptAddText(
        report = report,
        text = glue(
          "There were {listOfTestRetest[[glue('nTestRetestStage{stage}')]]} Stage {stage} students for whom scores on both this test ({listOfDetails$assessment}) and the previous test ({listOfDetails$assessmentPrev}) were available. The Pearson correlation between the test scores was {sprintf('%.2f', listOfTestRetest[[glue('corrValStage{stage}')]])} (95% confidence interval {sprintf('%.2f', listOfTestRetest[[glue('corrLowCIStage{stage}')]])} to {sprintf('%.2f', listOfTestRetest[[glue('corrHighCIStage{stage}')]])})."
        )
      )
      fnRptAddParagraph(report)

      # listOfPlots Scatterplot of test score comparisons STAGES A
      fnRptAddPlot(
        report = report,
        plot = listOfPlots[[glue('testRetestStage{stage}')]],
        plotCount = plotCount,
        caption = glue(
          'Scatterplot of Stage {stage} scores on current and previous test'
        )
      )
      # Manually increment plot count
      plotCount <- plotCount + 1

      # listOfTables Test Retest Matrix STAGES A
      fnRptAddTable(
        report = report,
        table = listOfTestRetest[[glue('matrixStage{stage}')]],
        tableCount = tableCount,
        caption = glue(
          'Stage {stage} students by grade awarded in {listOfDetails$assessment} (row) and {listOfDetails$assessmentPrev} (column)'
        )
      )
      # Manually increment table count
      tableCount <- tableCount + 1
    }

    fnRptSectionHeading(report, glue("Stage {stage} Subgroup Analysis (ANOVA)"))

    fnRptAddText(
      report = report,
      text = glue(
        "Analysis of variance of the scores by students gender, ethnicity and disability status showed{ifelse(any(listOfDemographics[[glue('AnovaStage{stage}')]][['P-value']] < 0.05), ' ', ' no ')}statistically significant variation as shown in the table below."
      )
    )
    fnRptAddParagraph(report)

    # listOfTables ANOVA STAGES B
    fnRptAddTable(
      report = report,
      table = listOfDemographics[[glue('AnovaStage{stage}')]],
      tableCount = tableCount,
      caption = glue(
        "Analysis of variance table (Type III sums of squares, dependent variable: ({listOfDetails$assessment} % Score))"
      )
    )
    # Manually increment table count
    tableCount <- tableCount + 1

    # listOfTables Adj Means STAGES B
    fnRptAddTable(
      report = report,
      table = listOfDemographics[[glue('MeansAdjStage{stage}')]],
      tableCount = tableCount,
      caption = glue(
        "Estimated marginal means (dependent variable: ({listOfDetails$assessment} % Score))"
      )
    )
    # Manually increment table count
    tableCount <- tableCount + 1

    fnRptAddText(
      report = report,
      text = glue(
        "Score variation by entry pathway and origin cannot reliably be included in the above ANOVA due to low student numbers. The table below presents Stage {stage} students' observed mean scores broken down by Entry Pathway and Origin."
      )
    )
    # listOfTables Obs Means STAGES B
    fnRptAddTable(
      report = report,
      table = listOfDemographics[[glue('MeansObsStage{stage}')]],
      tableCount = tableCount,
      caption = glue("Observed mean percentage scores")
    )
    # Manually increment table count
    tableCount <- tableCount + 1
  }
  # For now- must return plotCount and tableCount so that they can be manually assigned in the main script. Global assignment not currently working and values are not updated between loops
  return(c(plotCount, tableCount))
  # Update plot and table counts in the global environment
  # assign(deparse(substitute(plotCount)), plotCount, envir = globalenv())
  # assign(deparse(substitute(tableCount)), tableCount, envir = globalenv())
}


################################################################################
# Below is the manual version that does not use this function for simplification
################################################################################

# for (i in listOfDetails$stagesB) {
#   fnRptSectionHeading(rpt$All, glue("Stage {i} Score Distribution"))
#
#   fnRptAddText(
#     report = rpt$All,
#     text = glue(
#       "Scores for the {dim(stageSpecificScores[[glue('Stage {i}')]])[1]} Stage {i} students were {ifelse(listOfTables[[glue('ShapiroStage{i}')]]$ShapiroP<0.05, 'not', '')} normally distributed (Shapiro-Wilk test, W={sprintf('%.3f', listOfTables[[glue('ShapiroStage{i}')]]$ShapiroW)}, p={sprintf('%.3f', listOfTables[[glue('ShapiroStage{i}')]]$ShapiroP)}."
#     )
#   )
#   fnRptAddParagraph(rpt$All)
#
#   # listOfPlots Score Distr. for STAGES B
#   fnRptAddPlot(
#     report = rpt$All,
#     plot = listOfPlots[[glue('HistogramScoresStage{i}')]],
#     plotCount = plotCount,
#     caption = glue('Distribution of Stage {i} test scores')
#   )
#
#   fnRptSectionHeading(rpt$All, glue("Stage {i} Test-Retest Statistics"))
#
#   fnRptAddText(
#     report = rpt$All,
#     text = glue(
#       "There were {listOfTestRetest[[glue('nTestRetestStage{i}')]]} Stage {i} students for whom scores on both this test ({listOfDetails$assessment}) and the previous test ({listOfDetails$assessmentPrev}) were available. The Pearson correlation between the test scores was {sprintf('%.2f', listOfTestRetest[[glue('corrValStage{i}')]])} (95% confidence interval {sprintf('%.2f', listOfTestRetest[[glue('corrLowCIStage{i}')]])} to {sprintf('%.2f', listOfTestRetest[[glue('corrHighCIStage{i}')]])})."
#     )
#   )
#   fnRptAddParagraph(rpt$All)
#
#   # listOfPlots Scatterplot of test score comparisons STAGES A
#   fnRptAddPlot(
#     report = rpt$All,
#     plot = listOfPlots[[glue('testRetestStage{i}')]],
#     plotCount = plotCount,
#     caption = glue('Scatterplot of Stage {i} scores on current and previous test')
#   )
#
#   # listOfTables Test Retest Matrix STAGES A
#   fnRptAddTable(
#     report = rpt$All,
#     table = listOfTestRetest[[glue('MatrixStage{i}')]],
#     tableCount = tableCount,
#     caption = glue(
#       'Stage {i} students by grade awarded in {listOfDetails$assessment} (row) and {listOfDetails$assessmentPrev} (column)'
#     )
#   )
#
#   fnRptSectionHeading(rpt$All, glue("Stage {i} Subgroup Analysis (ANOVA)"))
#
#   fnRptAddText(
#     report = rpt$All,
#     text = glue(
#       "Analysis of variance of the scores by students gender, ethnicity and disability status showed{ifelse(any(listOfDemographics[[glue('AnovaStage{i}')]][['P-value']] < 0.05), ' ', ' no ')}statistically significant variation as shown in the table below."
#     )
#   )
#   fnRptAddParagraph(rpt$All)
#
#   # listOfTables ANOVA STAGES B
#   fnRptAddTable(
#     report = rpt$All,
#     table = listOfDemographics[[glue('AnovaStage{i}')]],
#     tableCount = tableCount,
#     caption = glue(
#       "Analysis of variance table (Type III sums of squares, dependent variable: ({listOfDetails$assessment} % Score))"
#     )
#   )
#   # listOfTables Adj Means STAGES B
#   fnRptAddTable(
#     report = rpt$All,
#     table = listOfDemographics[[glue('MeansAdjStage{i}')]],
#     tableCount = tableCount,
#     caption = glue(
#       "Estimated marginal means (dependent variable: ({listOfDetails$assessment} % Score))"
#     )
#   )
#
#   fnRptAddText(
#     report = rpt$All,
#     text = glue(
#       "Score variation by entry pathway and origin cannot reliably be included in the above ANOVA due to low student numbers. The table below presents Stage {i} students' observed mean scores broken down by Entry Pathway and Origin."
#     )
#   )
#   # listOfTables Obs Means STAGES B
#   fnRptAddTable(
#     report = rpt$All,
#     table = listOfDemographics[[glue('MeansObsStage{i}')]],
#     tableCount = tableCount,
#     caption = glue("Observed mean percentage scores")
#   )
#
# } # END OF STAGES A LOOP
