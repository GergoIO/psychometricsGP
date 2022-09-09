#' Report: Stage Specific Analysis
#'
#' @param stage ***
#' @param report ***
#' @param cnst ***
#' @param plt ***
#' @param tab ***
#' @param tabDemog ***
#' @param dfResStagesOnlyScores ***
#' @param testRetest ***
#' @param countPlt ***
#' @param countTab ***
#'
#' @return ***
#' @export
#'
#' @examples ***

################################################################################

# Currently only testing on AMK

fnRptStageSpecificAnalysis <- function(stage = NULL,
                                       report = NULL,
                                       cnst = NULL,
                                       plt = NULL,
                                       tab = NULL,
                                       tabDemog = NULL,
                                       dfResStagesOnlyScores = NULL,
                                       testRetest = NULL,
                                       countPlt = NULL,
                                       countTab = NULL) {
  if (is.null(stage) == TRUE |
      is.null(report) == TRUE |
      is.null(cnst) == TRUE |
      is.null(plt) == TRUE |
      is.null(tab) == TRUE |
      is.null(tabDemog) == TRUE |
      is.null(dfResStagesOnlyScores) == TRUE |
      is.null(testRetest) == TRUE |
      is.null(countPlt) == TRUE |
      is.null(countTab) == TRUE) {
    stop(
      "fnRptStageSpecificAnalysis: One of the required variables for this function has not been specified."
    )
  } else{
    fnRptSectionHeading(report, glue("Stage {stage} Score Distribution"))

    fnRptAddText(
      report = report,
      text = glue(
        "Scores for the {dim(dfResStagesOnlyScores[[glue('Stage {stage}')]])[1]} Stage {stage} students were {ifelse(tab[[glue('ShapiroStage{stage}')]]$ShapiroP<0.05, 'not', '')} normally distributed (Shapiro-Wilk test, W={sprintf('%.3f', tab[[glue('ShapiroStage{stage}')]]$ShapiroW)}, p={sprintf('%.3f', tab[[glue('ShapiroStage{stage}')]]$ShapiroP)})."
      )
    )
    fnRptAddParagraph(report)

    # PLT Score Distr. for STAGES B
    fnRptAddPlot(
      report = report,
      plot = plt[[glue('HistogramScoresStage{stage}')]],
      plotCount = countPlt,
      caption = glue('Distribution of Stage {stage} test scores')
    )

    fnRptSectionHeading(report, glue("Stage {stage} Test-Retest Statistics"))

    if (cnst$testInYear == 1 && stage == 1) {
      fnRptAddText(report = report,
                   text = "As this is the first test for this cohort, there are no test-retest statistics available.")
    } else{
      fnRptAddText(
        report = report,
        text = glue(
          "There were {testRetest[[glue('nTestRetestStage{stage}')]]} Stage {stage} students for whom scores on both this test ({cnst$assessment}) and the previous test ({cnst$assessmentPrev}) were available. The Pearson correlation between the test scores was {sprintf('%.2f', testRetest[[glue('corrValStage{stage}')]])} (95% confidence interval {sprintf('%.2f', testRetest[[glue('corrLowCIStage{stage}')]])} to {sprintf('%.2f', testRetest[[glue('corrHighCIStage{stage}')]])})."
        )
      )
      fnRptAddParagraph(report)

      # PLT Scatterplot of test score comparisons STAGES A
      fnRptAddPlot(
        report = report,
        plot = plt[[glue('testRetestStage{stage}')]],
        plotCount = countPlt,
        caption = glue(
          'Scatterplot of Stage {stage} scores on current and previous test'
        )
      )
    }

    # TAB Test Retest Matrix STAGES A
    fnRptAddTable(
      report = report,
      table = testRetest[[glue('MatrixStage{stage}')]],
      tableCount = countTab,
      caption = glue(
        'Stage {stage} students by grade awarded in {cnst$assessment} (row) and {cnst$assessmentPrev} (column)'
      )
    )

    fnRptSectionHeading(report, glue("Stage {stage} Subgroup Analysis (ANOVA)"))

    fnRptAddText(
      report = report,
      text = glue(
        "Analysis of variance of the scores by students gender, ethnicity and disability status showed{ifelse(any(tabDemog[[glue('AnovaStage{stage}')]][['P-value']] < 0.05), ' ', ' no ')}statistically significant variation as shown in the table below."
      )
    )
    fnRptAddParagraph(report)

    # TAB ANOVA STAGES B
    fnRptAddTable(
      report = report,
      table = tabDemog[[glue('AnovaStage{stage}')]],
      tableCount = countTab,
      caption = glue(
        "Analysis of variance table (Type III sums of squares, dependent variable: ({cnst$assessment} % Score))"
      )
    )
    # TAB Adj Means STAGES B
    fnRptAddTable(
      report = report,
      table = tabDemog[[glue('MeansAdjStage{stage}')]],
      tableCount = countTab,
      caption = glue(
        "Estimated marginal means (dependent variable: ({cnst$assessment} % Score))"
      )
    )

    fnRptAddText(
      report = report,
      text = glue(
        "Score variation by entry pathway and origin cannot reliably be included in the above ANOVA due to low student numbers. The table below presents Stage {stage} students' observed mean scores broken down by Entry Pathway and Origin."
      )
    )
    # TAB Obs Means STAGES B
    fnRptAddTable(
      report = report,
      table = tabDemog[[glue('MeansObsStage{stage}')]],
      tableCount = countTab,
      caption = glue("Observed mean percentage scores")
    )
  }
}

################################################################################
# Below is the manual version that does not use this function for simplification
################################################################################

# for (i in cnst$stagesB) {
#   fnRptSectionHeading(rpt$All, glue("Stage {i} Score Distribution"))
#
#   fnRptAddText(
#     report = rpt$All,
#     text = glue(
#       "Scores for the {dim(dfResStagesOnlyScores[[glue('Stage {i}')]])[1]} Stage {i} students were {ifelse(tab[[glue('ShapiroStage{i}')]]$ShapiroP<0.05, 'not', '')} normally distributed (Shapiro-Wilk test, W={sprintf('%.3f', tab[[glue('ShapiroStage{i}')]]$ShapiroW)}, p={sprintf('%.3f', tab[[glue('ShapiroStage{i}')]]$ShapiroP)}."
#     )
#   )
#   fnRptAddParagraph(rpt$All)
#
#   # PLT Score Distr. for STAGES B
#   fnRptAddPlot(
#     report = rpt$All,
#     plot = plt[[glue('HistogramScoresStage{i}')]],
#     plotCount = countPlt,
#     caption = glue('Distribution of Stage {i} test scores')
#   )
#
#   fnRptSectionHeading(rpt$All, glue("Stage {i} Test-Retest Statistics"))
#
#   fnRptAddText(
#     report = rpt$All,
#     text = glue(
#       "There were {testRetest[[glue('nTestRetestStage{i}')]]} Stage {i} students for whom scores on both this test ({cnst$assessment}) and the previous test ({cnst$assessmentPrev}) were available. The Pearson correlation between the test scores was {sprintf('%.2f', testRetest[[glue('corrValStage{i}')]])} (95% confidence interval {sprintf('%.2f', testRetest[[glue('corrLowCIStage{i}')]])} to {sprintf('%.2f', testRetest[[glue('corrHighCIStage{i}')]])})."
#     )
#   )
#   fnRptAddParagraph(rpt$All)
#
#   # PLT Scatterplot of test score comparisons STAGES A
#   fnRptAddPlot(
#     report = rpt$All,
#     plot = plt[[glue('testRetestStage{i}')]],
#     plotCount = countPlt,
#     caption = glue('Scatterplot of Stage {i} scores on current and previous test')
#   )
#
#   # TAB Test Retest Matrix STAGES A
#   fnRptAddTable(
#     report = rpt$All,
#     table = testRetest[[glue('MatrixStage{i}')]],
#     tableCount = countTab,
#     caption = glue(
#       'Stage {i} students by grade awarded in {cnst$assessment} (row) and {cnst$assessmentPrev} (column)'
#     )
#   )
#
#   fnRptSectionHeading(rpt$All, glue("Stage {i} Subgroup Analysis (ANOVA)"))
#
#   fnRptAddText(
#     report = rpt$All,
#     text = glue(
#       "Analysis of variance of the scores by students gender, ethnicity and disability status showed{ifelse(any(tabDemog[[glue('AnovaStage{i}')]][['P-value']] < 0.05), ' ', ' no ')}statistically significant variation as shown in the table below."
#     )
#   )
#   fnRptAddParagraph(rpt$All)
#
#   # TAB ANOVA STAGES B
#   fnRptAddTable(
#     report = rpt$All,
#     table = tabDemog[[glue('AnovaStage{i}')]],
#     tableCount = countTab,
#     caption = glue(
#       "Analysis of variance table (Type III sums of squares, dependent variable: ({cnst$assessment} % Score))"
#     )
#   )
#   # TAB Adj Means STAGES B
#   fnRptAddTable(
#     report = rpt$All,
#     table = tabDemog[[glue('MeansAdjStage{i}')]],
#     tableCount = countTab,
#     caption = glue(
#       "Estimated marginal means (dependent variable: ({cnst$assessment} % Score))"
#     )
#   )
#
#   fnRptAddText(
#     report = rpt$All,
#     text = glue(
#       "Score variation by entry pathway and origin cannot reliably be included in the above ANOVA due to low student numbers. The table below presents Stage {i} students' observed mean scores broken down by Entry Pathway and Origin."
#     )
#   )
#   # TAB Obs Means STAGES B
#   fnRptAddTable(
#     report = rpt$All,
#     table = tabDemog[[glue('MeansObsStage{i}')]],
#     tableCount = countTab,
#     caption = glue("Observed mean percentage scores")
#   )
#
# } # END OF STAGES A LOOP
