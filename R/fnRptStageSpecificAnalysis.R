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

fnRptStageSpecificAnalysis <- function(
  stage = NULL,
  report = NULL,
  cnst = NULL,
  plt = NULL,
  tab = NULL,
  tabDemog = NULL,
  dfResStagesOnlyScores = NULL,
  testRetest = NULL,
  countPlt = NULL,
  countTab = NULL
) {
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
    stop("fnRptStageSpecificAnalysis: One of the required variables for this function has not been specified.")
  } else{
    fnRptSectionHeading(report, glue("Stage {stage} Score Distribution"))

    fnRptAddText(
      report = report,
      text = glue(
        "Scores for the {dim(dfResStagesOnlyScores[[glue('Stage {stage}')]])[1]} Stage {stage} students were {ifelse(tab[[glue('ShapiroStage{stage}')]]$ShapiroP<0.05, 'not', '')} normally distributed (Shapiro-Wilk test, W={sprintf('%.3f', tab[[glue('ShapiroStage{stage}')]]$ShapiroW)}, p={sprintf('%.3f', tab[[glue('ShapiroStage{stage}')]]$ShapiroP)}."
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
      caption = glue('Scatterplot of Stage {stage} scores on current and previous test')
    )

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
