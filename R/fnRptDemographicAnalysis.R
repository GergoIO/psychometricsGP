#' ANOVA for Report
#'
#' @param report String - the variable name of the report which the analysis should be added to
#' @param cnst *** SHOULD CHANGE THIS - ONLY cnst$assessment is required
#' @param tab *** A list - to save any generated tables to
#' @param tabDemog *** A list - to save any generated demographics tables
#' @param tableCount Numeric (integer) - the current number of tables in the report (will be incremented as more tables are added)
#'
#' @return ***
#' @export
#'
#' @examples ***
fnRptDemographicAnalysis <- function(report = NULL,
                                       cnst = NULL,
                                       tab = NULL,
                                       tabDemog = NULL,
                                       tableCount = NULL) {
  if (is.null(report) == TRUE |
      is.null(cnst) == TRUE |
      is.null(tab) == TRUE |
      is.null(tabDemog) == TRUE |
      is.null(tableCount) == TRUE) {
    stop(
      "fnRptDemographicAnalysis: One of the required variables for this function has not been specified."
    )
  } else{
    fnRptSectionHeading(report, glue("Demographic Analysis"))

    fnRptAddText(
      report = report,
      text = glue(
        "Analysis of variance of the scores by students gender, ethnicity and disability status showed{ifelse(any(tabDemog$Anova[['P-value']] < 0.05), ' ', ' no ')}statistically significant variation as shown in the table below."
      )
    )
    fnRptAddParagraph(report)

    # TAB ANOVA
    fnRptAddTable(
      report = report,
      table = tabDemog$Anova,
      tableCount = tableCount,
      caption = glue(
        "Analysis of variance table (Type III sums of squares, dependent variable: ({cnst$assessment} % Score))"
      )
    )
    # TAB Adj Means
    fnRptAddTable(
      report = report,
      table = tabDemog$MeansAdj,
      tableCount = tableCount,
      caption = glue(
        "Estimated marginal means (dependent variable: ({cnst$assessment} % Score))"
      )
    )

    fnRptAddText(
      report = report,
      text = glue(
        "Score variation by entry pathway and origin cannot reliably be included in the above ANOVA due to low student numbers. The table below presents students' observed mean scores broken down by Entry Pathway and Origin."
      )
    )
    # TAB Obs Means
    fnRptAddTable(
      report = report,
      table = tabDemog$MeansObs,
      tableCount = tableCount,
      caption = glue("Observed mean percentage scores")
    )
  }
}
