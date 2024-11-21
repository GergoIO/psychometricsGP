#' Report: Add ANOVA details
#'
#' @description This function will perform all relevant stage specific analysis (currently tested for BMBS AMK Assessments). The stage specific analysis is performed, generated data, plots and dataframes are saved to variables and also added to the specified report along with surround text and other formatting (heading, captions, explanations etc.)
#'
#' @param stage (Numeric, IGNORED/OPTIONAL if anovaIncludesAllStages is TRUE). The stage to add analysis for
#' @param anovaIncludesAllStages (Default = FALSE). Set to TRUE if the Anova you are adding is not stage specific and rather also looks at variance by stage
#' @param report String - the variable name of the report which the analysis should be added to
#' @param listOfDetails A list - containing at least the following variables must be defined as part of that list:
#' test_in_year (an integer, which test in the years is this, 1 for 1st, 2 for 2nd etc.) and assessment (the current assessment number - eg: PT36)
#' @param listOfDemographics A list of dataframes - containing 'AnovaStage#', 'MeansAdjStage#', MeansObsStage#'
#' @param tableCount Numeric (integer) - the current number of tables in the report (will be incremented as more tables are added)
#'
#' @return The latest values of the plot and table counts are returned as a vector (with the table count as the first item). This is to enable those values to be updated in the main script while this function is used inside a loop
#' @export
#'
#' @examples
#' .rtn <- fnRptAnova(
#' stage = cnst$stages,
#' report = rptAll,
#' listOfDetails = cnst,
#' listOfDemographics = tabDemog,
#' tableCount = nTab
#' )
#' rptAll <- .rtn$report
#' nTab <- .rtn$tableCount
#'
#' .rtn <- fnRptAnova(
#' anovaIncludesAllStages = TRUE,
#' report = rptAll,
#' listOfDetails = cnst,
#' listOfDemographics = tabDemog,
#' tableCount = nTab
#' )
#' rptAll <- .rtn$report
#' nTab <- .rtn$tableCount
#'
#   ____________________________________________________________________________
#   Variables                                                               ####
# Currently only testing on AMK

fnRptAnova <- function(stage = NULL,
                       anovaIncludesAllStages = FALSE,
                       report = NULL,
                       listOfDetails = NULL,
                       listOfDemographics = NULL,
                       tableCount = NULL) {
  if (is.null(report) |
      is.null(listOfDetails) |
      is.null(listOfDemographics) |
      is.null(tableCount)) {
    stop("fnRptAnova: One of the required variables for this function has not been specified.")
  } else{
    #   ________________________________________________________________________
    #   Adding to Report                                                    ####

    # STAGE SPECIFIC
    if (anovaIncludesAllStages == FALSE) {
      message(
        glue(
          "fnRptAnova: Adding stage specific analysis to the specified report for Stage {stage}"
        )
      )
      # NOTE on incrementing tableCount:
      # The relevant function (fnRptAddTable) increments the counts in the global environment of the script which call this function
      # The counts are not incremented within the scope of this script so it must be manually done here after each use of fnRptAddTable

      ##  ........................................................................
      ##  ANOVA                                                               ####

      report <-
        body_add_par(report,
                     glue("Stage {stage} Subgroup Analysis (ANOVA)"),
                     style = "heading 3")

      report <- fnRptAddText(
        report = report,
        text = glue(
          "Analysis of variance of the scores by students' gender, ethnicity and disability status showed{ifelse(any(listOfDemographics[[glue('AnovaStage{stage}')]][['P-value']] < 0.05), ' ', ' no ')}statistically significant variation as shown in the table below."
        )
      ) |>
        body_add_par("")

      # ANOVA
      # Statistically significant rows are yellow
      report <- fnRptAddTable(
        report = report,
        table = listOfDemographics[[glue('AnovaStage{stage}')]] |>
          qflextable() |>
          bg(i = ~ `P-value` < 0.05, bg = "yellow"),
        tableCount = tableCount,
        caption = glue(
          "Analysis of variance table (Type III sums of squares, dependent variable: ({listOfDetails$assessment} % Score))"
        ),
        stopFlextableConversion = TRUE
      )
      # Manually increment table count
      tableCount <- tableCount + 1

      # Adj Means
      report <- fnRptAddTable(
        report = report,
        table = listOfDemographics[[glue('MeansAdjStage{stage}')]],
        tableCount = tableCount,
        caption = glue(
          "Estimated marginal means (dependent variable: ({listOfDetails$assessment} % Score))"
        )
      )
      # Manually increment table count
      tableCount <- tableCount + 1

      report <- fnRptAddText(
        report = report,
        text = glue(
          "Score variation by entry pathway and origin cannot reliably be included in the above ANOVA in the case of low student numbers. The table below presents Stage {stage} students' observed mean scores broken down by Entry Pathway and Origin."
        )
      )

      # Obs Means
      report <- fnRptAddTable(
        report = report,
        table = listOfDemographics[[glue('MeansObsStage{stage}')]],
        tableCount = tableCount,
        caption = glue("Observed mean percentage scores")
      )
      # Manually increment table count
      tableCount <- tableCount + 1
  } else{
    #NOT STAGE SPECIFIC
    report <-
      body_add_par(report, "Demographic Analysis (Anova)", style = "heading 3")

    report <-
      fnRptAddText(
        report = report,
        glue(
          "Analysis of variance of the scores by students' gender, ethnicity and disability status showed{ifelse(any(listOfDemographics[['AnovaAll']][['P-value']] < 0.05), ' ', ' no ')}statistically significant variation as shown in the table below."
        )
      ) |>
      body_add_par("")

    # ANOVA
    # Statistically significant rows are yellow
    report <- fnRptAddTable(
      report = report,
      table = listOfDemographics$AnovaAll |>
        qflextable() |>
        bg(i = ~ `P-value` < 0.05, bg = "yellow"),
      tableCount = tableCount,
      caption = glue(
        "Analysis of variance table (Type III sums of squares, dependent variable: ({listOfDetails$assessment} % Score))"
      ),
      stopFlextableConversion = TRUE
    )
    # Manually increment table count
    tableCount <- tableCount + 1

    report <-
      fnRptAddTable(
        # TAB Stage est marginal means
        report = report,
        table = listOfDemographics$MeansAdjAll,
        tableCount = tableCount,
        caption = glue(
          'Estimated marginal means (dependent variable: {listOfDetails$assessment} score'
        )
      )
    # Manually increment table count
    tableCount <- tableCount + 1

    report <- fnRptAddText(
      report = report,
      text = glue(
        "Score variation by entry pathway and origin cannot reliably be included in the above ANOVA in the case of low student numbers. The table below presents students' observed mean scores broken down by Entry Pathway and Origin."
      )
    )

    report <-
      fnRptAddTable(
        # TAB Stage observed means
        report = report,
        table = listOfDemographics$MeansObsAll,
        tableCount = tableCount,
        caption = "Observed mean percentage scores"
      )
    # Manually increment table count
    tableCount <- tableCount + 1
  }
}
  # For now- must return tableCount so that it can be manually assigned in the main script. Global assignment not currently working and values are not updated between loops
  return(list(report = report,
              tableCount = tableCount))
}
