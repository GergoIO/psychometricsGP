% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fnRptStageSpecificAnalysis.R
\name{fnRptStageSpecificAnalysis}
\alias{fnRptStageSpecificAnalysis}
\title{Report: Stage Specific Analysis}
\usage{
fnRptStageSpecificAnalysis(
  stages = NULL,
  report = NULL,
  listOfDetails = NULL,
  listOfPlots = NULL,
  listOfTables = NULL,
  listOfDemographics = NULL,
  listOfTestRetest = NULL,
  stageSpecificScores = NULL,
  plotCount = NULL,
  tableCount = NULL
)
}
\arguments{
\item{stages}{Numeric (integer list) - the stages to compile analysis for. Looping is performed within the function}

\item{report}{String - the variable name of the report which the analysis should be added to}

\item{listOfDetails}{A list - containing at least the following variables must be defined as part of that list:
testInYear (an integer, which test in the years is this, 1 for 1st, 2 for 2nd etc.), assessment (the current assessment number - eg: PT36) and assessment_prev (the previous assessment number - eg: PT35)}

\item{listOfPlots}{A list - to save any generated plots to}

\item{listOfTables}{A list - to save any generated tables to}

\item{listOfDemographics}{A list - to save any generated demographics tables}

\item{listOfTestRetest}{A list - to save any generated test retest analysis (values, lists and dataframes)}

\item{stageSpecificScores}{A list - Each list item must be a dataframe titled 'Stage #' where # is the stage. Each list item must contain only number data of the scores for each student (rows) on each item (columns). No other information (studentIDs etc.) should be included}

\item{plotCount}{Numeric (integer) - the current number of plots in the report (will be incremented as more plots are added)}

\item{tableCount}{Numeric (integer) - the current number of tables in the report (will be incremented as more tables are added)}
}
\value{
The latest values of the plot and table counts are returned as a vector (with the table count as the first item). This is to enable those values to be updated in the main script while this function is used inside a loop
}
\description{
This function will perform all relevant stage specific analysis (currently tested for BMBS AMK Assessments). The stage specific analysis is performed, generated data, plots and dataframes are saved to variables and also added to the specified report along with surround text and other formatting (heading, captions, explanations etc.)
}
\examples{
.rtn <- fnRptStageSpecificAnalysis(
  stages = cnst$stagesA,
  report = rptAll,
  listOfDetails = cnst,
  listOfPlots = plt,
  listOfTables = tab,
  listOfDemographics = tabDemog,
  listOfTestRetest = testRetest,
  stageSpecificScores = lResults$stagesScores,
  plotCount = nPlt,
  tableCount = nTab
)
rptAll <- .rtn$report
nPlt <- .rtn$plotCount
nTab <- .rtn$tableCount

}
