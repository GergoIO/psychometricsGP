% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fnRptAnova.R
\name{fnRptAnova}
\alias{fnRptAnova}
\title{Report: Add ANOVA details}
\usage{
fnRptAnova(
  stage = NULL,
  anovaIncludesAllStages = FALSE,
  report = NULL,
  listOfDetails = NULL,
  listOfDemographics = NULL,
  tableCount = NULL
)
}
\arguments{
\item{stage}{(Numeric, IGNORED/OPTIONAL if anovaIncludesAllStages is TRUE). The stage to add analysis for}

\item{anovaIncludesAllStages}{(Default = FALSE). Set to TRUE if the Anova you are adding is not stage specific and rather also looks at variance by stage}

\item{report}{String - the variable name of the report which the analysis should be added to}

\item{listOfDetails}{A list - containing at least the following variables must be defined as part of that list:
testInYear (an integer, which test in the years is this, 1 for 1st, 2 for 2nd etc.) and assessment (the current assessment number - eg: PT36)}

\item{listOfDemographics}{A list of dataframes - containing 'AnovaStage#', 'MeansAdjStage#', MeansObsStage#'}

\item{tableCount}{Numeric (integer) - the current number of tables in the report (will be incremented as more tables are added)}
}
\value{
The latest values of the plot and table counts are returned as a vector (with the table count as the first item). This is to enable those values to be updated in the main script while this function is used inside a loop
}
\description{
This function will perform all relevant stage specific analysis (currently tested for BMBS AMK Assessments). The stage specific analysis is performed, generated data, plots and dataframes are saved to variables and also added to the specified report along with surround text and other formatting (heading, captions, explanations etc.)
}
\examples{
.rtn <- fnRptAnova(
stage = cnst$stages,
report = rptAll,
listOfDetails = cnst,
listOfDemographics = tabDemog,
tableCount = nTab
)
rptAll <- .rtn$report
nTab <- .rtn$tableCount

.rtn <- fnRptAnova(
anovaIncludesAllStages = TRUE,
report = rptAll,
listOfDetails = cnst,
listOfDemographics = tabDemog,
tableCount = nTab
)
rptAll <- .rtn$report
nTab <- .rtn$tableCount

}
