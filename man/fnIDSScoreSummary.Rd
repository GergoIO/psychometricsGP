% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fnIDSScoreSummary.R
\name{fnIDSScoreSummary}
\alias{fnIDSScoreSummary}
\title{Create score summary tables for IDS analysis}
\usage{
fnIDSScoreSummary(stage, dfDemographics, dfRes, dfResAbsent, programmes)
}
\arguments{
\item{stage}{The stage of students (numeric) This variable will be used to indentify the relevant students.}

\item{dfDemographics}{A dataframe containing student demographics}

\item{dfRes}{A dataframe containing the results for students who sat the exam}

\item{dfResAbsent}{A dataframe containing the results for students who did not sit the exam}

\item{programmes}{Which programmes to consider}
}
\value{
A table of the score summaries is returned
}
\description{
Create score summary tables for IDS analysis
}
\examples{
fnIDSScoreSummary(2, dfRes, dfResAbsent, c("BDS"))
}
