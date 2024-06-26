% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_point_biserial_significant.R
\name{fn_point_biserial_significant}
\alias{fn_point_biserial_significant}
\title{Calculate PtBis and Statistical Significance for Test Items}
\usage{
fn_point_biserial_significant(
  data,
  assessment = cnst$assessment,
  colStudentID = "StudentID",
  colIsCorrect = "IsCorrect",
  colItemID = "ItemID",
  significanceLevel = 0.05
)
}
\arguments{
\item{data}{(dataframe) Incoming long data eg a dataframe with cols StudentID, ItemID and IsCorrect}

\item{assessment}{(String) The name of the test.}

\item{colStudentID}{(default = "StudentID"), String The name of the column containing the Student ID number}

\item{colIsCorrect}{(default = "IsCorrect"), String. The name of the column containing the item score in each row}

\item{colItemID}{(default = "ItemID"), String. The name of the column containing the ItemID in each row}

\item{significanceLevel}{(default = 0.05) Numeric. Set the significance level for each point biserial correlation}
}
\value{
returns a new dataframe storing the point biserial info for all input items
}
\description{
Calculate PtBis and Statistical Significance for Test Items
}
\examples{
 df <- data.frame(


}
