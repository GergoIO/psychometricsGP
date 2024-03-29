% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_item_analysis.R
\name{fn_item_analysis}
\alias{fn_item_analysis}
\title{Item Analysis}
\usage{
fn_item_analysis(
  stages = NULL,
  scores_all = NULL,
  excluded_items = NULL,
  facility_bounds = NULL
)
}
\arguments{
\item{stages}{A vector - denoting the stages to be considered. (stages = 2 or stages = c(2,3,4,5) etc.). It makes sense to set all stages that sat the assessment.}

\item{scores_all}{Stage separated (each list item should be stage1 etc. with ALL (including excluded items) items showing non-negatively marked item (col) scores for each student (row) in that stage (l_results$stages_scores_all)}

\item{excluded_items}{A vector - denoting the items to remove decided post-test. If empty, the data marked 'all' will be the same as data without the 'all' demarkation}

\item{facility_bounds}{A two-element vector (usually c(0.2, 0.8)) containing the thresholds where item facilities are deemed low or high respectively. (0.2 corresponds to a 20% item facility etc.)}
}
\value{
A list of dataframes is returned containing all the item analysis info. If saving this list to an existing list of dataframes, make sure to use the append function. Otherwise, this returned list can be saved directly to an empty variable.
}
\description{
Item Analysis
}
\examples{
item_analysis <- fn_item_analysis(stages = cnst$stages, scores_all = lResults$stagesScoresAll, excluded_items = cnst$itemsExcl, facility_bounds = c(0.2,0.8))


}
