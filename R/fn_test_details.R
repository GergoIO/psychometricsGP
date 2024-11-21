#' Test Details Parameters (Stage Specific)
#'
#' @param assessment (OPTIONAL) A string - must define when used for IDS assessment for special conditions for that assessment type
#' @param stages A vector - denoting the stages to be considered. (stages = 2 or stages = c(2,3,4,5) etc.)
#' @param programme A string - the programme of the students (Must match what is in the "Programme" col in the data_demog dataframe (see below) eg "BMBS" etc.)
#' @param data_demog A dataframe - containing demographics data columns. Columns titled "Programme", "Stage" and "Year_Status" must be included. Make sure to use a df containing demographics data for all students and not one which has been filtered to include present students only
#' @param results A dataframe - containing results for students. A column titled "Stage" must be included. Each student should have one entry (row) in this dataframe. This will be used to determine the number of students assessed in each stage
#' @param results_absent A dataframe - containing results for absent students. A column titled "Stage" must be included. Each absent student should have one entry (row) in this dataframe. This will be used to determine the number of absent students who were not assessed in each stage
#'
#' @return A dataframe is returned (testDetails) containing the calculated test details. A new column is generated for each stage considered
#' @export
#'
#' @examples fn_test_details(assessment = cnst$assessment_type, stages = cnst$stages, programme = cnst$programme, demog = dfDemog, results = dfRes, results_absent = dfResAbsent)

################################################################################

fn_test_details <-
  function(assessment = NULL,
           stages = NULL,
           programme = NULL,
           data_demog = NULL,
           results = NULL,
           results_absent = NULL) {
    if (is.null(stages) |
        is.null(programme) |
        is.null(data_demog) |
        is.null(results) | is.null(results_absent)) {
      stop("One of the required variables for this function has not been specified.")
    } else{
      testDetails <- data.frame(stringsAsFactors = FALSE)
      # Continue as normal except for IDS assessment
      if (assessment != "IDS") {
        for (i in stages) {
          stage <- glue("Stage {i}")
          testDetails["Students in Stage", stage] <-
            length(
              which(
                data_demog$Programme == programme &
                  data_demog$Stage == i &
                  data_demog$Year_Status %!in% c("Interrupted", "Withdrawn")
              )
            )
          testDetails["Students Assessed", stage] <-
            dim(results[results$Stage == i, ])[1]
          testDetails["Students Absent", stage] <-
            dim(results_absent[results_absent$Stage == i, ])[1]

          fn_stats_scores <- function(x) {
            c(mean(x),
              median(x),
              sd(x),
              min(x),
              max(x),
              max(x) - min(x),
              IQR(x))
          }
          testDetails[c(
            "Mean Score (%)",
            "Median Score (%)",
            "Std. Dev",
            "Min Score (%)",
            "Max Score (%)",
            "Range",
            "IQR"
          ), stage] <-
            sapply(list(results[results$Stage == i, ]$pctScoreTotal), fn_stats_scores)
        }
      } else{
        # This is for IDS only

        # If dfAbsent is empty, bind_rows will not work because the Programme name will be logical and not character.
        # To fix, force the Programme col to be character
        dfAbsent <- dfAbsent |>
          mutate(
            Programme = as.character(Programme)
          )

        # Overall: 1 col entry for each Programmes and a final col for All Programmes
        resultsAll <- bind_rows(dfResults, dfAbsent) |>
          select(Programme, pctScoreTotal)

        detailsProgrammes <- resultsAll  |>
          group_by(Programme) |>
          summarise(
            `Number in Cohort` = n(),
            `Number Present` = sum(!is.na(pctScoreTotal)),
            `Number Absent` = sum(is.na(pctScoreTotal)),
            Mean = mean(pctScoreTotal, na.rm = TRUE),
            Median = median(pctScoreTotal, na.rm = TRUE),
            STDev = sd(pctScoreTotal, na.rm = TRUE),
            Min = min(pctScoreTotal, na.rm = TRUE),
            Max = max(pctScoreTotal, na.rm = TRUE),
            Range = max(pctScoreTotal, na.rm = TRUE) - min(pctScoreTotal, na.rm = TRUE),
            IQR = IQR(pctScoreTotal, na.rm = TRUE)
          )

        detailsAll <- resultsAll |>
          summarise(
            Programme = "All",
            `Number in Cohort` = n(),
            `Number Present` = sum(!is.na(pctScoreTotal)),
            `Number Absent` = sum(is.na(pctScoreTotal)),
            Mean = mean(pctScoreTotal, na.rm = TRUE),
            Median = median(pctScoreTotal, na.rm = TRUE),
            STDev = sd(pctScoreTotal, na.rm = TRUE),
            Min = min(pctScoreTotal, na.rm = TRUE),
            Max = max(pctScoreTotal, na.rm = TRUE),
            Range = max(pctScoreTotal, na.rm = TRUE) - min(pctScoreTotal, na.rm = TRUE),
            IQR = IQR(pctScoreTotal, na.rm = TRUE)
          )

        testDetails <- bind_rows(detailsProgrammes, detailsAll) |>
          t() |>
          as.data.frame() %>%
          set_names(.[1, ]) |>
          slice(-1)
      }
      return(testDetails)
    }
  }# END
