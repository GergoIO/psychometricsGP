#' Score Statistics Table
#'
#' @author Gergo Pinter, \email{gergo.pinter@@plymouth.ac.uk}, \url{https://www.gergo.io}
#'
#' @param assessmentType A string. The type of assessment being analysed. This will determine the selection process for finding and returning items to review
#' @param listOfConstants A list of constants. At least the following must be defined as list elements: assessment (Eg "ADK38"), nPresent (the number of students who sat the assessment), nItems (the original number of items), nItemsExcl (the number of items excluded), itemsExclStr (a string of the specific item numbers removed)
#' @param passMarks (OPTIONAL - Required for ADK, ADTK) A data frame containing the relevant passmarks. Usually a single row with cols for each stage (named "Stage 1", "Stage 2" etc.) The code of this function can be changed to match the specific formats that this table can take for different assessmentTypes.
#'
#' @return A dataframe of the score statistics (the main first table added to the assessment report)
#' @export
#'
#' @examples tab$scoreStatistics <- fnScoreStatistics(
#' assessmentType = "ADK",
#' listOfConstants = cnst,
#' passMarks =tab$passMarks)
#'
################################################################################
#'
fnScoreStatistics <- function(assessmentType = NULL,
                              listOfConstants = NULL,
                              passMarks = data.frame(stringsAsFactors = FALSE)) {
  if (is.null(assessmentType) |
      is.null(listOfConstants)) {
    stop("One of the required variables for this function has not been specified.")
  } else{
    # Create empty df to store tab
    scoreStatistics <- data.frame(stringsAsFactors = FALSE)

    if (cnst$assessmentType %in% c("ADK")) {
      #   ______________________________________________________________________
      #   ADK                                                               ####
      # ADK Create df of score stats
      scoreStatistics = data.frame(
        c(
          "Test Number" = listOfConstants$assessment,
          "Number of Candidates" = listOfConstants$nPresent,
          "Original Number of Items" = listOfConstants$nItems,
          "Number of Items Removed" = listOfConstants$nItemsExcl,
          "Items Removed" = listOfConstants$itemsExclStr,
          "Stage 5 Standard" = fnRnd(passMarks[["Stage 5"]], 2),
          "Stage 4 Standard" = fnRnd(passMarks[["Stage 4"]], 2),
          "Stage 3 Standard" = fnRnd(passMarks[["Stage 3"]], 2),
          "Stage 2 Standard" = fnRnd(passMarks[["Stage 2"]], 2)
        )
      )
      # Set cols and names for ADK/ADTK variants
      scoreStatistics <-
        rownames_to_column(scoreStatistics, "Test Detail")
      colnames(scoreStatistics)[2] <- "Value"

    } else if (cnst$assessmentType %in% c("ADTK")) {
      #   ______________________________________________________________________
      #   ADTK                                                              ####
      # ADTK Create df of score stats
      scoreStatistics = data.frame(
        c(
          "Test Number" = listOfConstants$assessment,
          "Number of Candidates" = listOfConstants$nPresent,
          "Original Number of Items" = listOfConstants$nItems,
          "Number of Items Removed" = listOfConstants$nItemsExcl,
          "Items Removed" = listOfConstants$itemsExclStr
        )
      )
      # Set cols and names for ADK/ADTK variants
      scoreStatistics <-
        rownames_to_column(scoreStatistics, "Test Detail")
      colnames(scoreStatistics)[2] <- "Value"
    } else if (cnst$assessmentType %in% c("IDS")) {
      scoreStatistics <-
        tibble(
          `Test Detail` = c(
            "Test Number",
            "Number in Cohort",
            "Original Number of Items",
            "Number of Items Removed",
            "Items Removed",
            "Pass Mark"
          ),
          `Value` = c(
            listOfConstants$assessment,
            listOfConstants$nCohort,
            listOfConstants$nItems,
            listOfConstants$nItemsExcl,
            listOfConstants$itemsExclStr,
            listOfConstants$passMark
          )
        )
    }
    return(scoreStatistics)
  }
}
