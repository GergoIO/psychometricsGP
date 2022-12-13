#' Create Empty Lists From a Vector of Strings
#'
#' @description This function uses an input of the assessment type ('assessmentType') being analysed. Then, the empty lists to create are set (as a vector) based on the assessment type. Each element of the vector is checked. If the element does not already exist as a variable, then a new variable (an empty list, with the name of the vector element) is created. Alternatively, if the required 'assessmentType' is not configured, set a variable 'listsToCreate' as the vector of strings to create lists from. One of 'assessmentType' or 'listsToCreate' must be defined. Setting 'listsToCreate' overrides a defined 'assessmentType'. If 'assessmentType' is not defined and 'listsToCreate' is defined, the function will create empty lists with the name of the items stored in the 'listsToCreate' vector of strings.
#'
#' @param assessmentType (OPTIONAL) A string - the current assessment being analysed (either "ADK" or "ADTK"). Setting this overrides any set
#' @param listsToCreate (OPTIONAL) A vector of strings.
#'
#' @return Creates the required empty lists based on the assessment type in the main (global) environment
#' @export
#'
#' @examples # If using a preconfigured assessmentType:
#' fnListsFromVector(assessmentType = "ADK")
#' #if manually setting the list of strings to create empty lists from:
#' fnListsFromVector(listsToCreate = c("list1", "list2"))
#'
################################################################################
#'
fnListsFromVector <- function(assessmentType = NULL,
                              listsToCreate) {
  # Define opposite of %in% operator
  `%!in%` <- Negate(`%in%`)
  # Store the assessment types that this functions can handle. Add to the vector when new types are added
  configuredAssessmentTypes <- c("ADK", "ADTK")

  #   __________________________________________________________________________
  #   Check error conditions                                                ####

  # Error condition - if the defined assessmentType is not in the list of preconfigured types and a manually configured variable storing the lists to create ('listsToCreate') has also not been set
  if (assessmentType %!in% configuredAssessmentTypes && is.na(listsToCreate)) {
    stop(
      "The configured assessment type has not been preconfigured in this function and the variable 'listsToCreate' has not been defined. To use the currently configured assessment type, define the input variable 'listsToCreate' to be a vector of strings of the names of any empty lists that must be created for later analysis."
    )
  }

  #   __________________________________________________________________________
  #   Set lists to create based on assessment type                          ####

  # If assessment type is in configures assessment types
  if (assessmentType %in% configuredAssessmentTypes) {
    if (assessmentType %in% c("ADK", "ADTK")) {
      vectorOfLists <- c(
        "historicStats",
        "itemAnalysis",
        "lReliab",
        "lResults",
        "plt",
        "rpt",
        "tabPulse",
        "testRetest",
        "testAngoff",
        "tabDemog"
      )
      message("fnListsFromVector: Creating the default lists for ADK/ADTK assessments")
    }
    # NOTE: Add more assessment types with the corresponding lists here using 'else if'
    print(vectorOfLists)
  }

  #   __________________________________________________________________________
  #   Overwrite is lists to create has been defined                         ####

  # Overwrite with new list if it was defined in the input of the function
  # Default use of function sets listsToCreate to NA in the input unless its otherwise defined
  if (!any(is.na(listsToCreate))) {
    message(
      "fnListsFromVector: Overwriting any preconfigured lists and using the defined names in the 'listsToCreate' variable to create lists"
    )
    vectorOfLists <- listsToCreate
  }

  print(vectorOfLists)

  #   __________________________________________________________________________
  #   Create empty lists                                                    ####

  # Check all elements of input vector
  for (i in 1:length(vectorOfLists)) {
    # Only run for elements of the input vector where a corresponding variable does not already exist
    if (!exists(as.character(vectorOfLists[i]))) {
      assign(vectorOfLists[i], list(), envir = globalenv())
    }
  }
}
