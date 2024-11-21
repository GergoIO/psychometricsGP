#' Detect Items to Review
#'
#' @param assessment_type A string. The type of assessment being analysed. This will determine the selection process for finding and returning items to review
#' @param item_analysis_data A dataframe. This dataframe must contain at least the following columns: "Item" (the items number), "Stage 5 Facility" (for ADK), "Stage 5 vs 2 Growth" (for ADK), "Stage 5 PtBis" (for ADK) OR "Stage 3 Facility" (for ADTK), "Stage 3 vs 2 Growth" (for ADTK), "Stage 3 PtBis" (for ADTK) OR "Stage 2 Facility" (for PAPT), "Stage 2 vs 1 Growth" (for PAPT), "Stage 2 PtBis" (for PAPT) OR "Facility" (for IDS/Y1KT), "PtBis" (for IDS/Y1KY)
#' @param test_in_year (OPTIONAL unless AMK) - Required for AMK assessments so the stage to use for item review can be determined
#'
#' @return A list of the detected items to review is returned.
#' @export
#'
#' @examples #For ADK/ADTK/PAPT/IDS/Y1KT/AMK/AKT:
#'  cnst <- append(
#'  cnst,
#'   fn_item_review(assessment_type = cnst$assessment_type, item_analysis_data = tab$itemAnalysis)
#'   )
#'
#' # For AMK:
#'  cnst <- append(
#'  cnst,
#'   fn_item_review(assessment_type = cnst$assessment_type, item_analysis_data = tab$itemAnalysis, test_in_year = cnst$test_in_year)
#'   )
#'
################################################################################
#'
fn_item_review <- function(assessment_type = NULL,
                         item_analysis_data = NULL,
                         test_in_year = NULL) {
  if (is.null(assessment_type) |
      is.null(item_analysis_data)) {
    stop("One of the required variables for this function has not been specified.")
  } else{
    listOfItemReview <- list()
    #   ________________________________________________________________________
    #   ADK                                                                 ####

    if (assessment_type %in% c("ADK")) {
      # Low (<20%) Facility
      listOfItemReview <- list(
        itemReviewLowFac = toString(unique(sort(
          as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Stage 5 Facility"]] < 0.2]))
        ))),

        # Negative Growth Items
        itemReviewNegGrowth = toString(unique(sort(
          as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Stage 5 vs 2 Growth"]] < 0]))
        ))),

        # Negative PtBis Items
        itemReviewNegPtBis = toString(unique(sort(
          as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Stage 5 PtBis"]] < 0]))
        ))),

        # Statistically Significant Negative PtBis Items
        itemReviewSigNegPtBis = toString(unique(sort(
          as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Stage 5 Sig PtBis"]] < 0]))
        )))
      )
    } else if (assessment_type %in% c("ADTK")) {
      # Low (<20%) Facility
      listOfItemReview <- list(
        itemReviewLowFac = toString(unique(sort(
          as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Stage 3 Facility"]] < 0.2]))
        ))),

        # Negative Growth Items
        itemReviewNegGrowth = toString(unique(sort(
          as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Stage 3 vs 2 Growth"]] < 0]))
        ))),

        # Negative PtBis Items
        itemReviewNegPtBis = toString(unique(sort(
          as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Stage 3 PtBis"]] < 0]))
        ))),

        # Statistically Significant Negative PtBis Items
        itemReviewSigNegPtBis = toString(unique(sort(
          as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Stage 3 Sig PtBis"]] < 0]))
        )))
      )
    } else if (assessment_type %in% c("AMK")) {
      # AMK logic
      if (is.null(test_in_year)) {
        stop("For AMK assessments, the test in year (test_in_year) variable must be defined.")
      } else {
        # Check if test_in_year is 1 or 2
        if (test_in_year %in% c(1, 2)) {
          # Low (<20%) Facility - Stage 5
          # Negative PtBis Items - Stage 5
          listOfItemReview <- list(
            itemReviewLowFac = toString(unique(sort(
              as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Stage 5 Facility"]] < 0.2]))
            ))),
            itemReviewNegPtBis = toString(unique(sort(
              as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Stage 5 PtBis"]] < 0]))
            )))
          )
        } else {
          # Low (<20%) Facility - Stage 4
          # Negative PtBis Items - Stage 4
          listOfItemReview <- list(
            itemReviewLowFac = toString(unique(sort(
              as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Stage 4 Facility"]] < 0.2]))
            ))),
            itemReviewNegPtBis = toString(unique(sort(
              as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Stage 4 PtBis"]] < 0]))
            )))
          )
        }
      }
    } else if (assessment_type %in% c("PAPT")) {
      # Low (<20%) Facility
      listOfItemReview <- list(
        itemReviewLowFac = toString(unique(sort(
          as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Stage 2 Facility"]] < 0.2]))
        ))),

        # Negative Growth Items
        itemReviewNegGrowth = toString(unique(sort(
          as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Stage 2 vs 1 Growth"]] < 0]))
        ))),

        # Negative PtBis Items
        itemReviewNegPtBis = toString(unique(sort(
          as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Stage 2 PtBis"]] < 0]))
        ))),

        # Statistically Significant Negative PtBis Items
        itemReviewSigNegPtBis = toString(unique(sort(
          as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Stage 2 Sig PtBis"]] < 0]))
        )))
      )
    } else if (assessment_type %in% c("IDS", "Y1KT", "AKT")) {
      # Low (<20%) Facility
      listOfItemReview <- list(
        itemReviewLowFac = toString(unique(sort(
          as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Facility"]] < 0.2]))
        ))),

        # Negative PtBis Items
        itemReviewNegPtBis = toString(unique(sort(
          as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["PtBis"]] < 0]))
        ))),

        # Statistically Significant Negative PtBis Items
        itemReviewSigNegPtBis = toString(unique(sort(
          as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Sig PtBis"]] < 0]))
        )))
      )
    } else if (assessment_type %in% c("MSA", "iMSA")) {
      # Low (<20%) Facility
      listOfItemReview <- list(
        itemReviewLowFac = toString(unique(sort(
          as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Facility"]] < 0.2]))
        ))),

        # Negative PtBis Items
        itemReviewNegPtBis = toString(unique(sort(
          as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["PtBis"]] < 0]))
        ))),

        # Statistically Significant Negative PtBis Items
        itemReviewSigNegPtBis = toString(unique(sort(
          as.numeric(na.omit(item_analysis_data$Item[item_analysis_data[["Sig PtBis"]] < 0]))
        )))
      )
    }


    return(listOfItemReview)

  }
}

# if (is.null(assessment_type) |
#     is.null(item_analysis_data)) {
#   stop("One of the required variables for this function has not been specified.")
# } else{
#   listOfItemReview <- list()
#   #   ________________________________________________________________________
#   #   ADK                                                                 ####
#
#   if (assessment_type %in% c("ADK")) {
#     # Low (<20%) Facility
#     listOfItemReview$itemReviewLowFac <-
#       unique(sort(as.numeric(c(
#         na.omit(item_analysis_data$Item[item_analysis_data[["Stage 5 Facility"]] < 0.2])
#       ))))
#     listOfItemReview$itemReviewLowFac <-
#       toString(listOfItemReview$itemReviewLowFac, sep = ",")
#
#     # Negative Growth Items
#     listOfItemReview$itemReviewNegGrowth <-
#       unique(sort(as.numeric(c(
#         na.omit(item_analysis_data$Item[item_analysis_data[["Stage 5 vs 2 Growth"]] < 0])
#       ))))
#     listOfItemReview$itemReviewNegGrowth <-
#       toString(listOfItemReview$itemReviewNegGrowth, sep = ",")
#
#     # Negative PtBis Items
#     listOfItemReview$itemReviewNegPtBis <-
#       unique(sort(as.numeric(c(
#         na.omit(item_analysis_data$Item[item_analysis_data[["Stage 5 PtBis"]] < 0])
#       ))))
#     listOfItemReview$itemReviewNegPtBis <-
#       toString(listOfItemReview$itemReviewNegPtBis, sep = ",")
#
#     # Statistically Significant Negative PtBis Items
#     listOfItemReview$itemReviewSigNegPtBis <-
#       unique(sort(as.numeric(c(
#         na.omit(item_analysis_data$Item[item_analysis_data[["Stage 5 Sig PtBis"]] < 0])
#       ))))
#     listOfItemReview$itemReviewSigNegPtBis <-
#       toString(listOfItemReview$itemReviewSigNegPtBis, sep = ",")
#
#
#   } else if (assessment_type %in% c("ADTK")) {
#     #   ______________________________________________________________________
#     #   ADTK                                                              ####
#
#     # Low (<20%) Facility
#     listOfItemReview$itemReviewLowFac <-
#       unique(sort(as.numeric(c(
#         na.omit(item_analysis_data$Item[item_analysis_data[["Stage 3 Facility"]] < 0.2])
#       ))))
#     listOfItemReview$itemReviewLowFac <-
#       toString(listOfItemReview$itemReviewLowFac, sep = ",")
#
#     # Negative Growth Items
#     listOfItemReview$itemReviewNegGrowth <-
#       unique(sort(as.numeric(c(
#         na.omit(item_analysis_data$Item[item_analysis_data[["Stage 3 vs 2 Growth"]] < 0])
#       ))))
#     listOfItemReview$itemReviewNegGrowth <-
#       toString(listOfItemReview$itemReviewNegGrowth, sep = ",")
#
#     # Negative PtBis Items
#     listOfItemReview$itemReviewNegPtBis <-
#       unique(sort(as.numeric(c(
#         na.omit(item_analysis_data$Item[item_analysis_data[["Stage 3 PtBis"]] < 0])
#       ))))
#     listOfItemReview$itemReviewNegPtBis <-
#       toString(listOfItemReview$itemReviewNegPtBis, sep = ",")
#
#     # Statistically Significant Negative PtBis Items
#     listOfItemReview$itemReviewSigNegPtBis <-
#       unique(sort(as.numeric(c(
#         na.omit(item_analysis_data$Item[item_analysis_data[["Stage 3 Sig PtBis"]] < 0])
#       ))))
#     listOfItemReview$itemReviewSigNegPtBis <-
#       toString(listOfItemReview$itemReviewSigNegPtBis, sep = ",")
#
#
#   } else if (assessment_type %in% c("AMK")) {
#     #   ______________________________________________________________________
#     #   AMK                                                               ####
#
#     if (is.null(test_in_year)) {
#       stop("For AMK assessments, the test in year (test_in_year) variable must be defined.")
#     } else {
#       if (test_in_year %in% c(1, 2)) {
#         # Low (<20%) Facility - Stage 5
#         listOfItemReview$itemReviewLowFac <-
#           unique(sort(as.numeric(c(
#             na.omit(item_analysis_data$Item[item_analysis_data[["Stage 5 Facility"]] < 0.2])
#           ))))
#         # Negative PtBis Items - Stage 5
#         listOfItemReview$itemReviewNegPtBis <-
#           unique(sort(as.numeric(c(
#             na.omit(item_analysis_data$Item[item_analysis_data[["Stage 5 PtBis"]] < 0])
#           ))))
#       } else {
#         # Low (<20%) Facility - Stage 4
#         listOfItemReview$itemReviewLowFac <-
#           unique(sort(as.numeric(c(
#             na.omit(item_analysis_data$Item[item_analysis_data[["Stage 4 Facility"]] < 0.2])
#           ))))
#         # Negative PtBis Items - Stage 4
#         listOfItemReview$itemReviewNegPtBis <-
#           unique(sort(as.numeric(c(
#             na.omit(item_analysis_data$Item[item_analysis_data[["Stage 4 PtBis"]] < 0])
#           ))))
#       }
#       # Convert to strings
#       listOfItemReview$itemReviewLowFac <-
#         toString(listOfItemReview$itemReviewLowFac, sep = ",")
#       listOfItemReview$itemReviewNegPtBis <-
#         toString(listOfItemReview$itemReviewNegPtBis, sep = ",")
#     }
#   } else if (assessment_type %in% c("PAPT")) {
#     #   ______________________________________________________________________
#     #   PAPT                                                              ####
#
#     # Low (<20%) Facility
#     listOfItemReview$itemReviewLowFac <-
#       unique(sort(as.numeric(c(
#         na.omit(item_analysis_data$Item[item_analysis_data[["Stage 2 Facility"]] < 0.2])
#       ))))
#     listOfItemReview$itemReviewLowFac <-
#       toString(listOfItemReview$itemReviewLowFac, sep = ",")
#
#     # Negative Growth Items
#     listOfItemReview$itemReviewNegGrowth <-
#       unique(sort(as.numeric(c(
#         na.omit(item_analysis_data$Item[item_analysis_data[["Stage 2 vs 1 Growth"]] < 0])
#       ))))
#     listOfItemReview$itemReviewNegGrowth <-
#       toString(listOfItemReview$itemReviewNegGrowth, sep = ",")
#
#     # Negative PtBis Items
#     listOfItemReview$itemReviewNegPtBis <-
#       unique(sort(as.numeric(c(
#         na.omit(item_analysis_data$Item[item_analysis_data[["Stage 2 PtBis"]] < 0])
#       ))))
#     listOfItemReview$itemReviewNegPtBis <-
#       toString(listOfItemReview$itemReviewNegPtBis, sep = ",")
#
#     # Statistically Significant Negative PtBis Items
#     listOfItemReview$itemReviewSigNegPtBis <-
#       unique(sort(as.numeric(c(
#         na.omit(item_analysis_data$Item[item_analysis_data[["Stage 2 Sig PtBis"]] < 0])
#       ))))
#     listOfItemReview$itemReviewSigNegPtBis <-
#       toString(listOfItemReview$itemReviewSigNegPtBis, sep = ",")
#
#   } else if (assessment_type %in% c("IDS", "Y1KT", "AKT")) {
#     #   ______________________________________________________________________
#     #   IDS/Y1KT                                                          ####
#
#     # Low (<20%) Facility
#     listOfItemReview$itemReviewLowFac <-
#       unique(sort(as.numeric(c(
#         na.omit(item_analysis_data$Item[item_analysis_data[["Facility"]] < 0.2])
#       ))))
#     listOfItemReview$itemReviewLowFac <-
#       toString(listOfItemReview$itemReviewLowFac, sep = ",")
#
#     # Negative PtBis Items
#     listOfItemReview$itemReviewNegPtBis <-
#       unique(sort(as.numeric(c(
#         na.omit(item_analysis_data$Item[item_analysis_data[["PtBis"]] < 0])
#       ))))
#     listOfItemReview$itemReviewNegPtBis <-
#       toString(listOfItemReview$itemReviewNegPtBis, sep = ",")
#
#     # Statistically Significant Negative PtBis Items
#     listOfItemReview$itemReviewSigNegPtBis <-
#       unique(sort(as.numeric(c(
#         na.omit(item_analysis_data$Item[item_analysis_data[["Sig PtBis"]] < 0])
#       ))))
#     listOfItemReview$itemReviewSigNegPtBis <-
#       toString(listOfItemReview$itemReviewSigNegPtBis, sep = ",")
#
#   }
#   return(listOfItemReview)
# }
# }
