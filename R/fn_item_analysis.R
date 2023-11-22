#' Item Analysis
#'
#' @param stages A vector - denoting the stages to be considered. (stages = 2 or stages = c(2,3,4,5) etc.). It makes sense to set all stages that sat the assessment.
#' @param scores_all Stage separated (each list item should be stage1 etc. with ALL (including excluded items) items showing non-negatively marked item (col) scores for each student (row) in that stage (l_results$stages_scores_all)
#' @param excluded_items A vector - denoting the items to remove decided post-test. If empty, the data marked 'all' will be the same as data without the 'all' demarkation
#' @param facility_bounds A two-element vector (usually c(0.2, 0.8)) containing the thresholds where item facilities are deemed low or high respectively. (0.2 corresponds to a 20% item facility etc.)
#'
#' @return A list of dataframes is returned containing all the item analysis info. If saving this list to an existing list of dataframes, make sure to use the append function. Otherwise, this returned list can be saved directly to an empty variable.
#' @export
#'
#' @examples item_analysis <- fn_item_analysis(stages = cnst$stages, scores_all = lResults$stagesScoresAll, excluded_items = cnst$itemsExcl, facility_bounds = c(0.2,0.8))
#'
################################################################################
#'
fn_item_analysis <- function(stages = NULL,
                             scores_all = NULL,
                             excluded_items = NULL,
                             facility_bounds = NULL) {
  if (is.null(stages) |
      is.null(scores_all) |
      is.null(facility_bounds)) {
    stop("One of the required variables for this function has not been specified.")
  } else{
    item_analysis <- list()

    for (i in stages) {
      .stage <- glue("stage{i}")
      # .stage_col <- glue("Stage {i}")
      .stage_scores_all <- scores_all[[.stage]]
      .stage_scores <-
        fn_rm_cols_by_name(.stage_scores_all, excluded_items)

      matrix_stage_all <- na.exclude(as.matrix(.stage_scores_all))
      matrix_stage <- na.exclude(as.matrix(.stage_scores))

      #   ______________________________________________________________________
      #   Facility: Stage Separated                                         ####
      # Calculate stage-specific facility

      item_analysis$stages_facility_all[[.stage]] <-
        apply(matrix_stage_all, 2, mean)
      item_analysis$stages_facility[[.stage]] <-
        apply(matrix_stage, 2, mean)

      #   ______________________________________________________________________
      #   Facility: Combined                                                ####
      # Combined calculated stage-specific facility data

      item_analysis$facility_all <-
        as.data.frame(do.call("cbind", item_analysis$stages_facility_all))
      colnames(item_analysis$facility_all) <-
        gsub("stage",
             "Stage ",
             colnames(item_analysis$facility_all),
             fixed = TRUE)
      item_analysis$facility <-
        as.data.frame(do.call("cbind", item_analysis$stages_facility))
      colnames(item_analysis$facility) <-
        gsub("stage",
             "Stage ",
             colnames(item_analysis$facility),
             fixed = TRUE)

      #   ______________________________________________________________________
      #   PtBis                                                             ####

      matrix_tot_all <- apply(matrix_stage_all, 1, sum)
      matrix_tot_no_index_all <- matrix_tot_all - (matrix_stage_all)
      item_analysis$stages_pt_bis_all[[.stage]] <-
        suppressWarnings(diag(cor(
          matrix_stage_all, matrix_tot_no_index_all
        )))

      matrix_tot <- apply(matrix_stage, 1, sum)
      matrix_tot_no_index <- matrix_tot - (matrix_stage)
      item_analysis$stages_pt_bis[[.stage]] <-
        suppressWarnings(diag(cor(matrix_stage, matrix_tot_no_index)))

      #   ______________________________________________________________________
      #   PtBis: Combined                                                   ####
      # Combined calculated stage-specific PtBis

      item_analysis$pt_bis_all <-
        as.data.frame(do.call("cbind", item_analysis$stages_pt_bis_all))
      colnames(item_analysis$pt_bis_all) <-
        gsub("stage",
             "Stage ",
             colnames(item_analysis$pt_bis_all),
             fixed = TRUE)
      item_analysis$pt_bis <-
        as.data.frame(do.call("cbind", item_analysis$stages_pt_bis))
      colnames(item_analysis$pt_bis) <-
        gsub("stage",
             "Stage ",
             colnames(item_analysis$pt_bis),
             fixed = TRUE)

      #   ______________________________________________________________________
      #   TAB: Facility Summary                                             ####
      # Create a summary of the stage separated high and low facility items
      # Set constants for the low and high facility boundary variables
      .fac_low = facility_bounds[1]
      .fac_high = facility_bounds[2]

      .tab_fac_summary <-
        data.frame(rbind(length(
          which(item_analysis$stages_facility[[.stage]] < .fac_low)
        ), length(
          which(item_analysis$stages_facility[[.stage]] > .fac_high)
        )))
      rownames(.tab_fac_summary) <-
        c(glue("Facility < {.fac_low}"),
          glue("Facility > {.fac_high}"))
      colnames(.tab_fac_summary) <- c(glue("Stage {i}"))

      # new_df[,ncol(new_df)+1]
      if (i == stages[1]) {
        item_analysis$facility_summary <- .tab_fac_summary
      } else{
        item_analysis$facility_summary <-
          cbind(item_analysis$facility_summary, .tab_fac_summary)
      }
    }
    return(item_analysis)
  }
}
