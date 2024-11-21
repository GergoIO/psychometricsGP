#' Test Reliability Parameters (Non/Stage Specific)
#'
#' @param stages (Optional) Vector containing all the stages being considered. If undefined, only a single dataset (data_scores) is expected/considered.
#' @param data_scores If stage specific: A list of dataframes with students scores for each stage. Each data frame in the list should be named "stage#" where # is the relevant stage.
#' If non stage specific: A single dataframe containing what would otherwise be contained in one of the dataframes in the list of stage specific dataframes. In any dataframe the columns should be the items and the rows should be the index of each student (actual value unimportant). Each data point should represent the score for a particular student on a particular item. All students with NA scores and excluded items should be removed prior to use in this function
#' @param data_results A dataframe containing student results. This variable must contain a "Stage" column in which the stage of each student is entered in a new row. This is used to add the number of students assessed in each Stage to the resultant export of reliability data.
#' @param items_included (Integer) The number of items included in the exam. Usually stored under l_exam_vars$n_items_incl
#'
#' @return A dataframe containing the reliability data for each considered stage is returned
#' @export
#'
#' @examples fn_reliability(stages = cnst$stages, data_scores = df_res_stages_only_scores, items_included = l_exam_vars$n_items_incl)

################################################################################

fn_reliability <-  function(stages = NULL,
                            data_scores = NULL,
                            data_results = NULL,
                            items_included = NULL) {
  rel_calcs = list() # To save intermediary calculations which are not returned as part of the function
  reliability = data.frame(matrix(nrow = 9, ncol = 0), stringsAsFactors = FALSE) # To save final reliability calculations

  if (is.null(data_scores) | is.null(data_results)) {
    stop("One of the required variables for this function has not been specified.")
  }
  if (is.null(stages)) {
    message("fn_reliability: Analysing reliability across a single dataset (single stage)")
    scores_long <-
      transform(data_scores, Candidate = c(1:nrow(data_scores)))
    scores_long <-
      melt(scores_long, id = "Candidate")
    colnames(scores_long) <- c("Candidate", "Item", "Score")
    scores_long$Item <-
      gsub("X", "", scores_long$Item, fixed = TRUE)

    fit_linear_mix_effect_model <-
      lmer(Score ~ (1 |
                      Candidate) + (1 |
                                      Item), data = scores_long)

    rel_calcs$var_corr <-
      as.data.frame(VarCorr(fit_linear_mix_effect_model))
    rownames(rel_calcs$var_corr) <- rel_calcs$var_corr$grp
    rel_calcs$var_corr$perc <-
      rel_calcs$var_corr$vcov / sum(rel_calcs$var_corr$vcov) * 100

    cv <-
      rel_calcs$var_corr["Candidate", 4] #Candidate Variances or Covariance
    iv <-
      rel_calcs$var_corr["Item", 4] #Item Variances or Covariance
    rv <-
      rel_calcs$var_corr["Residual", 4] #Residual Variances or Covariance
    rel_calcs$phi <-
      cv / (cv + (iv / items_included) + (rv / items_included))
    rel_calcs$g <- cv / (cv + (rv / items_included))
    rel_calcs$sem_abs <-
      100 * sqrt((iv / items_included) + (rv / items_included))
    rel_calcs$sem_rel <- 100 * sqrt(rv / items_included)
    rel_calcs$variance_candidate <-
      rel_calcs$var_corr["Candidate", 6]
    rel_calcs$variance_item <- rel_calcs$var_corr["Item", 6]
    rel_calcs$variance_residual <-
      rel_calcs$var_corr["Residual", 6]

    rownames(reliability) <-
      c(
        "Students Assessed",
        "Cronbach's Alpha",
        "Variance (%) due to candidates",
        "Variance (%) due to items",
        "Variance (%) residual",
        "G coefficient",
        "Relative SEM",
        "Phi Coefficient",
        "Absolute SEM"
      )
    reliability[, 1] <- c(
      dim(data_results)[1],
      cronbach.alpha(data_scores)[[1]],
      rel_calcs$variance_candidate,
      rel_calcs$variance_item,
      rel_calcs$variance_residual,
      rel_calcs$g,
      rel_calcs$sem_rel,
      rel_calcs$phi,
      rel_calcs$sem_abs
    )
    colnames(reliability) <- "Value"
  } else{
    message(glue("fn_reliability: Analysing reliability across multiple stage(s): {stages}"))
    for (i in stages) {
      stage <- glue("stage{i}")
      stage_col_name <- glue("Stage {i}")
      loop_df <- data_scores[[stage]]

      loop_scores_long <-
        transform(loop_df, Candidate = c(1:nrow(loop_df)))
      loop_scores_long <-
        melt(loop_scores_long, id = "Candidate")
      colnames(loop_scores_long) <- c("Candidate", "Item", "Score")
      loop_scores_long$Item <-
        gsub("X", "", loop_scores_long$Item, fixed = TRUE)

      fit_linear_mix_effect_model <-
        lmer(Score ~ (1 |
                        Candidate) + (1 |
                                        Item), data = loop_scores_long)

      rel_calcs$var_corr <-
        as.data.frame(VarCorr(fit_linear_mix_effect_model))
      rownames(rel_calcs$var_corr) <- rel_calcs$var_corr$grp
      rel_calcs$var_corr$perc <-
        rel_calcs$var_corr$vcov / sum(rel_calcs$var_corr$vcov) * 100

      cv <-
        rel_calcs$var_corr["Candidate", 4] #Candidate Variances or Covariance
      iv <-
        rel_calcs$var_corr["Item", 4] #Item Variances or Covariance
      rv <-
        rel_calcs$var_corr["Residual", 4] #Residual Variances or Covariance
      rel_calcs$phi <-
        cv / (cv + (iv / items_included) + (rv / items_included))
      rel_calcs$g <- cv / (cv + (rv / items_included))
      rel_calcs$sem_abs <-
        100 * sqrt((iv / items_included) + (rv / items_included))
      rel_calcs$sem_rel <- 100 * sqrt(rv / items_included)
      rel_calcs$variance_candidate <-
        rel_calcs$var_corr["Candidate", 6]
      rel_calcs$variance_item <- rel_calcs$var_corr["Item", 6]
      rel_calcs$variance_residual <-
        rel_calcs$var_corr["Residual", 6]

      rownames(reliability) <-
        c(
          "Students Assessed",
          "Cronbach's Alpha",
          "Variance (%) due to candidates",
          "Variance (%) due to items",
          "Variance (%) residual",
          "G coefficient",
          "Relative SEM",
          "Phi Coefficient",
          "Absolute SEM"
        )
      reliability[, stage_col_name] <- c(
        dim(data_results[data_results$Stage == i, ])[1],
        cronbach.alpha(data_scores[[stage]])[[1]],
        rel_calcs$variance_candidate,
        rel_calcs$variance_item,
        rel_calcs$variance_residual,
        rel_calcs$g,
        rel_calcs$sem_rel,
        rel_calcs$phi,
        rel_calcs$sem_abs
      )
    }
  }
  return(reliability)
} # END
