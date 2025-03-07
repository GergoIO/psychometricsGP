#' Test Reliability Parameters (Non/Stage Specific)
#'
#' @param stages (Optional) Vector containing all the stages being considered. If undefined, only a single dataset (data_scores) is expected/considered.
#' @param data_scores If stage specific: A list of dataframes with students scores for each stage. Each data frame in the list should be named "stage#" where # is the relevant stage.
#' If non stage specific: A single dataframe containing what would otherwise be contained in one of the dataframes in the list of stage specific dataframes. In any dataframe the columns should be the items and the rows should be the index of each student (actual value unimportant). Each data point should represent the score for a particular student on a particular item. All students with NA scores and excluded items should be removed prior to use in this function
#' @param data_results A dataframe containing student results. This variable must contain a "Stage" column in which the stage of each student is entered in a new row. This is used to add the number of students assessed in each Stage to the resultant export of reliability data.
#'
#' @return A dataframe containing the reliability data for each considered stage is returned
#' @export
#'
#' @examples fnReliability(stages = cnst$stages, data_scores = dfResStagesOnlyScores)

################################################################################

fnReliability <-  function(stages = NULL,
                           data_scores = NULL,
                           data_results = NULL) {
  relCalcs = list() # To save intermediary calculations which are not returned as part of the function
  reliability = data.frame(matrix(nrow = 9, ncol = 0), stringsAsFactors = FALSE) # To save final reliability calculations

  if (is.null(data_scores) | is.null(data_results)) {
    stop("One of the required variables for this function has not been specified.")
  }
  if (is.null(stages)) {
    message("fnReliability: Analysing reliability across a single dataset (single stage)")
    scoresLong <-
      transform(data_scores, Candidate = c(1:nrow(data_scores)))
    scoresLong <-
      melt(scoresLong, id = "Candidate")
    colnames(scoresLong) <- c("Candidate", "Item", "Score")
    scoresLong$Item <-
      gsub("X", "", scoresLong$Item, fixed = TRUE)

    fitLinearMixEffectModel <-
      lmer(Score ~ (1 | Candidate) + (1 | Item), data = scoresLong)

    relCalcs$varCorr <-
      as.data.frame(VarCorr(fitLinearMixEffectModel))
    rownames(relCalcs$varCorr) <- relCalcs$varCorr$grp
    relCalcs$varCorr$perc <-
      relCalcs$varCorr$vcov / sum(relCalcs$varCorr$vcov) * 100

    cv <-
      relCalcs$varCorr["Candidate", 4] #Candidate Variances or Covariance
    iv <-
      relCalcs$varCorr["Item", 4] #Item Variances or Covariance
    rv <-
      relCalcs$varCorr["Residual", 4] #Residual Variances or Covariance
    relCalcs$phi <-
      cv / (cv + (iv / cnst$nItemsIncl) + (rv / cnst$nItemsIncl))
    relCalcs$g <- cv / (cv + (rv / cnst$nItemsIncl))
    relCalcs$semAbs <-
      100 * sqrt((iv / cnst$nItemsIncl) + (rv / cnst$nItemsIncl))
    relCalcs$semRel <- 100 * sqrt(rv / cnst$nItemsIncl)
    relCalcs$varianceCandidate <-
      relCalcs$varCorr["Candidate", 6]
    relCalcs$varianceItem <- relCalcs$varCorr["Item", 6]
    relCalcs$varianceResidual <-
      relCalcs$varCorr["Residual", 6]

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
      relCalcs$varianceCandidate,
      relCalcs$varianceItem,
      relCalcs$varianceResidual,
      relCalcs$g,
      relCalcs$semRel,
      relCalcs$phi,
      relCalcs$semAbs
    )
    colnames(reliability) <- "Value"
  } else{
    message("fnReliability: Analysing reliability across multiple stages")
    for (i in stages) {
      stage <- glue("stage{i}")
      stageColName <- glue("Stage {i}")
      loopDf <- data_scores[[stage]]

      loopScoresLong <-
        transform(loopDf, Candidate = c(1:nrow(loopDf)))
      loopScoresLong <-
        melt(loopScoresLong, id = "Candidate")
      colnames(loopScoresLong) <- c("Candidate", "Item", "Score")
      loopScoresLong$Item <-
        gsub("X", "", loopScoresLong$Item, fixed = TRUE)

      fitLinearMixEffectModel <-
        lmer(Score ~ (1 |
                        Candidate) + (1 |
                                        Item), data = loopScoresLong)

      relCalcs$varCorr <-
        as.data.frame(VarCorr(fitLinearMixEffectModel))
      rownames(relCalcs$varCorr) <- relCalcs$varCorr$grp
      relCalcs$varCorr$perc <-
        relCalcs$varCorr$vcov / sum(relCalcs$varCorr$vcov) * 100

      cv <-
        relCalcs$varCorr["Candidate", 4] #Candidate Variances or Covariance
      iv <-
        relCalcs$varCorr["Item", 4] #Item Variances or Covariance
      rv <-
        relCalcs$varCorr["Residual", 4] #Residual Variances or Covariance
      relCalcs$phi <-
        cv / (cv + (iv / cnst$nItemsIncl) + (rv / cnst$nItemsIncl))
      relCalcs$g <- cv / (cv + (rv / cnst$nItemsIncl))
      relCalcs$semAbs <-
        100 * sqrt((iv / cnst$nItemsIncl) + (rv / cnst$nItemsIncl))
      relCalcs$semRel <- 100 * sqrt(rv / cnst$nItemsIncl)
      relCalcs$varianceCandidate <-
        relCalcs$varCorr["Candidate", 6]
      relCalcs$varianceItem <- relCalcs$varCorr["Item", 6]
      relCalcs$varianceResidual <-
        relCalcs$varCorr["Residual", 6]

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
      reliability[, stageColName] <- c(
        dim(data_results[data_results$Stage == i, ])[1],
        cronbach.alpha(data_scores[[stage]])[[1]],
        relCalcs$varianceCandidate,
        relCalcs$varianceItem,
        relCalcs$varianceResidual,
        relCalcs$g,
        relCalcs$semRel,
        relCalcs$phi,
        relCalcs$semAbs
      )
    }
  }
  return(reliability)
} # END
