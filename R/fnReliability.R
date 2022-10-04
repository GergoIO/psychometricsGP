#' Test Reliability Parameters (Non/Stage Specific)
#'
#' @param stages (Optional) Vector containing all the stages being considered. If undefined, only a single dataset (scoresData) is expected/considered.
#' @param scoresData If stage specific: A list of dataframes with students scores for each stage. Each data frame in the list should be named "Stage #" where # is the relevant stage.
#'    If non stage specific: A single dataframe contaning what would otherwise be contained in one of the dataframes in the list of stage specific dataframes. In any dataframe the columns should be the items and the rows should be the index of each student (actual value unimportant). Each data point should represent the score for a particular student on a particular item. All students with NA scores and excluded items should be removed prior to use in this function
#' @param resultsData A dataframe containing student results. This variable must contain a "Stage" column in which the stage of each student is entered in a new row. This is used to add the number of students assessed in each Stage to the resultant export of reliability data.
#'
#' @return A dataframe containing the reliability data for each considered stage is returned
#' @export
#'
#' @examples fnReliability(stages = cnst$stages, scoresData = dfResStagesOnlyScores)

################################################################################

fnReliability <-  function(stages = NULL,
                           scoresData = NULL,
                           resultsData = NULL) {
  relCalcs = list() # To save intermediary calculations which are not returned as part of the function
  reliability = data.frame(matrix(nrow = 9, ncol = 0), stringsAsFactors = FALSE) # To save final reliability calculations

  if (is.null(scoresData) == TRUE | is.null(resultsData) == TRUE) {
    stop("One of the required variables for this function has not been specified.")
  }
  if (is.null(stages) == TRUE) {
    message("fnReliability: Analysing reliability across a single dataset (single stage)")
    loopDf <- scoresData

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
    reliability <- c(
      dim(resultsData)[1],
      cronbach.alpha(scoresData)[[1]],
      relCalcs$varianceCandidate,
      relCalcs$varianceItem,
      relCalcs$varianceResidual,
      relCalcs$g,
      relCalcs$semRel,
      relCalcs$phi,
      relCalcs$semAbs
    )
  } else{
    message("fnReliability: Analysing reliability across multiple stages")
    for (i in stages) {
      stage <- glue("Stage {i}")
      loopDf <- scoresData[[stage]]

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
      reliability[, stage] <- c(
        dim(resultsData[resultsData$Stage == i,])[1],
        cronbach.alpha(scoresData[[stage]])[[1]],
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
