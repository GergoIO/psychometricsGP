#' Perform ANOVA, save ANOVA results, adjusted means and observed means for different stages
#'
#' @param demogData Dataframe containing demographics data columns (independent variables) and the student score (dependent variable) in another column and the relevant stage of each student row (if multiple stages are to be considered)
#' @param stages A vector denoting the stages to be considered. (stages = 2 or stages = c(2,3,4,5) etc.) If multiple stages are to be considered there must be a 'Stage' col in demogData
#' @param colScore
#' @param varsAll
#' @param varsAnova
#' @param reportObsMeanForAllVars
#'
#' @return
#' @export
#'
#' @examples
#'

################################################################################

fnAnova <-
  function(demogData = NULL,
           colScore = NULL,
           varsAll = NULL,
           varsAnova = NULL,
           stages = NULL,
           reportObsMeanForAllVars = NULL) {
    if (is.null(demogData) == TRUE |
        is.null(colScore) == TRUE |
        is.null(varsAll) == TRUE |
        is.null(varsAnova) == TRUE |
        is.null(stages) == TRUE |
        is.null(reportObsMeanForAllVars) == TRUE) {
      stop("fnAnova: One of the required variables for this function has not been specified.")
    } else{
      lReturn <- list() # For returning multiple dfs and other objects
      # Check if multiple stages are being considered
      if (length(stages) > 1) {
        # When multiple stages are being considered
        # Check if there is a 'Stage' col in the data
        if ("Stage" %in% colnames(dfDemog) == FALSE) {
          stop(
            "fnAnova: There is no 'Stages' column in demogData and there are multiple stages requested."
          )
        } else {
          # Store data separately so the original df is not modified
          dfDemog <- demogData
          # Rename the col storing the scores (needed for manual input in aov fn)
          names(dfDemog)[names(dfDemog) == colScore] <- "Score"

          demogDataStages <- split(dfDemog, dfDemog$Stage)
          names(demogDataStages) <-
            glue("Stage {sort(unique(demogData$Stage))}") # Rename each split df as "Stage *"

          # Loop over all stages
          for (i in stages) {
            loopStage <-
              glue("Stage {i}") # Get string of the current stage for col selection
            loopDemogData <- demogDataStages[[loopStage]]

            ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
            ### ANOVA                                                             ####

            aov <-
              (aov(Score ~ ., data = na.omit(loopDemogData[, c("Score", varsAnova)])))

            lReturn[[glue('RawAnovaStage{i}')]] <- .aov

            tabAov <- drop1(aov, test = "F")
            tabAov <- tabAov[-1,]
            tabAov <-
              cbind(fnRound(tabAov[, 1:5], 2), (fnRound(tabAov[, 6], 3)))
            colnames(tabAov) <-
              c("Df",
                "Sum of Sq",
                "RSS",
                "AIC",
                "F-statistic",
                "P-value")
            tabAov <- rownames_to_column(tabAov, "Factor")

            lReturn[[glue('AnovaStage{i}')]] <- tabAov

            ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
            ### Adjusted Means                                                    ####

            tabMeansAdj <- fnEmMeans(aov)

            # Precautionary measure - if cols in original input demog file have repeats, they may have suffixes like .x and .y etc. This step removes them
            tabMeansAdj <-
              data.frame(lapply(tabMeansAdj, function(x) {
                gsub("\\.x|\\.y|\\.z", "", x)
              }))
            colnames(tabMeansAdj) <-
              c("Factor", "Level", "N", "Adjusted\nMean")

            lReturn[[glue('MeansAdjStage{i}')]] <- tabMeansAdj

            ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
            ### Observed Means                                                    ####

            # Create a nested list of the number and mean scores for the variables not included in ANOVA

            if (reportObsMeanForAllVars == TRUE) {
              obsMeans <- varsAll
            } else {
              obsMeans <- setdiff(varsAll, varsAnova)
            }
            lstOfLsts <-
              apply(na.omit(loopDemogData[, obsMeans]), 2, function(x)
              {
                tapply(loopDemogData$Score, x, function(x) {
                  c(length(x), fnRound(mean(x), 2))
                })
              })

            tabObsMeans <-
              data.frame(t(data.frame(
                unlist(lstOfLsts, recursive = F)
              )))
            # Match only first '.' in rownames and replace with '*'
            rownames(tabObsMeans) <-
              sub('\\.', "\\*", rownames(tabObsMeans))
            # Replace all remaining '.' with ' '
            rownames(tabObsMeans) <-
              gsub('\\.', " ", rownames(tabObsMeans))
            tabObsMeans <-
              rownames_to_column(tabObsMeans, "Factor")
            colnames(tabObsMeans) <-
              c("Factor", "N", "Observed\nMean")
            tabObsMeans <-
              separate(
                tabObsMeans,
                col = Factor,
                into = c('Factor', 'Level'),
                sep = "\\*"
              )
            tabObsMeans$Factor[duplicated(tabObsMeans$Factor)] <- ""

            lReturn[[glue('MeansObsStage{i}')]] <- tabObsMeans
          }
          return(lReturn)
        }
      } else{
        # Insert code for anova etc when only 1 stage (or add to the data saving process)
      }
    }
  }
