#' Perform ANOVA, save ANOVA results, adjusted means and observed means for different stages
#'
#' @param demogData A dataframe - containing demographics data columns (independent variables) and the student score (dependent variable) in another column and the relevant stage of each student row (if multiple stages are to be considered)
#' @param stages A vector (OPTIONAL) - denoting the stages to be considered. (stages = 2 or stages = c(2,3,4,5) etc.) If multiple stages are to be considered there must be a 'Stage' col in demogData. If not defined, no stage separation occurs - instead the user can set the appendName variable to define what string is added to results
#' @param appendName A string (OPTIONAL) - this string is appended to all results (only if the stages variable is not set)
#' @param colScore A string - the name of the column containing the student scores (the dependent variable)
#' @param varsAll A vector of strings - all the column names of the demographic properties to consider
#' @param varsAnova A vector of strings - all the column names of the demographic properties to consider for the Anova
#' @param reportObsMeanForAllVars Boolean (TRUE/FALSE) - TRUE: Report observed means for all the demographics (varsAll). FALSE: Report observed means for only the demographics which were not included in the Anova
#'
#' @return A list of dataframes with the anova results, summarised anova results, adjusted means table and observed means table. Use the append function to add results to tabDemog if multiple fnAnova functions are to be run (see examples).
#' @export
#'
#' @examples #With stage separation:
#'     tabDemog <- append(tabDemog,fnAnova(demogData = dfDemog, stages = cnst$stages, colScore = glue("{cnst$assessment}_Score"), varsAll = c("Gender", "Ethnicity", "Disability", "Entry", "Origin"), varsAnova = c("Gender", "Ethnicity", "Disability"), reportObsMeanForAllVars = FALSE))
#' # Without stage separation:
#'     tabDemog <- append(tabDemog,fnAnova(demogData = dfDemog, colScore = glue("{cnst$assessment}_Score"), appendName = "All", varsAll = c("Gender", "Ethnicity", "Disability", "Stage", "Entry", "Origin"), varsAnova = c("Gender", "Ethnicity", "Disability", "Stage"), reportObsMeanForAllVars = FALSE)))
#'
################################################################################
#'
fnAnova <-
  function(demogData = NULL,
           colScore = NULL,
           varsAll = NULL,
           varsAnova = NULL,
           stages = NULL,
           appendName = NULL,
           reportObsMeanForAllVars = NULL) {
    if (is.null(demogData) |
        is.null(colScore) |
        is.null(varsAll) |
        is.null(varsAnova) |
        is.null(reportObsMeanForAllVars)) {
      stop("fnAnova: One of the required variables for this function has not been specified.")
    } else{
      lReturn <- list() # For returning multiple dfs and other objects
      # Check if multiple stages are being considered
      if (length(stages) > 1) {
        #   ____________________________________________________________________
        #   With Stage Separation                                           ####

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

            ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
            ### ANOVA                                                       ####

            # Check if all vars have at least two factors

            aov <-
              (aov(Score ~ ., data = na.omit(loopDemogData[, c("Score", varsAnova)])))

            # Add the raw anova result to the list of data returned
            lReturn[[glue('RawAnovaStage{i}')]] <- aov

            # Format and save Anova results
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

            ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
            ### Adjusted Means                                              ####

            tabMeansAdj <- fnEmMeans(aov)

            # Precautionary measure - if cols in original input demog file have repeats, they may have suffixes like .x and .y etc. This step removes them
            tabMeansAdj <-
              data.frame(lapply(tabMeansAdj, function(x) {
                gsub("\\.x|\\.y|\\.z", "", x)
              }))
            colnames(tabMeansAdj) <-
              c("Factor", "Level", "N", "Adjusted\nMean")

            lReturn[[glue('MeansAdjStage{i}')]] <- tabMeansAdj

            ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
            ### Observed Means                                              ####

            # Set which columns with be included in the observed means table
            if (reportObsMeanForAllVars) {
              obsMeans <- varsAll
            } else {
              obsMeans <- setdiff(varsAll, varsAnova)
            }

            # Create a nested list of the number and mean scores for selected demographics
            lstOfLsts <-
              apply(loopDemogData[, obsMeans], 2, function(x)
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
      } else {
        #   ____________________________________________________________________
        #   No Stage Separation                                             ####

        # Store data separately so the original df is not modified
        dfDemog <- demogData
        # Rename the col storing the scores (needed for manual input in aov fn)
        names(dfDemog)[names(dfDemog) == colScore] <- "Score"

        # If Stage is still requested to be added to the ANOVA (though not including stage separation),
        # Then modify the values so they are characters and not numeric so the anova picks them up properly
        dfDemog <- dfDemog %>%
          mutate(Stage = if_else(exists("Stage"), paste0("Stage ", Stage), Stage))

        ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
        ### ANOVA                                                           ####

        aov <-
          (aov(Score ~ ., data = na.omit(dfDemog[, c("Score", varsAnova)])))

        # Add the raw anova result to the list of data returned
        lReturn[[glue('RawAnova{appendName}')]] <- aov

        # Format and save Anova results
        tabAov <- drop1(aov, test = "F")
        tabAov <- tabAov[-1, ]
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

        lReturn[[glue('Anova{appendName}')]] <- tabAov

        ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
        ### Adjusted Means                                                  ####

        tabMeansAdj <- fnEmMeans(aov)

        # Precautionary measure - if cols in original input demog file have repeats, they may have suffixes like .x and .y etc. This step removes them
        tabMeansAdj <-
          data.frame(lapply(tabMeansAdj, function(x) {
            gsub("\\.x|\\.y|\\.z", "", x)
          }))
        colnames(tabMeansAdj) <-
          c("Factor", "Level", "N", "Adjusted\nMean")

        lReturn[[glue('MeansAdj{appendName}')]] <- tabMeansAdj

        ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
        ### Observed Means                                                  ####

        # Set which columns with be included in the observed means table
        if (reportObsMeanForAllVars) {
          obsMeans <- varsAll
        } else {
          obsMeans <- setdiff(varsAll, varsAnova)
        }

        # Create a nested list of the number and mean scores for selected demographics
        lstOfLsts <-
          apply(dfDemog[, obsMeans], 2, function(x)
          {
            tapply(dfDemog$Score, x, function(x) {
              c(length(x), fnRound(mean(x), 2))
            })
          })

        tabObsMeans <-
          data.frame(t(data.frame(unlist(
            lstOfLsts, recursive = F
          ))))
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

        lReturn[[glue('MeansObs{appendName}')]] <- tabObsMeans
        return(lReturn)
      }
    }
  } # END

# Return to later - checking which demographics should be included as part of anova

# https://www.datanovia.com/en/lessons/anova-in-r/
# ddemog1 <-
#   na.omit(dfDemog[, c("ScoreTotal", "Gender", "Ethnicity", "Disability")])
#
# install.packages("rstatix")
# library(rstatix)
#
# dSummary <- ddemog1 %>%
#   group_by(Gender, Ethnicity) %>%
#   get_summary_stats(ScoreTotal, type = "mean_sd")
#
# dSummaryGender <- ddemog1 %>%
#   group_by(Gender) %>%
#   get_summary_stats(ScoreTotal, type = "mean_sd")
#
# dSummaryEthnicity <- ddemog1 %>%
#   group_by(Ethnicity) %>%
#   get_summary_stats(ScoreTotal, type = "mean_sd")
#
# dSummaryDisability <- ddemog1 %>%
#   group_by(Disability) %>%
#   get_summary_stats(ScoreTotal, type = "mean_sd")
#
#
# dOutlier <- ddemog1 %>%
#   group_by(Gender, Ethnicity, Disability) %>%
#   identify_outliers(ScoreTotal)
#
# dShapiro <- ddemog1 %>%
#   group_by(Gender, Ethnicity) %>%
#   shapiro_test(ScoreTotal)
#
# dLevene <-
#   ddemog1 %>% levene_test(ScoreTotal ~ Gender * Ethnicity * Disability)
#
