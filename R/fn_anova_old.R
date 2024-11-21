#' Perform ANOVA, save ANOVA results, adjusted means and observed means for different stages
#'
#' @param data_demog A dataframe - containing demographics data columns (independent variables) and the student score (dependent variable) in another column and the relevant stage of each student row (if multiple stages are to be considered)
#' @param stages A vector (OPTIONAL, if append_name defined. Must define is Stage is to be one of the ANOVA vars) - denoting the stages to be considered. (stages = 2 or stages = c(2,3,4,5) etc.) If multiple stages are to be considered there must be a 'Stage' col in data_demog. If not defined, no stage separation occurs - instead the user must set the append_name variable to define what string is added to results names. Additionally, if no stage separation but Stage is to be one of the ANOVA variables, 'stages_for_filtering' must be defined.
#' @param stages_for_filtering A vector (OPTIONAL, must define if Stage is to be one of the ANOVA vars and stage separation is not requested) - denoting the stages to be considered in the ANOVA. (stages_for_filtering = 2 or stages_for_filtering = c(2,3,4,5) etc.) If multiple stages are to be considered there must be a 'Stage' col in data_demog.
#' @param append_name A string (OPTIONAL, REQUIRED if stages is not defined) - this string is appended to all results (only if the stages variable is not set)
#' @param col_score A string - the name of the column containing the student scores (the dependent variable)
#' @param vars_all A vector of strings - all the column names of the demographic properties to consider
#' @param vars_anova A vector of strings - all the column names of the demographic properties to consider for the Anova
#' @param report_observed_mean_for_all_vars Boolean (TRUE/FALSE) - TRUE: Report observed means for all the demographics (vars_all). FALSE: Report observed means for only the demographics which were not included in the Anova
#'
#' @return A list of dataframes with the anova results, summarised anova results, adjusted means table and observed means table. Use the append function to add results to tabDemog if multiple fn_anova functions are to be run (see examples).
#' @export
#'
#' @examples #With stage separation:
#'     tabDemog <- append(tabDemog,fn_anova(data_demog = dfDemog, stages = cnst$stages, col_score = glue("{cnst$assessment}_Score"), vars_all = c("Gender", "Ethnicity", "Disability", "Entry", "Origin"), vars_anova = c("Gender", "Ethnicity", "Disability"), report_observed_mean_for_all_vars = FALSE))
#' # Without stage separation:
#'     tabDemog <- append(tabDemog,fn_anova(data_demog = dfDemog, col_score = glue("{cnst$assessment}_Score"), append_name = "All", vars_all = c("Gender", "Ethnicity", "Disability", "Stage", "Entry", "Origin"), vars_anova = c("Gender", "Ethnicity", "Disability", "Stage"), report_observed_mean_for_all_vars = FALSE)))
#'
################################################################################
#'
fn_anova_old <-
  function(data_demog,
           col_score,
           vars_all,
           vars_anova,
           stages = NULL,
           stages_for_filtering = NULL,
           append_name = NULL,
           report_observed_mean_for_all_vars) {
    lReturn <- list() # For returning multiple dfs and other objects
    # Check if multiple stages are being considered

    if (is.null(stages) &
        is.null(append_name)) {
      stop("fn_anova: If 'stages' is not defined, then 'append_name' must be defined.")
    } else{
      if (length(stages) > 1) {
        #   ____________________________________________________________________
        #   With Stage Separation                                           ####

        # When multiple stages are being considered
        # Check if there is a 'Stage' col in the data
        if ("Stage" %in% colnames(dfDemog) == FALSE) {
          stop(
            "fn_anova: There is no 'Stages' column in data_demog and there are multiple stages requested."
          )
        } else {
          # Store data separately so the original df is not modified
          dfDemog <- data_demog |>
            # Rename the col storing the scores (needed for manual input in aov fn)
            rename(Score = col_score)

          demogDataStages <- split(dfDemog, dfDemog$Stage)
          names(demogDataStages) <-
            glue("Stage {sort(unique(data_demog$Stage))}") # Rename each split df as "Stage *"

          # Loop over all stages
          for (i in stages) {
            loopStage <-
              glue("Stage {i}") # Get string of the current stage for col selection
            loopDemogData <- demogDataStages[[loopStage]]

            ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
            ### ANOVA                                                       ####

            # Check if all vars have at least two factors

            aov <-
              (aov(Score ~ ., data = na.omit(loopDemogData[, c("Score", vars_anova)])))

            # Add the raw anova result to the list of data returned
            lReturn[[glue('RawAnovaStage{i}')]] <- aov

            # Format and save Anova results
            tabAov <- drop1(aov, test = "F")
            tabAov <- tabAov[-1,]
            tabAov <-
              cbind(fnGPRound(tabAov[, 1:5], 2), (fnGPRound(tabAov[, 6], 3)))
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

            tabMeansAdj <- fnGPEmMeans(aov)

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
            if (report_observed_mean_for_all_vars) {
              obsMeans <- vars_all
            } else {
              obsMeans <- setdiff(vars_all, vars_anova)
            }

            # Create a nested list of the number and mean scores for selected demographics
            lstOfLsts <-
              apply(loopDemogData[, obsMeans], 2, function(x)
              {
                tapply(loopDemogData$Score, x, function(x) {
                  c(length(x), fnGPRound(mean(x), 2))
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

        # If only a single stage is defined in 'stages' and 'append_name' is NOT defined
        if (is.null(append_name)) {
          append_name <- glue("Stage{stages}")
        }

        # Store data separately so the original df is not modified
        dfDemog <- data_demog %>%
          # Rename the col storing the scores (needed for manual input in aov fn)
          rename(Score = col_score)
        # names(dfDemog)[names(dfDemog) == col_score] <- "Score"

        # If Stage is still requested to be added to the ANOVA (though not including stage separation),
        # Then modify the values so they are characters and not numeric so the anova picks them up properly
        if ("Stage" %in% vars_anova) {
          if (is.null(stages_for_filtering)) {
            stop(
              "fn_anova: 'Stage' is set to be one of the ANOVA vars and stage separation is not requested.\nBut, the 'stages_for_filtering' variable is not defined.\nThe 'stages_for_filtering' variable must be defined to perform the requested action."
            )
          } else{
            dfDemog <-
              dfDemog |>
              # Still filter to make sure that only the specified stage exists in dfDemog
              filter(Stage %in% stages_for_filtering) |>
              mutate(Stage = paste0("Stage ", Stage))
          }
        }

        ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
        ### ANOVA                                                           ####

        aov <-
          (aov(Score ~ ., data = na.omit(dfDemog[, c("Score", vars_anova)])))

        # Add the raw anova result to the list of data returned
        lReturn[[glue('RawAnova{append_name}')]] <- aov

        # Format and save Anova results
        tabAov <- drop1(aov, test = "F")
        tabAov <- tabAov[-1, ]
        tabAov <-
          cbind(fnGPRound(tabAov[, 1:5], 2), (fnGPRound(tabAov[, 6], 3)))
        colnames(tabAov) <-
          c("Df",
            "Sum of Sq",
            "RSS",
            "AIC",
            "F-statistic",
            "P-value")
        tabAov <- rownames_to_column(tabAov, "Factor")

        lReturn[[glue('Anova{append_name}')]] <- tabAov

        ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
        ### Adjusted Means                                                  ####

        tabMeansAdj <- fnGPEmMeans(aov)

        # Precautionary measure - if cols in original input demog file have repeats, they may have suffixes like .x and .y etc. This step removes them
        tabMeansAdj <-
          data.frame(lapply(tabMeansAdj, function(x) {
            gsub("\\.x|\\.y|\\.z", "", x)
          }))
        colnames(tabMeansAdj) <-
          c("Factor", "Level", "N", "Adjusted\nMean")
        # Manual fix for Stage section which has a triple repeated of "Stage"
        tabMeansAdj <-
          tabMeansAdj %>%  mutate(Level = gsub("Stage Stage Stage", "Stage", Level))

        lReturn[[glue('MeansAdj{append_name}')]] <- tabMeansAdj

        ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
        ### Observed Means                                                  ####

        # Set which columns with be included in the observed means table
        if (report_observed_mean_for_all_vars) {
          obsMeans <- vars_all
        } else {
          obsMeans <- setdiff(vars_all, vars_anova)
        }

        # Create a nested list of the number and mean scores for selected demographics
        lstOfLsts <-
          apply(dfDemog[, obsMeans], 2, function(x)
          {
            tapply(dfDemog$Score, x, function(x) {
              c(length(x), fnGPRound(mean(x), 2))
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

        lReturn[[glue('MeansObs{append_name}')]] <- tabObsMeans
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
