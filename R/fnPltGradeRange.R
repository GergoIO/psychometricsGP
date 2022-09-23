#' Plot: Grade ranges for scores across multiple stages
#'
#' @param data A dataframe containing score data. For each group (stage) to be plotted, values should be added to a new column in the following order: First - minimum achieved score, then - grade boundaries in increasing order, finally - the maximum achieved score
#' @param grades The possible grades. Currently either "UBSE" (default) or "US
#' @param xLabels A vector (of strings or numerics). The labels to be added along the x-axis for each bar. Must be the same length as the number of columns (separate groups/stages) in the 'data' variable
#' @param axisTitles A vector (length 2), the x and the y axis titles (in that order)
#'
#' @return A plot of the grade ranges for scores across multiple stages is returned
#' @export
#'
#' @examples
#'
#' testDataUBSE <-
#'data.frame(
#'  # Order: Min, B bound., S bound., E bound., Max
#'  c(30, 4.2, 7.12, 38.23, 54),
#'  c(30, 22.6, 40.2, 62.84, 72),
#'  c(NA, 34.26, 45.45, 69.06, 78.25),
#'  c(20, 38, 52.2, 75.3, 85.2),
#'  row.names = c(
#'    'Minimum',
#'    'Borderline boundary',
#'    'Satisfactory boundary',
#'    'Excellent boundary',
#'    'Maximum'
#'  )
#')
#'
#'testDataUS <-
#'  data.frame(
#'    c(20.93, 25.1, 49.80),
#'    c(20.93, NA, 49.80),
#'    c(-41.8, 49.2, 68.8),
#'    row.names = c("Minimum", "Satisfactory boundary", "Maximum")
#'  )
#'
#'fnPltGradeRange(
#'  data = testDataUBSE,
#'  grades = "UBSE",
#'  xLabels = c(1, 2, 3, 4),
#'  axisTitles = c("Stage", "Score (%)")
#')
#'
#'fnPltGradeRange(
#'  data = testDataUS,
#'  grades = "US",
#'  xLabels = c(1, 2),
#'  axisTitles = c("Stage", "Score (%)")
#')
#'
################################################################################
#'

fnPltGradeRange <-
  function(data = NULL,
           grades = "UBSE",
           xLabels = NULL,
           axisTitles = NULL) {
    #   ________________________________________________________________________
    #   Check all required variables are defined                            ####

    if (is.null(data) == TRUE |
        is.null(xLabels) == TRUE |
        is.null(axisTitles) == TRUE)
    {
      stop("One of the required variables for this function has not been specified.")
    } else {
      #   ______________________________________________________________________
      #   Ensure Data is Numeric                                            ####

      data <- mutate_all(data, function(x) as.numeric(as.character(x)))

      #   ______________________________________________________________________
      #   Define Constants                                                  ####

      # Get a vector of possible grades ("U", "B", "S", "E") or ("U", "S")
      gradesLong = unlist(strsplit(grades, split = ""))

      # Get the number of grades in each group (stage)
      nGradesPerGroup = length(gradesLong)

      # Get the number of groups (separate bars) to be plotted, usually the number of stages
      nGroups <- dim(data)[2]

      if (nGroups != length(xLabels)) {
        stop(
          "fnPltGradeRange: The x-axis labels variable (xLabels) is not the same length as the number of groups (stages) provided for plotting. The number of defined x-axis labels should match the number of columns provided in the data variable."
        )
      }

      #   ______________________________________________________________________
      #   Define Functions                                                  ####

      # Define rounding function - https://stackoverflow.com/a/32508105
      fnRounder <- function(x, y) {
        if (y >= 0) {
          x + (y - x %% y)
        }
        else {
          x - (x %% abs(y))
        }
      }

      # -ve y rounds down.
      # using y = -20 to round down to the next lowest multiple of 20
      # this keeps the method of using a break separation of 20 working
      # eg fnRounder(-3.4, -20) = -20
      # Function for use as part of the function fnPltGradeRange
      # This function removes bar segments below the min achieved score for each group (stage)
      # Input data has columns: group (the stage for each bar),
      # yMin and yMax (The min and max y values of each bar segment) and
      # Grade which is the grade assigned to each bar segment

      # Within each group (stage)
      # If in a row yMin < yMax, set the next row's yMin to the current row's yMin and
      # also set the current yMin and yMax to NA (so they aren't plotted) yMin = yMax = NA
      # This propagates the min score through so the bottom of the lowest bar is at the actual min achieved val
      # It may be that noone was grated U so there need not be a U bar
      # Instead the bar should start from the actual min achieved in B (not from the B boundary)
      fnSetMin <- function(data, nGroups, nGradesPerGroup) {
        for (i in 1:nGroups) {
          # Subset a single group (stage)
          dfSubset <- data[data$group == i,]

          for (j in 1:nGradesPerGroup) {
            if (is.na(dfSubset[j, "yMin"]) == TRUE |
                is.na(dfSubset[j, "yMax"]) == TRUE) {
              # Do Nothing - If any cells in the row are NA
            }
            else if (as.numeric(dfSubset[j, "yMin"]) <= as.numeric(dfSubset[j, "yMax"])) {
              # Do Nothing - If the Min value is <= to the Max val
            } else{
              # Otherwise - Set the following row's min to the current min and both current min and max to NA
              dfSubset[j + 1, "yMin"] <- dfSubset[j, "yMin"]
              dfSubset[j, "yMin"] <- dfSubset[j, "yMax"] <- NA
            }
          }

          # Combine each of the individual groups back to the same format as the original dataset
          # Create the combined df first time round then add subsequent ones to it
          if (i == 1) {
            dfCombined <- dfSubset
          } else {
            dfCombined <- rbind(dfCombined, dfSubset)
          }
        }
        return(dfCombined)
      }

      #   ______________________________________________________________________
      #   Grades: UBSE                                                      ####

      if (grades == "UBSE") {
        # Create a df of the data required for plotting
        dataPlt <- data.frame(
          # For each grade box to be plotted, add the group (stage) that this corresponds to
          # Eg for 2 stages graded UBSE this col will be 1,1,1,1,2,2,2,2
          # Eg for 3 stages graded US this col will be 1,1,2,2,3,3
          group = sort(rep(1:nGroups, nGradesPerGroup)),
          # 1:4 covers Min - Excellent
          yMin = unlist(data[1:4,], use.names = FALSE),
          # 2-5 covers Borderline - Max
          yMax = unlist(data[2:5,], use.names = FALSE),
          Grade = rep(unlist(gradesLong), nGroups)
        )

        # Use a function to remove bar segments below the min achieved score
        dataPlt <- fnSetMin(data = dataPlt,
                            nGroups = 4,
                            nGradesPerGroup = nGradesPerGroup)

        dataPlt[["Grade"]] <- factor(dataPlt[["Grade"]],
                                     levels = c("E", "S", "B", "U"),
                                     ordered = TRUE)

        # Set min y axis limit
        # If all min scores are >=0, set to 0. Otherwise, round the min value to the nearest 20 and subtract 20 so it is definitely included. Eg for a min yMin value of -3 the min y axis limit will be -20 and for a min yMin value of -32 (unlikely!) it would be -40.
        limitMinY <- ifelse(min(dataPlt$yMin, na.rm = TRUE) >= 0,
                            0,
                            fnRounder(min(dataPlt$yMin, na.rm = TRUE), -20))

        # PLOT UBSE
        plot <- ggplot(dataPlt,
                       aes(
                         xmin = group - 0.45,
                         xmax = group + 0.45,
                         ymin = yMin,
                         ymax = yMax
                       )) +
          geom_rect(aes(fill = Grade), colour = "black") +
          scale_fill_manual(
            values = c("#3D52A1", # Blue/Green/Orange/Red
                       "#86BB6A",
                       "#E68B33",
                       "#D92120"),
            breaks = c("E", "S", "B", "U"),
            labels = c(
              "Excellent",
              "Satisfactory",
              "Borderline",
              "Unsatisfactory"
            )
          ) + xlab(axisTitles[1]) + ylab(axisTitles[2]) +
          scale_y_continuous(
            limits = c(limitMinY, 100),
            breaks = seq(limitMinY, 100, 20),
            expand = c(0, 0)
          ) +
          scale_x_continuous(
            limits = c(0.5, nGroups + 0.5),
            breaks = c(1:nGroups),
            labels = xLabels,
            expand = c(0, 0)
          )

        #   ____________________________________________________________________
        #   Grades: US                                                      ####

      } else if (grades == "US") {
        # Create a df of the data required for plotting
        dataPlt <- data.frame(
          # For each grade box to be plotted, add the group (stage) that this corresponds to
          # Eg for 2 stages graded UBSE this col will be 1,1,1,1,2,2,2,2
          # Eg for 3 stages graded US this col will be 1,1,2,2,3,3
          group = sort(rep(1:nGroups, nGradesPerGroup)),
          # 1:2 covers Min - Satisfactory
          yMin = unlist(data[1:2,], use.names = FALSE),
          # 2-3 covers Satisfactory - Max
          yMax = unlist(data[2:3,], use.names = FALSE),
          Grade = rep(unlist(gradesLong), nGroups)
        )

        # Use a function to remove bar segments below the min achieved score
        dataPlt <- fnSetMin(data = dataPlt,
                            nGroups = nGroups,
                            nGradesPerGroup = nGradesPerGroup)

        dataPlt[["Grade"]] <- factor(dataPlt[["Grade"]],
                                     levels = c("S", "U"),
                                     ordered = TRUE)

        # Set min y axis limit (See UBSE section for more details)
        limitMinY <- ifelse(min(dataPlt$yMin, na.rm = TRUE) >= 0,
                            0,
                            fnRounder(min(dataPlt$yMin, na.rm = TRUE), -20))

        # PLOT US
        plot <- ggplot(dataPlt,
                       aes(
                         xmin = group - 0.45,
                         xmax = group + 0.45,
                         ymin = yMin,
                         ymax = yMax
                       )) +
          geom_rect(aes(fill = Grade), colour = "black") +
          scale_fill_manual(
            # Green/Blue
            values = c("#86BB6A", "#D92120"),
            breaks = c("S", "U"),
            labels = c("Satisfactory",
                       "Unsatisfactory")
          ) + xlab(axisTitles[1]) + ylab(axisTitles[2]) +
          scale_y_continuous(
            limits = c(limitMinY, 100),
            breaks = seq(limitMinY, 100, 20),
            expand = c(0, 0)
          ) +
          scale_x_continuous(
            limits = c(0.5, nGroups + 0.5),
            breaks = c(1:nGroups),
            labels = xLabels,
            expand = c(0, 0)
          )
      }
      # print(dataPlt)
      if (any(is.na(dataPlt) == TRUE)) {
        message(
          "fnPltGradeRange: One of the score extremes or grade boundaries has not been defined. The plot will be incomplete. Check that all input data values are defined."
        )
      }
      return(plot)
    }
  }
