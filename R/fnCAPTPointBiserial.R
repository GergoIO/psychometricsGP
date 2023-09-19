#' Calculate PtBis for CAPT Items
#'
#' @param data (dataframe) Incoming long data eg a dataframe with cols StudentID, ItemID and IsCorrect
#' @param colStudentID (default = "StudentID"), String The name of the column containing the Student ID number
#' @param colItemID (default = "ItemID"), String. The name of the column containing the ItemID in each row
#' @param colIsCorrect (default = "IsCorrect"), String. The name of the column containing the item score in each row
#' @param significanceLevel (default = 0.05, 5%) Numeric. Set the significance level for each point biserial correlation
#'
#' @return returns a new dataframe storing the point biserial info for all input items
#' @export
#'
#' @examples df <- data.frame(
# StudentID = c(
#   "001",
#   "001",
#   "001",
#   "001",
#   "001",
#   "002",
#   "002",
#   "002",
#   "002",
#   "002",
#   "003",
#   "003",
#   "003",
#   "003",
#   "003",
#   "004",
#   "004",
#   "004",
#   "004",
#   "004"
# ),
# ItemID = c(
#   "B",
#   "C",
#   "D",
#   "E",
#   "F",
#   "B",
#   "C",
#   "D",
#   "E",
#   "F",
#   "A",
#   "B",
#   "D",
#   "E",
#   "F",
#   "B",
#   "C",
#   "D",
#   "E",
#   "F"
# ),
# IsCorrect = c(1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0)
# )
#
# df |> fnCAPTPointBiserial()
#'
################################################################################
#'
fnCAPTPointBiserial <-
  function(data,
           colStudentID = "StudentID",
           colIsCorrect = "IsCorrect",
           colItemID = "ItemID",
           significanceLevel = 0.05) {
    # Modify the incoming data
    # Leave cols: StudentID, ItemID, IsCorrect, TotalScore
    data <-
      data |>
      select(all_of(c(
        colStudentID,
        colItemID,
        colIsCorrect
      ))) |>
      # Use group_by_at so that the group_by can have a variable as the col name
      group_by_at(colStudentID) |>
      mutate(TotalScore = sum(across(all_of(colIsCorrect))))

    # Create an empty data frame to store the results
    dataPtBis <- data.frame(
      Test = character(),
      ItemID = character(),
      CorrelationCoefficient = numeric(),
      PValue = numeric(),
      stringsAsFactors = FALSE
    )

    # Initialise some vars to notify of progress
    nItemsProcessed <- 0
    pctItemsProgressedNext <- 20

    # Save the unique item numbers present in the input data
    # These are the items that the below loop will loop over and try to calculate the PtBis
    listItemNumbers <- unique(data[[colItemID]])

    # Loop through each item
    for (itemNumber in listItemNumbers) {
      # Filter data for the current item for this loop
      dataItem <-
        data |>
        filter(ItemID == itemNumber)

      # Calculate the point-biserial correlation coefficient and p-value for the current item with IsCorrect
      # PtBis cannot be calculate in some circumstances eg
      # if no-one got the item correct /
      # if everyone got the item correct /
      # if the student only got that item correct /
      # if there are two or fewer responses for that item
      if (sum(dataItem[[colIsCorrect]]) == 0 |
          sum(dataItem[[colIsCorrect]]) == nrow(dataItem) |
          sum(is.finite(dataItem$TotalScore)) < 2 |
          nrow(dataItem) == 2) {
        correlationResult <- (list(r = NA, p = NA))
      } else {
        result <- tryCatch(
          # PtBis actually calculated here:
          cor.test(dataItem[[colIsCorrect]], dataItem$TotalScore),
          error = function(e) {
            cat("Error occurred during cor.test for the following data:\n")
            print(dataItem)
            correlationResult <- (NULL)
          }
        )
        # If PtBis is not calculated, add values to the PtBis results instead
        if (is.null(result)) {
          correlationResult <- (list(r = NA, p = NA))
        } else {
          correlationResult <- (list(r = result$estimate, p = result$p.value))
        }
      }

      # Store the result in the result data frame
      # Add the results from this loop to the overall results
      dataPtBis <- rbind(
        dataPtBis,
        data.frame(
          Test = cnst$assessment,
          ItemID = itemNumber,
          CorrelationCoefficient = correlationResult$r,
          PValue = correlationResult$p,
          CorrelationCoefficientSignificant = ifelse(correlationResult$p < significanceLevel, correlationResult$r, NA)
        )
      )

      # Update progress and display message when reaching the next multiple of 20% progress
      nItemsProcessed <- nItemsProcessed + 1
      pctItemsProcessedCurr <-
        # Get fraction of the total items processed
        nItemsProcessed / length(unique(lResponses$main$ItemID)) * 100
      if (pctItemsProcessedCurr >= pctItemsProgressedNext) {
        print(paste(
          "PtBis Calculation Progress:",
          floor(pctItemsProcessedCurr),
          "% of items considered"
        ))
        pctItemsProgressedNext <- pctItemsProgressedNext + 20
      }
    }
    return(dataPtBis)
  }
