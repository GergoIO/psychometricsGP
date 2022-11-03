#' Tag Analysis Table and Plot
#'
#' @param data ***
#' @param scoreOptions ***
#' @param variableName ***
#' @param variableOptions ***
#' @param variableNQuestions The number of times a question on each variable occurs. The first col contains the variables (should be titled "Var1") and the second col contains the number of times each occurs (should be titled "Number\nof\nQuestions")
#' @param textLabels Boolean (TRUE/FALSE). Set to TRUE to add the specific tag names to the x-axis on the plot. Otherwise each tag will be given a number (with a corresponding number also in the generated table)
#'
#' @return ***
#' @export
#'
#' @examples ***
#'
################################################################################
#'
fnTagAnalysis <- function(data = NULL,
                          scoreOptions = NULL,
                          variableName = NULL,
                          variableOptions = NULL,
                          variableNQuestions = NULL,
                          textLabels = NULL) {
  if (is.null(data) == TRUE |
      is.null(scoreOptions) == TRUE |
      is.null(variableName) == TRUE |
      is.null(variableOptions) == TRUE |
      is.null(variableNQuestions) == TRUE |
      is.null(textLabels) == TRUE) {
    stop("One of the required variables for this function has not been specified.")
  } else{
    lReturn <- list() # For returning multiple dfs and other objects
    #   __________________________________________________________________________
    #   TABLE                                                                 ####

    ##  ..........................................................................
    ##  Prep                                                                  ####

    # Prepping Speciality Specific Response PCTs
    for (i in 1:length(variableOptions)) {
      .responses <-
        data.frame(100 * prop.table(table(unlist(data[data[[variableName]] == variableOptions[i], -1]))))
      if (i == 1) {
        .tabIA = suppressWarnings(merge(
          scoreOptions,
          .responses,
          by = "Var1",
          all.x = TRUE
        ))
      } else {
        .tabIA = suppressWarnings(merge(.tabIA,
                                        .responses,
                                        by = "Var1",
                                        all.x = TRUE))
      }
    }

    .tabIA[is.na(.tabIA)] <- 0
    .tabIA <- data.frame(t(.tabIA))
    colnames(.tabIA) <- .tabIA[1, ]
    .tabIA <- .tabIA[-1, ]
    rownames(.tabIA) <- variableOptions

    # For use with later plotting of item specific responses
    .tabIALong <- .tabIA

    colnames(.tabIA) <-
      c("Incorrect\n(%)", "Don't\nKnow\n(%)", "Correct\n(%)")
    .tabIA <- fnRound(.tabIA, 2)
    # Move specialities to their own col
    .tabIA <- rownames_to_column(.tabIA, "Var1")

    .tabIA <- merge(variableNQuestions,
                    .tabIA,
                    by = "Var1")
    colnames(.tabIA)[1] <- "Speciality"

    # Order by most incorrect answers at top
    .tabIA <- .tabIA[order(-.tabIA[, 3]), ]

    # Add a col for speciality number
    .tabIA <-
      cbind("Speciality\nNumber" = c(1:length(variableOptions)), .tabIA)

    # TABLE
    .tabIA <- qflextable(.tabIA)
    # Change background of correct response to green
    .tabIA <-
      bg(.tabIA, j = 4, bg = "#ff4d4d") # Red
    .tabIA <-
      bg(.tabIA, j = 5, bg = "#b5bfc9") # Gray
    .tabIA <-
      bg(.tabIA, j = 6, bg = "#86BB6A") # Green

    # Save table to be returned
    lReturn[[glue("tab{variableName}")]] <- .tabIA


    #   ____________________________________________________________________________
    #   PLOTS                                                                   ####

    # Reformat stage specific responses using melt for bar plotting
    .tabIALong <-
      rownames_to_column(.tabIALong, "VarName")
    # Order by most incorrect answers at top
    .tabIALong <- .tabIALong[order(-.tabIALong[, 2]), ]

    # Suppression of messages stops a "Using Stage as id variables" text being displayed each loop
    .tabIALong <- suppressMessages(melt(.tabIALong))
    colnames(.tabIALong) <-
      c("VarName", "Response", "Proportion")

    # Assigns a number to each variable
    .tabIALong$variable <- c(1:length(variableOptions))

    .pltIA <- ggplot(data = .tabIALong,
                     aes(
                       x = variable,
                       y = Proportion,
                       fill = factor(
                         Response,
                         levels = c(-0.25, 0, 1),
                         labels = c("Incorrect", "Correct", "Don't Know")
                       )
                     )) +
      labs(
        x = glue("{variableName} Number"),
        y = "Proportion (%)",
        fill = "Response"
      ) +
      scale_fill_manual(values = c("#ff4d4d", "#b5bfc9", "#86BB6A")) +
      geom_bar(width = 0.85,
               stat = "identity",
               position = "stack") +
      scale_y_continuous(limits = c(0, 100.1), expand = c(0, 0)) +
      theme_psmd() +
    if (textLabels == TRUE) {
        scale_x_continuous(
          breaks = c(1:length(variableOptions)),
          expand = c(0, 0),
          labels = variableOptions
        )
      } else{
        scale_x_continuous(breaks = c(1:length(variableOptions)),
                           expand = c(0, 0))
      }

    # Save table to be returned
    lReturn[[glue("plt{variableName}")]] <- .pltIA

    return(lReturn)
  }
}

# The Data variable -
# Data must be one row for each item.
# First col contains the variable type for each item (ie the specific speciality of whatever is being compared)
# Other cols contain student scores (-0.25, 0, 1)
# Each student has a column
