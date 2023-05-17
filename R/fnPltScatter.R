#' Scatterplots for Test-Retest and Facility-by-ResponseRate.
#'
#' @description fnScatterplot creates scatterplots for test-retest performance (with or without grouping by Stage), and scatterplots of Facility by Response Rate.
#'
#' The output from fnScatterplot is a ggplot object which can then be saved or customised as needed.
#'
#' @usage fnScatterplot(Data=NULL, x=NULL, y=NULL, Grouping=NULL, Type=NULL, Items=NULL)
#'
#' @param Data Data should be a dataframe including variables for at least x and y. It should also include Grouping and Items if used as fnScatterplot checks Data for the specified variables. Data should be entered as a named object, i.e. Data.Scores rather than as a string, "Data.Scores".
#'
#' @param x Ideally a numeric variable of values to be plotted on the x-axis. Should be entered as a string, i.e. x="variable.name" rather than x=variable.name. fnScatterplot will attempt to coerce non-numeric and factorised variables into suitable formats. Proportions will be converted to percentage.
#'
#' @param y Ideally a numeric variable of values to be plotted on the x-axis. Should be entered as a string, i.e. "y=[variable.name]" rather than y=variable.name. fnScatterplot will attempt to coerce non-numeric and factorised variables into suitable formats. Proportions will be converted to percentage.
#'
#' @param Grouping The variable by which TRT points should be groups, usully likely to be Stage. Should be entered as a string, i.e. Grouping="Stage" rather than Grouping=variable.name. fnScatterplot will attempt to coerce factorised variables into suitable formats. Can be NULL.
#'
#' @param Type The type of plot required. Specify "TRT" for test-retest (with or without Grouping), or "IA" for facility by response rate. If using IA, the Items argument must be specified.
#'
#' @param Items The variable which specifies item numbers to be used to label points in a Facility-by-ResponseRate plot.
#'
#'  @note
#'  DZ180118: Error messages are sequential; i.e. if Data is missing, no other argumenrts will be checked. Also, all variables should be in Data; possibly need to add some flexibility here to pull arguments from other dataframes.
#'
#' Tweaks by Gergo Pinter 10/08/22 (see #GP for notes on tweaks)
#'
#' @examples
#'
#' Examples of warning combinations and working plots (using Data.1 below; need to update to use dataExample):
#'
#' Data.1<-data.frame(Item=c(1:20),Stage=factor(c(1,1,1,1,1,2,2,2,2,2,3,3,4,4,4,3,2,4,5,5)), Test1=c(25,10,12,13,25,54,54,44,36,29,65,55,59,57,58,70,75,82,95,81), Test2=c(27,17,20,18,32,60,50,39,30,35,68,62,50,70,65,79,78,90,91,90),ResponseRate=c(25,32,36,84,85,74,65,55,41,52,24,26,96,84,75,77,65,68,63,69),Facility=c(0.10,0.25,0.40,0.90,0.82,0.65,0.84,0.24,0.85,0.74,0.07,0.09,0.98,0.89,0.76,0.74,0.68,0.71,0.80,0.73))
#'
#' fnScatterplot(Data=NULL, x=NULL, y=NULL, Grouping=NULL, Type=NULL, Items=NULL)
#' fnScatterplot(Data=Data.1, x=NULL, y=NULL, Grouping=NULL, Type=NULL, Items=NULL)
#' fnScatterplot(Data=Data.1, x="Test1", y=NULL, Grouping=NULL, Type=NULL, Items=NULL)
#' fnScatterplot(Data=Data.1, x="Test1", y="Test2", Grouping=NULL, Type="TRT", Items=NULL)
#' fnScatterplot(Data=Data.1, x="Test1", y="Test2", Grouping=NULL, Type="IA", Items=NULL)
#' fnScatterplot(Data=Data.1, x="Test1", y="Test2", Grouping=NULL, Type="Expelliarmus", Items=NULL)
#' fnScatterplot(Data=Data.1, x="Test1", y="Mysterious", Grouping=NULL, Type="IA", Items=NULL)
#' fnScatterplot(Data=Data.1, x="Test1", y="Mysterious", Grouping=NULL, Type="IA", Items="Item")
#' fnScatterplot(Data=Data.1, x="Test1", y="Mysterious", Grouping=NULL, Type="TRT", Items=NULL)
#' fnScatterplot(Data=Data.1, x="Mysterious", y="Test2", Grouping=NULL, Type="TRT", Items=NULL)
#' fnScatterplot(Data=Data.1, x="Test1", y="Test2", Grouping="Stage", Type="TRT", Items=NULL)
#' fnScatterplot(Data=Data.1, x="Test1", y="Test2", Grouping=NULL, Type="IA", Items="Mysterious")
#' fnScatterplot(Data=Data.1, x="Test1", y="Test2", Grouping=NULL, Type="IA", Items="Item")
#'
#' @author Dr Daniel Zahra, \email{daniel.zahra@plymouth.ac.uk}
#'
#' @export

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### fnScatterplot - last updated 1800118 ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# fnScatterplot, for Test-Retest plots, for single and multiple years, as Facility-by-Response plots.

# Required packages

# library("psychometricsPSMD")
# library("ggplot2")

# For testing (after compiling the function)

#Data.1<-data.frame(Item=c(1:20),Stage=factor(c(1,1,1,1,1,2,2,2,2,2,3,3,4,4,4,3,2,4,5,5)), Test1=c(25,10,12,13,25,54,54,44,36,29,65,55,59,57,58,70,75,82,95,81), Test2=c(27,17,20,18,32,60,50,39,30,35,68,62,50,70,65,79,78,90,91,90),ResponseRate=c(25,32,36,84,85,74,65,55,41,52,24,26,96,84,75,77,65,68,63,69),Facility=c(0.10,0.25,0.40,0.90,0.82,0.65,0.84,0.24,0.85,0.74,0.07,0.09,0.98,0.89,0.76,0.74,0.68,0.71,0.80,0.73))

#fnScatterplot(Data=NULL, x=NULL, y=NULL, Grouping=NULL, Type=NULL, Items=NULL)
#fnScatterplot(Data=Data.1, x=NULL, y=NULL, Grouping=NULL, Type=NULL, Items=NULL)
#fnScatterplot(Data=Data.1, x="Test1", y=NULL, Grouping=NULL, Type=NULL, Items=NULL)
#fnScatterplot(Data=Data.1, x="Test1", y="Test2", Grouping=NULL, Type="TRT", Items=NULL)
#fnScatterplot(Data=Data.1, x="Test1", y="Test2", Grouping=NULL, Type="IA", Items=NULL)
#fnScatterplot(Data=Data.1, x="Test1", y="Test2", Grouping=NULL, Type="Expelliarmus", Items=NULL)
#fnScatterplot(Data=Data.1, x="Test1", y="Mysterious", Grouping=NULL, Type="IA", Items=NULL)
#fnScatterplot(Data=Data.1, x="Test1", y="Mysterious", Grouping=NULL, Type="IA", Items="Item")
#fnScatterplot(Data=Data.1, x="Test1", y="Mysterious", Grouping=NULL, Type="TRT", Items=NULL)
#fnScatterplot(Data=Data.1, x="Mysterious", y="Test2", Grouping=NULL, Type="TRT", Items=NULL)
#fnScatterplot(Data=Data.1, x="Test1", y="Test2", Grouping="Stage", Type="TRT", Items=NULL)
#fnScatterplot(Data=Data.1, x="Test1", y="Test2", Grouping=NULL, Type="IA", Items="Mysterious")
#fnScatterplot(Data=Data.1, x="Test1", y="Test2", Grouping=NULL, Type="IA", Items="Item")

# Notes
# Other than Data, arguments should be entered as character strings, e.g. "Scores" to denote the column on the dataframe called 'Scores'.

#### Start Function####

fnPltScatter <-
  function(Data = NULL,
           x = NULL,
           y = NULL,
           Grouping = NULL,
           Type = NULL,
           Items = NULL) {
    #### Check Inputs ####

    # Default to Continue=="Yes"

    Continue <- "Yes"

    # Check Data, x, and y are specified, switch Continue to "No" if not, and display warning

    if (is.null(Data)) {
      Continue <- "No"
      warning("Please specify the dataframe to use.")
    } else{
      if (is.null(x) | is.null(y)) {
        Continue <- "No"
        warning("Please specify both x and y variables, using \"[variable name]\".")
      }
    }

    # Check an acceptable Type has been specified, switch to Continue=="No" and display warning if not.

    if (Continue == "Yes") {
      if (is.null(Type)) {
        Continue <- "No"
        warning(
          "Plese specify a Type (\"TRT\" for Test-Retest or \"IA\" for Item Anlysis/Facility by Response Rate)"
        )
      } else{
        if (Type == "TRT" | Type == "IA") {
          Continue <- "Yes"
        } else{
          Continue <- "No"
          warning(
            "Please specify a valid Type (\"TRT\" for Test-Retest or \"IA\" for Item Anlysis/Facility by Response Rate)"
          )
        }
      }
    }

    # Check Item variable has been specified if Type=="IA"

    if (Continue == "Yes") {
      if (Type == "IA" & is.null(Items)) {
        Continue <- "No"
        warning(
          "Please specify a variable of Item numbers for Items argument for use with IA plots, using the format \"[variable name]\""
        )
      }
    }

    #### Format Inputs ####

    if (Continue == "Yes") {
      # Check x and y appear in Data

      if (Continue == "Yes") {
        if (is.null(Data[x])) {
          warning("x specifies a column not in Data")
          Continue <- "No"
        }
        if (is.null(Data[y])) {
          warning("y specifies a column not in Data")
          Continue <- "No"
        }
      }

      # Check Items appears in Data

      if (Continue == "Yes") {
        if (!is.null(Items)) {
          if (is.null(Data[[Items]])) {
            warning("Items specifies a column not in Data")
            Continue <- "No"
          }
        }
      }

      # Format x, y, and Grouping if needed

      if (!is.null(Grouping)) {
        Grouping.Name <- as.character(Grouping)
      }

      if (Continue == "Yes") {
        x <- as.numeric(as.character(Data[[x]]))
        y <- as.numeric(as.character(Data[[y]]))
        if (!is.null(Grouping)) {
          Grouping <- Data[[Grouping]]
          if (!is.factor(Grouping)) {
            Grouping <- factor(as.numeric(as.character(Grouping)))
          }
        }
      }

    } # Close Continue Conditional

    ####  Determine plot type; TRT, IA, or Unknown  ####
    # TO BE EXPANDED

    if (Continue == "Yes") {
      if (is.null(Type)) {
        warning("Please specify a valid Type (\"TRT\" or \"IA\")")
        Type.Given <- "No"
        Type <- "Unknown"
      } else{
        if (Type != "TRT" & Type != "IA") {
          Type.Given <- "No"
          Type <- "Unknown"
          warning("Please specify a valid Type (\"TRT\" or \"IA\")")
        } else{
          Type.Given <- "Yes"
        }
      }
    }

    #### Basic blank Plot ####

    if (Continue == "Yes") {
      if (Type.Given == "Yes") {
        Plot <- ggplot(Data, aes(x = x, y = y)) + themeGP()
      }

      #### Conditional additions to Plot ####

      #### TRT with no grouping ####

      if (Type == "TRT" & is.null(Grouping)) {
        Plot <- Plot +
          geom_point(size = 3) +
          xlab("Test1") +
          ylab("Test2") +
          xlim(0, 100) +
          ylim(0, 100) +
          theme(legend.position = "none")
      }

      #### TRT with grouping ####

      if (Type == "TRT" & is.null(Grouping) != TRUE) {
        StageColours <- fnGPColours("Stage")
        Plot <- Plot +
          geom_point(aes(
            colour = Grouping,
            shape = Grouping,
            fill = Grouping
          ),
          size = 3) +
          xlab("Test1") +
          ylab("Test2") +
          xlim(0, 100) +
          ylim(0, 100) +
          scale_colour_manual(values = StageColours) +
          scale_fill_manual(values = StageColours) +
          scale_shape_manual(values = c(15, 16, 17, 23, 25)) +
          guides(color = guide_legend(title = Grouping.Name)) +
          guides(shape = guide_legend(title = Grouping.Name)) +
          guides(fill = guide_legend(title = Grouping.Name))
      }

      #### ItemAnalysis ####

      if (Type == "IA") {
        if (max(x) <= 1) {
          x <- (100 * x)
        }
        if (max(y) <= 1) {
          y <- (100 * y)
        }

        if (is.null(Items)) {
          Plot <- Plot +
            geom_point(size = 3) +
            xlab("Facility (%)") +
            ylab("Response Rate (%)") +
            xlim(-5, 105) + # so plot format matches jittered version below
            ylim(-5, 105) + # so plot format matches jittered version below
            geom_smooth(formula = y ~ x,
                        method = lm,
                        se = FALSE)
        } # GP - added formula = y ~ x,

        if (is.null(Items) != TRUE) {
          Plot <- Plot +
            xlab("Facility") +
            ylab("Response Rate (%)") +
            xlim(-5, 105) + # so all ploints plot after any combination of jitter
            ylim(-5, 105) + # so all ploints plot after any combination of jitter
            geom_smooth(formula = y ~ x,
                        method = lm,
                        se = FALSE) + # GP - added formula = y ~ x
            geom_text(
              aes(label = Data[[Items]]),
              position = position_jitter(width = 3.5, height = 3.5),
              size = 4
            )
        }

      } # Close ItemAnalysis conditional

      #### Returns ####

      if (Type.Given == "Yes") {
        return(Plot)
      }
    } # close continue conditional
  } # close function

#### End Function ####

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##
##
##
##
##
##
####
###
##
#
