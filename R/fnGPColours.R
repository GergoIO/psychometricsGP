#' Function to provide a hex-code colour palette for a specified scheme or data vector
#'
#' @description fnGPColours takes either a specified scheme or a vector of data and returns (a) a character string of
#' hex-codes called Colour.Palette to the global environment, and (b) the same string as a return from the function.
#' This allows fnGPColours to be used in isolation within a script to create Colour.Palette for use elsewhere, or to be
#' used within plotting functions to provide a string of colours appropriate for the scheme/data being plotted. See
#' 'Examples'.
#' Use fnGPColours(Info="Y") to display details of the various colours and schemes handled by the function. Note that
#' although the abbreviations listed in recognised schemes are recommended if specifying a theme, fnGPColours can handle
#' (some) variations in these, e.g. 'ubse', 'U/B/S/E', and 'u-b-s-e' will all be recognised as 'UBSE'.
#'
#' @usage fnGPColours(DataOrScheme, DropValue, Info)
#'
#' @param DataOrScheme Either a specific scheme or a vector of data. Schemes should be entered as "UBSE", "PF", "UBLSHSE" etc. Data should be entered as a single vector. Schemes such as "Assessor" and "Maroon" can also be used where a single colour is required. See 'Notes' for recognised/determined schemes. Where subset-schemes are required, e.g. PF, the scheme determined will be PFE. DropValue (see below) can be used to correct this by excluding unwanted elements.
#' @param DropValue The DropValue argument allows specific elements of the scheme to be excluded. They shoul dbe entered as a single vector. For example, DropValue="S" will exclude S from any scheme including/requiring a colour for S, DropValue=c("B","S") will exclude B and S and their associated colours. See 'Notes' for limitations.
#' @param Info Optional argument. If Info="Y", fnGPColours returns a list object of package information listing hexcodes associated with each colour, recognised schemes and associated palettes, and a list of themes detectable from data inputs. Info="Y" over-rides all other function outputs.
#'
#' @examples
#' # Warnings for missing critical inputs
#' fnGPColours()
#'
#' # Details of recognised schemes, their palettes, and hex codes
#' fnGPColours(Info="Y")
#'
#' # Palettes for specific schemes
#'
#' fnGPColours(DataOrScheme = "UBSE")
#' fnGPColours(DataOrScheme = "UBLSHSE")
#' fnGPColours(DataOrScheme = "Assessor")
#'
#' # Palettes for data, including variation in input (within reason)
#'
#' fnGPColours(DataOrScheme = dataExample$Grade.UBSE)
#' fnGPColours(DataOrScheme = c("U","E","Excellent","HS","HS","LS","borderline","HS"))
#'
#' # Schemes and Data with excluded elements
#'
#' fnGPColours(DataOrScheme = "UBSE", DropValue = c("B","S"))
#' fnGPColours(DataOrScheme = dataExample$Grade.UBSE, DropValue = "B")
#' fnGPColours(DataOrScheme = c(1,2,3,5,1,2,2,5,3), DropValue = c(4))
#'
#' # Palette derived within plot functions (examples requires ggplot2)
#'
#' ggplot(data=dataExample, aes(x=Stage, y=Score.Perc, fill=Grade.UBSE)) +
#'   geom_bar(stat="identity", position = "dodge")+
#'   scale_fill_manual(values=fnGPColours("UBSE"))
#'
#' ggplot(data=dataExample, aes(x=Stage, y=Score.Perc, fill=Grade.PFE)) +
#'   geom_bar(stat="identity", position = "dodge")+
#'   scale_fill_manual(values=fnGPColours(dataExample$Grade.PFE))
#'
#' @author Dr Daniel Zahra, \email{daniel.zahra@plymouth.ac.uk}
#'
#' @export

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### fnGPColours (DZ) ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# DataOrScheme
# Either a scheme in the form of letters/name, (e.g. UBSE, PF, UBLSHSE, Assessors)
# OR a single vector of grades, years, etc

# DropValue
# Vector of grade letters to drop (e.g. "B", c("U","B")), defaults to none
# Can be used in conjunction with DataOrScheme to provide some flexibility
# To drop stages, specify either c("1","2"), or c("Stage1","Stage2")
# Cohorts cannot currently be dropped
# Values specified in DropValue which are not recognised will have no effect

# Notes
#
# Currently, the function adds 'Colour.Palette' to the global environment, and returns Colour.Palette
# So it can be used by itself to create an object for use later without the need to assign it,
# or used within a plot call to provide alist of colours.
#
# If a colour needs changing, just replace the Hex code for it in Table.Hexcodes
# Need to add wider range of DropValue options to Table.Drops, to accommodate variation;
# e.g. "U" and "Unsatisfactory"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Start function ####

fnGPColours <-
  function(DataOrScheme = "Missing",
           DropValue = "None",
           Info = "N") {
    # Remove any factorisation from inputs
    DataOrScheme <- as.character(DataOrScheme)
    DropValue <- as.character(DropValue)
    Info <- as.character(Info)

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #### Set options(stringsAsFactors = FALSE) ####
    # Removed by GP
    # FactorSettings <- options()$stringsAsFactors
    # if (FactorSettings == TRUE) {
    #   OriginalSettings <- "TRUE"
    #   options(stringsAsFactors = FALSE)
    # } else{
    #   OriginalSettings <- "FALSE"
    #   options(stringsAsFactors = FALSE)
    # } # shouldnt need this else part, but just to make sure.

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #### Create reference table/list for colours-in-schemes and hex codes ####

    # Data.frame of colour names (scheme elements) and associated hex codes

    Table.Hexcodes <- data.frame(
      Colour = c(
        "Maroon",
        "Assessor",
        "red",
        "orange",
        "green",
        "greenLS",
        "greenHS",
        "blue",
        "Stage1",
        "Stage2",
        "Stage3",
        "Stage4",
        "Stage5",
        "Cohort1",
        "Cohort2",
        "Cohort3",
        "Cohort4",
        "Cohort5",
        "Cohort6",
        "athenayellow",
        "athenagrey",
        "athenateal",
        "turdly"
      ),
      Hexcode = c(
        "#800000",
        "#88CCEE",
        "#D92120",
        "#E68B33",
        "#86BB6A",
        "#B1BE4E",
        "#6DB388",
        "#3D52A1",
        "#AA4455",
        "#AA7744",
        "#AAAA44",
        "#44AA77",
        "#4477AA",
        "#AA4455",
        "#AA7744",
        "#AAAA44",
        "#44AA77",
        "#44AAAA",
        "#4477AA",
        "#F9CA80",
        "#809BB2",
        "#B2E1E9",
        "##bea474"
      ),
      Description = c(
        "Maroon",
        "Light blue",
        "U-Red",
        "B-Orange",
        "S-Mid-Green",
        "LS-Orange-Green",
        "HS-Blue-Green",
        "E-Blue",
        "Series Red",
        "Series Orange",
        "Series Orange-Green",
        "Series Dark-Green",
        "Series Light-Blue",
        "Series Pinky-Red",
        "Series Muted-Orange",
        "Series-Yellowy-Green",
        "Series-Blue-Green",
        "Series-Greeny-Blue",
        "Series-Grey-Blue",
        "Athena SWAN Yellow",
        "Athena SWAN Grey",
        "Athena SWAN Teal",
        "AI Turdly"
      )
    )
    for (i in colnames(Table.Hexcodes)) {
      Table.Hexcodes[[i]] <-
        as.character(Table.Hexcodes[[i]])
    } # force as.character (some people might have global settings which treat new variables as factors by default, and factors mess up conditionals)

    # Data.frame of DropValue(s) and which colour names they exclude
    Table.Drops <- data.frame(
      DropValue = c(
        "U",
        "B",
        "S",
        "E",
        "P",
        "F",
        "1",
        "2",
        "3",
        "4",
        "5",
        "LS",
        "HS",
        "Stage1",
        "Stage2",
        "Stage3",
        "Stage4",
        "Stage5"
      ),
      Colour = c(
        "red",
        "orange",
        "green",
        "blue",
        "green",
        "red",
        "Stage1",
        "Stage2",
        "Stage3",
        "Stage4",
        "Stage5",
        "greenLS",
        "greenHS",
        "Stage1",
        "Stage2",
        "Stage3",
        "Stage4",
        "Stage5"
      )
    )
    for (i in colnames(Table.Drops)) {
      Table.Drops[[i]] <- as.character(Table.Drops[[i]])
    }

    # List of all schemes; each list is a vector of the colour names (scheme elements) needed
    # These will then be corrected according to the DropValue argument if used
    # Add a list to accommodate new schemes or variations in spelling/naming etc; e.g. Assessor/Assessors/Turquoise all call Assessor/#88CCEE

    List.Colours <- list(
      UBSE = c("red", "orange", "green", "blue"),
      UBLSHSE = c("red", "orange", "greenLS", "greenHS", "blue"),
      USE = c("red", "green", "blue"),
      UBS = c("red", "orange", "green"),
      PF = c("red", "green"),
      PFE = c("red", "green", "blue"),
      CIDK = c("green", "red", "orange"),
      CNINC = c("green", "orange", "red"),
      Assessor = c("Assessor"),
      Turquoise = c("Assessor"),
      Blue = c("Assessor"),
      Maroon = c("Maroon"),
      Single = c("Maroon"),
      Stage = c("Stage1", "Stage2", "Stage3", "Stage4", "Stage5"),
      Year = c("Stage1", "Stage2", "Stage3", "Stage4", "Stage5"),
      Cohort = c(
        "Cohort1",
        "Cohort2",
        "Cohort3",
        "Cohort4",
        "Cohort5",
        "Cohort6"
      ),
      Historic = c(
        "Cohort1",
        "Cohort2",
        "Cohort3",
        "Cohort4",
        "Cohort5",
        "Cohort6"
      ),
      Ethnicity = c("Cohort4", "Cohort5", "Cohort6"),
      Disability = c("Cohort4", "Cohort5", "Cohort6"),
      Gender = c("Stage4", "Stage5"),
      AthenaSWAN = c("athenayellow", "athenagrey", "athenateal"),
      AIGeneratedColours = c("turdly")
    )

    # Data.Scheme reference table for determining schemes; created here so that it's available if Info="Y" is called.
    # Includes possible combinations and variations including associated scheme.
    # Freq=0 column filled later during determination

    Data.Scheme <- data.frame(
      Value = c(
        "U",
        "B",
        "S",
        "E",
        "Unsatisfactory",
        "Borderline",
        "Satisfactory",
        "Excellent",
        "C",
        "C",
        "I",
        "DK",
        "Correct",
        "Incorrect",
        "DontKnow",
        "Dont Know",
        "Don't Know",
        "NI",
        "NC",
        "Competent",
        "Needs Improvement",
        "Not Competent",
        "NeedsImprovement",
        "NotCompetent",
        "u",
        "b",
        "s",
        "e",
        "unsatisfactory",
        "borderline",
        "satisfactory",
        "excellent",
        "dk",
        "correct",
        "incorrect",
        "dontknow",
        "dont know",
        "don't know",
        "ni",
        "nc",
        "competent",
        "needs improvement",
        "not competent",
        "needsimprovement",
        "notcompetent",
        "Pass",
        "Fail",
        "pass",
        "fail",
        "p",
        "f",
        "P",
        "F",
        "1",
        "0",
        "-0.25",
        "Male",
        "Female",
        "male",
        "female",
        "M",
        "F",
        "m",
        "f",
        "c",
        "c",
        "White",
        "white",
        "Asian",
        "asian",
        "Other",
        "other",
        "Black",
        "black",
        "Arab",
        "arab",
        "No known disability",
        "Specific learning difficulty",
        "Other disability",
        "No Known Disability",
        "Specific Learning Difficulty",
        "Other Disability",
        "no known disability",
        "specific learning difficulty",
        "other disability",
        "SLD",
        "sld",
        "Other",
        "other",
        "none",
        "None",
        "OTHER",
        "PASS",
        "FAIL",
        "E",
        "e",
        "Excellent",
        "excellent",
        "i",
        "U",
        "B",
        "LS",
        "HS",
        "E",
        "Unsatisfactory",
        "Borderline",
        "LowSatisfactory",
        "HighSatisfactory",
        "Excellent",
        "Low-Satisfactory",
        "High-Satisfactory",
        "low-satisfactory",
        "high-satisfactory",
        "unsatisfactory",
        "borderline",
        "excellent",
        "1",
        "2",
        "3",
        "4",
        "5",
        "Stage1",
        "Stage2",
        "Stage3",
        "Stage4",
        "Stage5",
        "stage1",
        "stage2",
        "stage3",
        "stage4",
        "stage5",
        "s1",
        "s2",
        "s3",
        "s4",
        "s5",
        "S1",
        "S2",
        "S3",
        "S4",
        "S5",
        "1011",
        "1112",
        "1213",
        "1314",
        "1415",
        "1516",
        "1718",
        "1819",
        "1920",
        "2021",
        "2122",
        "2223",
        "2324",
        "2425"
      ),
      Scheme = c(
        "UBSE",
        "UBSE",
        "UBSE",
        "UBSE",
        "UBSE",
        "UBSE",
        "UBSE",
        "UBSE",
        "CIDK",
        "CNINC",
        "CIDK",
        "CIDK",
        "CIDK",
        "CIDK",
        "CIDK",
        "CIDK",
        "CIDK",
        "CNINC",
        "CNINC",
        "CNINC",
        "CNINC",
        "CNINC",
        "CNINC",
        "CNINC",
        "UBSE",
        "UBSE",
        "UBSE",
        "UBSE",
        "UBSE",
        "UBSE",
        "UBSE",
        "UBSE",
        "CIDK",
        "CIDK",
        "CIDK",
        "CIDK",
        "CIDK",
        "CIDK",
        "CNINC",
        "CNINC",
        "CNINC",
        "CNINC",
        "CNINC",
        "CNINC",
        "CNINC",
        "PFE",
        "PFE",
        "PFE",
        "PFE",
        "PFE",
        "PFE",
        "PFE",
        "PFE",
        "CIDK",
        "CIDK",
        "CIDK",
        "Gender",
        "Gender",
        "Gender",
        "Gender",
        "Gender",
        "Gender",
        "Gender",
        "Gender",
        "CIDK",
        "CNINC",
        "Ethnicity",
        "Ethnicity",
        "Ethnicity",
        "Ethnicity",
        "Ethnicity",
        "Ethnicity",
        "Ethnicity",
        "Ethnicity",
        "Ethnicity",
        "Ethnicity",
        "Disability",
        "Disability",
        "Disability",
        "Disability",
        "Disability",
        "Disability",
        "Disability",
        "Disability",
        "Disability",
        "Disability",
        "Disability",
        "Disability",
        "Disability",
        "Disability",
        "Disability",
        "Disability",
        "PFE",
        "PFE",
        "PFE",
        "PFE",
        "PFE",
        "PFE",
        "CIDK",
        "UBLSHSE",
        "UBLSHSE",
        "UBLSHSE",
        "UBLSHSE",
        "UBLSHSE",
        "UBLSHSE",
        "UBLSHSE",
        "UBLSHSE",
        "UBLSHSE",
        "UBLSHSE",
        "UBLSHSE",
        "UBLSHSE",
        "UBLSHSE",
        "UBLSHSE",
        "UBLSHSE",
        "UBLSHSE",
        "UBLSHSE",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Stage",
        "Cohort",
        "Cohort",
        "Cohort",
        "Cohort",
        "Cohort",
        "Cohort",
        "Cohort",
        "Cohort",
        "Cohort",
        "Cohort",
        "Cohort",
        "Cohort",
        "Cohort",
        "Cohort"
      ),
      Freq = 0,
      Replacement = c(
        "Unsatisfactory",
        "Borderline",
        "Satisfactory",
        "Excellent",
        "Unsatisfactory",
        "Borderline",
        "Satisfactory",
        "Excellent",
        "Correct",
        "Competent",
        "Incorrect",
        "Don't Know",
        "Correct",
        "Incorrect",
        "Don't Know",
        "Don't Know",
        "Don't Know",
        "Needs Improvement",
        "Not Competent",
        "Competent",
        "Needs Improvement",
        "Not Competent",
        "Needs Improvement",
        "Not Competent",
        "Unsatisfactory",
        "Borderline",
        "Satisfactory",
        "Excellent",
        "Unsatisfactory",
        "Borderline",
        "Satisfactory",
        "Excellent",
        "Don't Know",
        "Correct",
        "Incorrect",
        "Don't Know",
        "Don't Know",
        "Don't Know",
        "Needs Improvement",
        "Not Competent",
        "Competent",
        "Needs Improvement",
        "Not Competent",
        "Needs Improvement",
        "Not Competent",
        "Pass",
        "Fail",
        "Pass",
        "Fail",
        "Pass",
        "Fail",
        "Pass",
        "Fail",
        "Correct",
        "Don't Know",
        "Incorrect",
        "Male",
        "Female",
        "Male",
        "Female",
        "Male",
        "Female",
        "Male",
        "Female",
        "Correct",
        "Competent",
        "White",
        "White",
        "Asian",
        "Asian",
        "Other",
        "Other",
        "Other",
        "Other",
        "Other",
        "Other",
        "No Known Disability",
        "Specific Learning Difficulty",
        "Other Disability",
        "No Known Disability",
        "Specific Learning Difficulty",
        "Other Disability",
        "No Known Disability",
        "Specific Learning Difficulty",
        "Other Disability",
        "Specific Learning Difficulty",
        "Specific Learning Difficulty",
        "Other Disability",
        "Other Disability",
        "No Known Disability",
        "No Known Disability",
        "Other Disability",
        "Pass",
        "Fail",
        "Excellent",
        "Excellent",
        "Excellent",
        "Excellent",
        "Incorrect",
        "Unsatisfactory",
        "Borderline",
        "Low-Satisfactory",
        "High-Satisfactory",
        "Excellent",
        "Unsatisfactory",
        "Borderline",
        "Low-Satisfactory",
        "High-Satisfactory",
        "Excellent",
        "Low-Satisfactory",
        "High-Satisfactory",
        "Low-Satisfactory",
        "High-Satisfactory",
        "Unsatisfactory",
        "Borderline",
        "Excellent",
        "Stage1",
        "Stage2",
        "Stage3",
        "Stage4",
        "Stage5",
        "Stage1",
        "Stage2",
        "Stage3",
        "Stage4",
        "Stage5",
        "Stage1",
        "Stage2",
        "Stage3",
        "Stage4",
        "Stage5",
        "Stage1",
        "Stage2",
        "Stage3",
        "Stage4",
        "Stage5",
        "Stage1",
        "Stage2",
        "Stage3",
        "Stage4",
        "Stage5",
        "1011",
        "1112",
        "1213",
        "1314",
        "1415",
        "1516",
        "1718",
        "1819",
        "1920",
        "2021",
        "2122",
        "2223",
        "2324",
        "2425"
      )
    )

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #### Info="Y" overides everything and returns list of information re colours, schemes etc

    if (Info == "Y" | Info == "Yes" | Info == "y" | Info == "yes") {
      for (i in names(List.Colours)) {
        if (i == names(List.Colours)[1]) {
          Colours <- character()
        }
        Colours <- c(Colours, toString(List.Colours[[i]]))
      }

      Package.Information <- list(
        HexColours = Table.Hexcodes,
        RecognisedSchemes = data.frame(Scheme = names(List.Colours), Colours =
                                         Colours),
        DetectedSchemes = toString(as.character(unique(
          Data.Scheme$Scheme
        )))
      )

      names(Package.Information) <-
        c(
          "Hexcodes associated with each Colour Name",
          "Recognised Schemes and associated palettes (variations accepted too; e.g. 'ubse' and 'U/B/S/E')",
          "Detectable Schemes (can be determined from data entered in DataOrScheme)"
        )

      return(Package.Information)

    } else{
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      #### Check Data, Scheme, and DropValue are in the correct format ####

      # Create reference table for inout checks
      Table.Check <-
        data.frame(
          Input = c("DataOrScheme", "DropValue"),
          FormatOK = c("-", "-"),
          LengthOK = c("-", "-")
        )
      for (i in colnames(Table.Check)) {
        Table.Check[[i]] <-
          as.character(Table.Check[[i]])
      } # force as.character (some people might have global settings which treat new variables as factors by default, and factors mess up the check conditionals)

      # Set default to inputs not being OK, update later if checks passed.
      InputsOK <- "No"

      # Check Data input (vector, length>=1)

      if (is.vector(DataOrScheme) == FALSE |
          is.list(DataOrScheme) == TRUE) {
        Table.Check$FormatOK[1] <- "No"
      } else{
        if (is.vector(DataOrScheme) == TRUE) {
          Table.Check$FormatOK[1] <- "Yes"
        }
      }
      if (Table.Check$FormatOK[1] == "No") {
        Table.Check$LengthOK[1] <- "NA"
      } else{
        if (length(DataOrScheme) >= 1) {
          Table.Check$LengthOK[1] <- "Yes"
        }
      }

      if (is.vector(DataOrScheme) == TRUE) {
        if (DataOrScheme[1] == "Missing") {
          Table.Check$FormatOK[1] <- "Missing"
        }
      }

      # Check DropValue input (vector, length>=1)

      if (is.vector(DropValue) == FALSE |
          is.list(DropValue) == TRUE) {
        Table.Check$FormatOK[2] <- "No"
      } else{
        if (is.vector(DropValue) == TRUE) {
          Table.Check$FormatOK[2] <- "Yes"
        }
      }
      if (Table.Check$FormatOK[2] == "No") {
        Table.Check$LengthOK[2] <- "NA"
      } else{
        if (length(DropValue) >= 1) {
          Table.Check$LengthOK[2] <- "Yes"
        }
      }

      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      #### Display warnings ####

      if (Table.Check$FormatOK[1] == "Missing") {
        warning("DataOrScheme is missing.")
      }

      if (length(Table.Check$FormatOK[Table.Check$FormatOK == "Yes"]) != 2 &
          Table.Check$FormatOK[1] != "Missing") {
        warning(paste("Check format - ", toString(Table.Check$Input[Table.Check$FormatOK !=
                                                                      "Yes"]), sep = ""))
      }

      if (length(Table.Check$LengthOK[Table.Check$LengthOK == "Yes"]) != 2) {
        warning(paste(
          "Check argument length - ",
          toString(Table.Check$Input[Table.Check$LengthOK != "Yes"]),
          sep = ""
        ))
      }

      if (length(Table.Check$LengthOK[Table.Check$LengthOK == "Yes"]) == 2 &
          length(Table.Check$FormatOK[Table.Check$FormatOK == "Yes"]) == 2) {
        InputsOK <- "Yes"
      }

      # Proceed if InputsOK=="Yes"

      if (InputsOK == "Yes") {
        #print("Yay!") # output marker for testing

        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        #### If DataOrScheme IS NOT a known scheme, assume it's Data and determine scheme ####

        # Derive 'Known Schemes' as any variation on recognised schemes from Table.Hex

        Table.VariationCorrections <- data.frame(
          Input = c(
            "UBSE",
            "ubse",
            "u-b-s-e",
            "u/b/s/e",
            "U/B/S/E",
            "U-B-S-E",
            "UBLSHSE",
            "ublshse",
            "USE",
            "use",
            "PF",
            "pf",
            "PFE",
            "pfe",
            "CIDK",
            "cidk",
            "CNINC",
            "cninc",
            "Assessor",
            "assessor",
            "Assessors",
            "assessors",
            "ASSESSORS",
            "Turquoise",
            "turquoise",
            "TURQUOISE",
            "Blue",
            "blue",
            "BLUE",
            "single",
            "Single",
            "SINGLE",
            "Stage",
            "stage",
            "STAGE",
            "Stages",
            "stages",
            "Year",
            "year",
            "YEAR",
            "years",
            "Years",
            "Cohort",
            "cohort",
            "COHORTS",
            "AcYears",
            "Maroon",
            "maroon",
            "MAROON",
            "Ethnicity",
            "ethnicity",
            "ETHNICITY",
            "disability",
            "Disability",
            "DISABILITY",
            "Gender",
            "gender",
            "GENDER",
            "pass-fail",
            "Pass-Fail",
            "Historic",
            "historic",
            "HISTORIC",
            "p-f",
            "P-F",
            "p/f",
            "P/F",
            "u-s-e",
            "u/s/e",
            "U-S-E",
            "U/S/E",
            "p-f-e",
            "p/f/e",
            "P-F-E",
            "p/f/e",
            "u-b-ls-hs-e",
            "u/b/ls/hs/e",
            "U/B/LS/HS/E",
            "U-B-LS-HS-E",
            "pass/fail",
            "Pass/Fail",
            "PASS/FAIL"
          ),
          Replacement = c(
            "UBSE",
            "UBSE",
            "UBSE",
            "UBSE",
            "UBSE",
            "UBSE",
            "UBLSHSE",
            "UBLSHSE",
            "USE",
            "USE",
            "PF",
            "PF",
            "PFE",
            "PFE",
            "CIDK",
            "CIDK",
            "CNINC",
            "CNINC",
            "Assessor",
            "Assessor",
            "Assessor",
            "Assessor",
            "Assessor",
            "Turquoise",
            "Turquoise",
            "Turquoise",
            "Blue",
            "Blue",
            "Blue",
            "Single",
            "Single",
            "Single",
            "Stage",
            "Stage",
            "Stage",
            "Stage",
            "Stage",
            "Year",
            "Year",
            "Year",
            "Year",
            "Year",
            "Cohort",
            "Cohort",
            "Cohort",
            "Cohort",
            "Maroon",
            "Maroon",
            "Maroon",
            "Ethnicity",
            "Ethnicity",
            "Ethnicity",
            "Disability",
            "Disability",
            "Disability",
            "Gender",
            "Gender",
            "Gender",
            "PF",
            "PF",
            "Historic",
            "Historic",
            "Historic",
            "PF",
            "PF",
            "PF",
            "PF",
            "USE",
            "USE",
            "USE",
            "USE",
            "PFE",
            "PFE",
            "PFE",
            "PFE",
            "UBLSHSE",
            "UBLSHSE",
            "UBLSHSE",
            "UBLSHSE",
            "PF",
            "PF",
            "PF"
          )
        )

        KnownSchemes <- names(List.Colours)
        Scheme <- "Unknown" # default scheme
        Scheme.Determined.By.Function <-
          "No" # default switch used to determine correct messages/warnings

        # If DataOrScheme is a single-item vector, check if it's a known scheme or variation
        # If it is, correct it to the correct format of recognised schemes (if needed), and set Scheme==DataOrScheme
        # If it's not a known variation or scheme, Scheme will remain set to 'Unknown'

        if (is.vector(DataOrScheme) == TRUE &
            length(DataOrScheme) == 1) {
          if (length(Table.VariationCorrections$Input[Table.VariationCorrections$Input ==
                                                      DataOrScheme]) == 1) {
            DataOrScheme <-
              Table.VariationCorrections$Replacement[Table.VariationCorrections$Input ==
                                                       DataOrScheme]
          }
          if (length(KnownSchemes[KnownSchemes == DataOrScheme]) == 1) {
            Scheme <- DataOrScheme
          }
        }

        # If at this stage Scheme is 'Unknonw', then DataOrScheme wasn't a known scheme or variation, or was longer than length==1
        # The function then checks to see if DataOrScheme is a vactor of data that fits a recognised scheme, and sets the Scheme accordingly

        if (Scheme == "Unknown") {
          Data.Scheme$Replacement <-
            as.character(Data.Scheme$Replacement) # to make sure Data.Scheme$Replacement is not factorised (if factorised, replacement of values is thrown by level numbers being used in some versions/settings of R)
          Data.Scheme$Value <-
            as.character(Data.Scheme$Value) # ditto

          # Calculate frequencies of each element in DataOrScheme

          DataOrScheme <-
            as.character(DataOrScheme) # remove any factorisation
          for (i in Data.Scheme$Value) {
            Data.Scheme$Freq[Data.Scheme$Value == i] <-
              length(DataOrScheme[DataOrScheme == i])
          }

          # Look for any unknown values in the input Data (currently stored in Data.Plot$variable), and abort with warning if Unknown.Values>=1

          Data.ValuesCheck <-
            data.frame(DataOrScheme = unique(DataOrScheme),
                       Freq = 0)
          Data.ValuesCheck$DataOrScheme <-
            as.character(Data.ValuesCheck$DataOrScheme) # to clear any automatic factoring in some R builds/settings.

          for (i in unique(Data.ValuesCheck$DataOrScheme)) {
            Data.ValuesCheck$Freq[Data.ValuesCheck$DataOrScheme == i] <-
              length(Data.Scheme$Value[Data.Scheme$Value == i])
          }

          Unknown.Values <-
            Data.ValuesCheck$DataOrScheme[Data.ValuesCheck$Freq == 0]

          if (length(Unknown.Values) >= 1) {
            warning(
              "You entered data into DataOrScheme, but we could not determine which scheme you required. Sorry. Please try entering the scheme in DataOrScheme rather than the data itself."
            )
            Scheme <- "Unknown"
            InputsOK <- "No"
          }

          # Determine scheme if no unknown values

          if (length(Unknown.Values) == 0) {
            for (i in unique(Data.Scheme$Scheme)) {
              if (i == unique(Data.Scheme$Scheme)[1]) {
                Data.Scheme.Determination <-
                  data.frame(Scheme = character(), Freq = numeric())
              }
              Temp <-
                data.frame(Scheme = i,
                           Freq = sum(Data.Scheme$Freq[Data.Scheme$Scheme == i]))
              Data.Scheme.Determination <-
                rbind(Data.Scheme.Determination, Temp)
            }
            Data.Scheme.Determination$Perc <-
              100 * (Data.Scheme.Determination$Freq / length(DataOrScheme))
            Estimated.Scheme <-
              as.character(Data.Scheme.Determination$Scheme[Data.Scheme.Determination$Freq ==
                                                              max(Data.Scheme.Determination$Freq) &
                                                              Data.Scheme.Determination$Perc >= 95.00])
            if (length(Estimated.Scheme) == 0) {
              Estimated.Scheme <-
                "Unknown"
            } # in case no scheme meets the max freq and 95% criteria
            Scheme.Determined.By.Function <- "Yes"
            Scheme <- Estimated.Scheme
            if (length(Scheme) == 1 & Scheme != "Unknown") {
              # Commented out to suppress dialogue
              #print(paste("You have entered data into the DataOrScheme argument. We have used this to determine the scheme you require to be ", Scheme,". If this is incorrect, please enter the scheme you require instead of the data. If you require elements to be excluded, please add 'DropValue=c()' to the function call to specify elements to exclude.",sep=""))

            } else{
              warning(
                "You entered data into DataOrScheme, but we could not determine which scheme you required. Sorry. Please check the DataOrScheme entry, or try entering the scheme in DataOrScheme rather than the data itself."
              )
            }
          }

        } # Close Scheme=="Unknown" scheme determination loop
      } # Close InoutsOK=="Yes" conditional

      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      #### If DataOrScheme is a known scheme and/or a scheme could be determined, continue to colours ####

      if (InputsOK == "Yes") {
        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        #### If DataOrScheme IS a known scheme, use this to determine colours ####

        # Commented out to suppress dialogue
        #if(Scheme!="Unknown" & Scheme.Determined.By.Function!="Yes"){print(paste("You have entered a known scheme (",as.character(Scheme),") in the DataOrScheme argument. This will be used to determine the colours you require.",sep=""))}

        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        #### List hexcodes of colours required for Scheme, after adjusting for DropValue ####

        if (Scheme != "Unknown") {
          # Find colour names of values specified in DropValue

          for (i in DropValue) {
            if (i == DropValue[1]) {
              Drop.Colours <- as.character()
            }
            Drop.Colours <-
              c(Drop.Colours, Table.Drops$Colour[Table.Drops$DropValue == i])
          }

          # Determine colours required by Scheme, exclude any DropValue colours

          Colours.Required <- List.Colours[[Scheme]]
          for (i in Drop.Colours) {
            Colours.Required <- subset(Colours.Required, Colours.Required != i)
          }

          # Determine Hexcodes for the required colours

          for (i in Colours.Required) {
            if (i == Colours.Required[1]) {
              Colour.Palette <- as.character()
            }
            Colour.Palette <-
              c(Colour.Palette, Table.Hexcodes$Hexcode[Table.Hexcodes$Colour == i])
          }

          #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          ####  Re-Set options(stringsAsFactors = FALSE) ####
          # Removed by GP
          # if (OriginalSettings == "TRUE") {
          #   options(stringsAsFactors = TRUE)
          # }

          #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          #### Output from function ####

          assign("Colour.Palette",
                 value = Colour.Palette,
                 envir = globalenv())
          return(Colour.Palette)
        } # Close hex-code generation
      } # Close InputsOK=="Yes" conditional
    } # Close Info="Y" else argument
  } # Close function

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

# fnGPColours(Info = "Y")
