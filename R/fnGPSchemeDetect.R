#' Function to detect and determine grading schemes .
#'
#' @description Function to detect and determine grading schemes. fnGPSchemeDetect returns the scheme that has been determined from the data (SchemeDetect$Scheme), as well as a standardised data set (SchemeDetect$Data).
#'
#' Can currently be used with: UBSE, USE, CIDK, CNINC, CNINC, CNINC, PF, PFE, UBLHE, Gender, Disability, Letters, Ethnicity
#'
#' @note
#' (DZ070917) Can't yet handle if two schemes account for the same amount of values; all possibles listed and warning displayed. First possible used to standardise the data.
#' (DZ070917) Doesn't check for values not in Table.Schemes; so rogue values in Data cause an error.
#' (DZ070917) Doesn't check scheme specified in Force.Scheme exists in Table.Schemes. Need check/warning for this.
#'
#' @usage fnGPSchemeDetect(Data, Force.Scheme)
#'
#' @param Data Data vector to from which to determine a Scheme.
#' @param Force.Scheme Optional. Used to specify or force a Scheme.
#'
#' @examples
#'
#' fnGPSchemeDetect()
#' fnGPSchemeDetect(Data=dataExample$Grade.UBSE)
#' fnGPSchemeDetect(Data=dataExample$Grade.UBSE, Force.Scheme = "UBSE")
#' fnGPSchemeDetect(Force.Scheme = "UBSE")
#' fnGPSchemeDetect(Data=dataExample)
#' fnGPSchemeDetect(Data=dataExample, Force.Scheme = "UBSE")
#' fnGPSchemeDetect(Data=c("U","S","E","U","S","E"))
#' fnGPSchemeDetect(Data=c(1,2,3,4,5))
#' fnGPSchemeDetect(Data=c(10.09,22.53,39.66,43.21,75.55))
#' fnGPSchemeDetect(Data=c(109,112,334,421,7655))
#' fnGPSchemeDetect(Data=c("male","female","m","f","f","MALE"))
#' fnGPSchemeDetect(Data=dataExample$Grade.UBSE, Force.Scheme = "Hermione") # Need a check for this
#' fnGPSchemeDetect(Data=c("U","S","B","U","S","W")) # Need to account for these
#'
#' @author Dr Daniel Zahra, \email{daniel.zahra@plymouth.ac.uk}
#'
#' @export

# Function to detect and determine grading schemes (DZ 070917)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### fnGPSchemeDetect ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# NOTES:

# fnGPSchemeDetect returns the scheme that has been determined from the data (SchemeDetect$Scheme), as well as a standardised data set (SchemeDetect$Data)

# Can't yet handle if two schemes account for the same amount of values; all possibles listed and warning displayed. First possible used to standardis the data.

# Doesn't check for values not in Table.Schemes; so rogue values in Data cause an error.

# Doesn't check scheme specified in Force.Scheme exists in Table.Schemes. Need check/warning for this.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Inputs ####

fnGPSchemeDetect<-function(Data=NULL, Force.Scheme=NULL){

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #### Check Data and Force.Scheme ####

  # Start with 'Continue' as 'Yes'
  Continue<-"Yes"

  # Check data isn't dataframe/list and scheme isn't NULL
  if(is.data.frame(Data)==TRUE | is.list(Data)==TRUE){DataCheck<-"ERROR"}else{DataCheck<-"OK"}
  if(is.null(Data)==TRUE){DataCheck<-"Missing"}
  if(is.null(Force.Scheme)==FALSE){ForceSchemeCheck<-"OK"}else{ForceSchemeCheck<-"Missing"}

  # Compile checks table
  Table.Checks<-data.frame(Data=DataCheck, Force.Scheme=ForceSchemeCheck)

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #### Adjust Continue status by Table.Checks permutations ####

  # NOTE: This section could be condensed

  # ERROR Data, OK Scheme

  if(Table.Checks$Data=="ERROR" & Table.Checks$Force.Scheme=="OK"){
    DeterminedScheme<-Force.Scheme
    StandardisedData<-"Incorrect data format"
    Continue<-"No"}

  # ERROR Data, Missing Scheme

  if(Table.Checks$Data=="ERROR" & Table.Checks$Force.Scheme=="Missing"){
    DeterminedScheme<-"No scheme specified"
    StandardisedData<-"Incorrect data format"
    Continue<-"No"}

  # Missing Data, OK Scheme

  if(Table.Checks$Data=="Missing" & Table.Checks$Force.Scheme=="OK"){
    DeterminedScheme<-Force.Scheme
    StandardisedData<-"No data input"
    Continue<-"No"}

  # Missing Data, Missing Scheme

  if(Table.Checks$Data=="Missing" & Table.Checks$Force.Scheme=="Missing"){
    DeterminedScheme<-"No scheme specified"
    StandardisedData<-"No data input"
    Continue<-"No"}

  # OK Data, OK Scheme

  if(Table.Checks$Data=="OK" & Table.Checks$Force.Scheme=="OK"){
    DeterminedScheme<-Force.Scheme
    StandardisedData<-Data
    Continue<-"No"
    Replace.Data<-"Yes"}

  # OK Data, Missing Scheme

  if(Table.Checks$Data=="OK" & Table.Checks$Force.Scheme=="Missing"){
    DeterminedScheme<-"WIP"
    StandardisedData<-"WIP"
    Continue<-"Yes"}

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #### Determine scheme ####

  # Check if Data is numeric, if yes, determine if Data is Score, Stages, or AssessorIDs
  # NOTE: This section needs work, and factored number lists don't count as numeric

  if(Continue=="Yes"){
    if(is.numeric(Data)==TRUE){
      if(max(Data)<=125 & max(Data)-min(Data)>4){DeterminedScheme<-"Scores"}
      if(max(Data)<=125 & max(Data)-min(Data)<=4 & length(unique(Data))<=5){DeterminedScheme<-"Stages"}
      if(max(Data)>125){DeterminedScheme<-"AssessorID"}
      StandardisedData<-Data
      Continue<-"No"}}

  # Create reference table of possible Data values, schemes, and replacements
  # Outside of conditional so can be used by Replace.Data conditional later

  Table.Schemes<-data.frame(
    Value=c("u","b","s","e","unsatisfactory","borderline","satisfactory","excellent","u","s","e","unsatisfactory","satisfactory","excellent","c","i","dk","correct","incorrect","dont know", "don't know", "dontknow", "don'tknow","c","competent","ni","needs improvement", "needsimprovement","nc","not competent", "notcompetent","p","pass","f","fail","p","pass","f","fail","e","excellent","u","unsatisfactory","b","borderline","ls","low-satisfactory", "low satisfactory", "lowsatisfactory","hs","high-satisfactory", "high satisfactory", "highsatisfactory","e","excellent","m","f","male","female","no known disability","specific learning difficulty","other disability","sld","other","none","a","b","c","d","e","f","g","<","dk","white","asian","other","black","arab"),
    Scheme=c("UBSE","UBSE","UBSE","UBSE","UBSE","UBSE","UBSE","UBSE","USE","USE","USE","USE","USE","USE","CIDK","CIDK","CIDK","CIDK","CIDK","CIDK", "CIDK", "CIDK", "CIDK","CNINC","CNINC","CNINC","CNINC CNINC", "CNINC","CNINC","CNINC", "CNINC","PF","PF","PF","PF","PFE","PFE","PFE","PFE","PFE","PFE","UBLHE","UBLHE","UBLHE","UBLHE","UBLHE","UBLHE", "UBLHE", "UBLHE","UBLHE","UBLHE", "UBLHE", "UBLHE","UBLHE","UBLHE","Gender","Gender","Gender","Gender","Disability","Disability","Disability","Disability","Disability","Disability","Letters","Letters","Letters","Letters","Letters","Letters","Letters","Letters","Letters","Ethnicity","Ethnicity","Ethnicity","Ethnicity","Ethnicity"),
    Replacement=c("Unsatisfactory","Borderline","Satisfactory","Excellent","Unsatisfactory","Borderline","Satisfactory","Excellent","Unsatisfactory","Satisfactory","Excellent","Unsatisfactory","Satisfactory","Excellent","Correct","Incorrect","DontKnow","Correct","Incorrect","DontKnow", "DontKnow", "DontKnow", "DontKnow","Competent","Competent","Needs Improvement","Needs Improvement", "Needs Improvement","Not Competent","Not Competent", "Not Competent","Pass","Pass","Fail","Fail","Pass","Pass","Fail","Fail","Excellent","Excellent","Unsatisfactory","Unsatisfactory","Borderline","Borderline","Low-Satisfactory","Low-Satisfactory", "Low-Satisfactory", "Low-Satisfactory","High-Satisfactory","High-Satisfactory", "High-Satisfactory", "High-Satisfactory","Excellent","Excellent","Male","Female","Male","Female","No Known Disability","Specific Learning Difficulty","Other Disability","Specific Learning Difficulty","Other Disability","No Known Disability","A","B","C","D","E","F","G","DK","DK","White","Asian","Other","Black","arab"),
    Freq=0)

  # If Data is not numeric, strip formatting and change to lower-case to simplify Table.Schemes later

  if(Continue=="Yes" & is.numeric(Data)==FALSE){

    Data<-tolower(as.character(Data))

    # Calculate frequencies of each response in Table.Schemes

    for(i in unique(Data)){Table.Schemes$Freq[Table.Schemes$Value==i]<-length(Data[Data==i])}

    # Determine which scheme accounts for the most Data values and set DeterminedScheme

    for(i in Schemes<-as.character(unique(Table.Schemes$Scheme))){
      if(i==Schemes[1]){Table.SchemeFreqs<-data.frame(Scheme=Schemes,Freq=0)}
      Table.SchemeFreqs$Freq[Table.SchemeFreqs$Scheme==i] <- sum(subset(Table.Schemes, Table.Schemes$Scheme==i)$Freq)
      if(i==Schemes[length(Schemes)]){DeterminedScheme<-as.character(Table.SchemeFreqs$Scheme[Table.SchemeFreqs$Freq==max(Table.SchemeFreqs$Freq)])}}

  } # Close self-determination/Continue=="Yes" loop

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #### If Continue=="Yes" still, standardise Data ####

  if(exists("Replace.Data")==TRUE){Data<-tolower(as.character(Data))}

  if(Continue=="Yes" | exists("Replace.Data")==TRUE){
    for(i in Values<-unique(Data)){
      if(i==Values[1]){
        StandardisedData<-Data
        Temp<-subset(Table.Schemes, Table.Schemes$Scheme==DeterminedScheme[1])
        Temp$Replacement<-as.character(Temp$Replacement)}
      StandardisedData[StandardisedData==i]<-Temp$Replacement[Temp$Value==i]
    }}

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #### Returns ####

  if(length(DeterminedScheme)>1){warning("Multiple possible schemes. Data standardised using the first possible option. Please use Force.Scheme argument.")}

  return(SchemeDetect<-list(Scheme=DeterminedScheme, Data=StandardisedData))

} #Close function wrapper

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Testing/Demo Examples ####

#library("psychometricsPSMD")

#fnGPSchemeDetect()
#fnGPSchemeDetect(Data=dataExample$Grade.UBSE)
#fnGPSchemeDetect(Data=dataExample$Grade.UBSE, Force.Scheme = "UBSE")
#fnGPSchemeDetect(Force.Scheme = "UBSE")
#fnGPSchemeDetect(Data=dataExample)
#fnGPSchemeDetect(Data=dataExample, Force.Scheme = "UBSE")
#fnGPSchemeDetect(Data=c("U","S","E","U","S","E"))
#fnGPSchemeDetect(Data=c(1,2,3,4,5))
#fnGPSchemeDetect(Data=c(10.09,22.53,39.66,43.21,75.55))
#fnGPSchemeDetect(Data=c(109,112,334,421,7655))
#fnGPSchemeDetect(Data=c("male","female","m","f","f","MALE"))
#fnGPSchemeDetect(Data=dataExample$Grade.UBSE, Force.Scheme = "Hermione") # Need a check for this
#fnGPSchemeDetect(Data=c("U","S","B","U","S","W")) # Need to account for these

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
