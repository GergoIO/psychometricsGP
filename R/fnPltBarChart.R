#' Frequency or Percentage bar-charts of Grades or Responses.
#'
#' @description fnPltBarChart creates Frequency or Percentage bar-charts of Grades or Responses (and select characteristics such as Gender, Ethnicity, and Disability). It is designed to require only minimal input, but will request additional arguments if it cannot distinguish between two schemes (e.g. UBSE and USE when there are no B grades in Data).
#'
#' fnPltBarChart has also been designed to be as autonomous as possible, and can handle most variations in Data values, e.g. c("U","b","Satisfactory","E","excellent","S") will still be recognised as using a UBSE scheme, and the plot will relabel the values accordingly.
#'
#' The output from fnPltBarChart is a ggplot object which can then be saved or customised as needed.
#'
#' See Examples for more details.
#'
#' 10/10/2022 Renamed by Gergo, modified to fix a warning when using guides()
#'
#' @usage fnPltBarChart(Data, Type, Force.Scheme)
#'
#' @param Data Data should be a vector of values representing grades or responses, e.g. c("U","B","S","S","E") or Data.All$Grades.
#'
#' Factors will be coerced into suitable formats, data frames and lists will return a prompt to check the format.
#'
#' fnPltBarChart can handle combinations of capitalisation, alphanumeric or numeric abbreviations, and full-text labels for UBSE, USE, UBS, CIDK, CNINC, PFE, and PF schemes.
#'
#' The function can also handle selected themes outside of those related to responses and grades, though scheme detection is more restricted and less reliable. e.g. Gender, Ethnicity (will be reduced to "White"/"Asian"/"Other"), and Disability (will be reduced to "No Known Disability"/"Specific Learning Difficulty"/"Other Disability").
#' @param Type Type can be set to "Frequency" or "Percentage", and determines the y-axis scale. Type="Frequency" plots counts of each value in Data, whereas Type="Percentage" plots the percentage of each value in Data.
#'
#' fnPltBarChart can handle variations on "Frequency" and "Percentage" (within reason), e.g. "Perc" or "f"
#' @param Force.Scheme Force.Scheme is an optional argument that can take the values "USE", "UBS", "PF", "CIDK", and "CNINC". It is requested when the function cannot determine a single scheme from the values in Data. For example, if Data consists of a string of "C" values, the function will ask the user to specify Force.Scheme="CIDK" or Force.Scheme="CNINC", or if Data only contains U, S, and E grades (or variations), it will ask the user specify Force.Scheme="USE" if the scheme should be USE rather than UBSE.
#'
#' These pieces of additional information allow the correct handling of USE, UBS, and PF schemes, and C-only Data.
#'
#' Force.Scheme has no effect when a scheme can be determined or on totally unknown themes (unknown as opposed to indeterminate).
#'
#'  @note DZ260417: Added a fix for Type being converted to a factor after the reference table being used to handle abbreviated type names. This seems to work, but it needs checking with respect to different classes of Type object and associated error messages.
#'
#' @examples
#' # fnPltBarChart Help file; can handle UBSE, USE, UBS, CIDK, CNINC, PFE, PF, and Gender
#'
#'   ?fnPltBarChart
#'
#' # Basic Usage; Data examples from Data.Example in PSMD.Psychometrics
#'
#'   fnPltBarChart(Data=dataExample$Grade.UBSE, Type="Percentage")
#'
#'   fnPltBarChart(Data=dataExample$Response.CIDK, Type="Frequency")
#'
#' # Ability to handle variation in Data values (within schemes)
#'
#'   Data<-c("u","e","b","satisfactory","S","s","E")
#'   fnPltBarChart(Data,Type="Percentage")
#'
#'   Data<-c("C","I","Incorrect","Correct","correct","DK","dont know", "Don't Know")
#'   fnPltBarChart(Data,Type="Percentage")
#'
#' # Warnings when either or both inputs are missing
#'
#'   fnPltBarChart()
#'
#'   fnPltBarChart(Type="Percentage")
#'
#' # Warnings when either or both inputs are of the wrong type or a type that can't be coerced into the correct type.
#'
#'   head(dataExample)
#'   fnPltBarChart(dataExample, Type="Percentage") # e.g. can't input a whole data.frame (this example)
#'
#'   fnPltBarChart(Data=dataExample$Response.CIDK, Type="Chickens") # e.g. if an argument value is incorrect
#'
#' # If there is a rogue value in Data, a plot is generate but a warning raised.
#'
#'   Data<-c("U","B","S","E","E","DK") # Grade or response value but inconsistent with scheme.
#'   fnPltBarChart(Data,Type="Percentage")
#'
#'   Data<-c("U","B","S","E","E","W") # Value that isn't a grade or response, warning specifies unknown value.
#'   fnPltBarChart(Data,Type="Percentage")
#'
#' # If there is too little information in Data to determine or differentiate a scheme, instructions are displayed
#'
#'   Data<-c("U","S","S","S","E","E") # e.g. could be UBSE where noone was awarded B, or could be USE scheme.
#'   fnPltBarChart(Data,Type="Percentage") # Defaults to UBSE, but asks if USE and explains how to correct.
#'   fnPltBarChart(Data,Type="Percentage", Force.Scheme = "USE")
#'
#' # Outputs a ggplot object, which can them be editted as necessary.
#'
#'   Plot.BarChart.CNINC<-fnPltBarChart(Data=dataExample$Grade.CNINC, Type="Frequency")
#'   Plot.BarChart.CNINC # plot output
#'   Plot.BarChart.CNINC+ggtitle("C-NI-NC Response Frequencies") # edits
#'
#' @author Dr Daniel Zahra, \email{daniel.zahra@plymouth.ac.uk}
#'
#' @export

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### fnPltBarChart - last updated 250417 ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Function designed to plot grade and response frequencies, counts, and percentages.

# Inputs
#
# Data      Single vector containing all data points; e.g. c("U", "S", "S", "B", "E") or Data.All$Grades
#           Can be of any data type.
#           Can handle any combination of abbreviations and/or full-text labels
#           (Within reason; add any unusual ones to Data.Scheme)
#
# Type      Character string, specifying whether the plot should display is "Frequency" or "Percentage"
#           Can handle some abbreviations and variations of these two terms.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# TO DEVELOP
#
# DNAs currently treated as unknown value so scheme isn't determined.
#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fnPltBarChart<-function(Data="MinaHarker", Type="LucyWestenra", Force.Scheme="Renfield"){

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##### Check Inputs - Specified and Correct Format ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  # Test if Data and/or Type are missing and display warnings
  # These warnings are predicated on checking for Data in the first variable slot and Type in the second; so if you put Type but no Data, the warning will be 'No Type' as it assumes the first (and only) variable specified is Data.

  # Check if Data or Type arguments are factors, and un-factorise if yes.

  if(is.factor(Data)){Data<-as.character(Data)}
  if(is.factor(Type)){Type<-as.character(Type)}

  # Correct any Type typos or variations (in single-item Type arguments; otherwise leave for error chekcs that follow)
  # Single-item factors vectors, lists, and dataframes are accepted; multiple-item vectors, lists, and dataframes are skipped and checked later (Type-gormat error message generated)

  if(is.vector(Type)){
    if(length(Type)==1){
      Table.TypeRefs<-data.frame(Type=c("Percentage","percentage","percent","Percent","perc","Perc","p","Percentages","percentages","percents","Percents","percs","Percs","ps","Frequency","frequency","freq","Freq","F","f","Frequencies","frequencies","freqs","Freqs","Fs","fs"),Replacement=c(rep("Percentage",14),rep("Frequency",12)))
      if(length(Table.TypeRefs$Type[Table.TypeRefs$Type==Type])>=1){Type<-Table.TypeRefs$Replacement[Table.TypeRefs$Type==Type]}
      if(is.factor(Type)){Type<-as.character(Type)}
    }}

  # Tabulate inputs and proprties to create reference table for conditional checks.
  # Defaults to neither Data not Type being specified and both being the wrong format.

  Table.Checks<-data.frame(Required=c("Data","Type"), Specified=c("No","No"), Format=c("Incorrect","Incorrect"))
  for(i in colnames(Table.Checks)){Table.Checks[[i]]<-as.character(Table.Checks[[i]])} # to make sure Data.Scheme$Replacement is not factorised (if factorised, replacement of values is thrown by level numbers being used in some versions/settings of R)

  # Check if Data has been specified (nested conditionals to handle checking dataframes and lists)

  if(exists("Data") & !is.vector(Data)){
    if(Data[1,1]=="MinaHarker"){Table.Checks$Specified[1]<-"No"}
    if(Data[1,1]!="MinaHarker"){Table.Checks$Specified[1]<-"Yes"}}
  if(exists("Data") & is.vector(Data)){if(Data[1]!="MinaHarker"){Table.Checks$Specified[1]<-"Yes"}}

  # Check if Data is in the correct format (unfactorised single vector)

  if(Table.Checks$Specified[1]=="Yes"){
    if(is.list(Data) & Data[[1]][1]!="MinaHarker"){Table.Checks$Format[1]<-"Incorrect"}
    if(!is.list(Data) & is.vector(Data)){Table.Checks$Format[1]<-"Correct"}}

  # Check if Type is specified (nested conditionals to handle checking dataframes and lists)

  if(exists("Type") & !is.vector(Type)){
    if(Type[1,1]=="LucyWestenra"){Table.Checks$Specified[2]<-"No"}
    if(Type[1,1]!="LucyWestenra"){Table.Checks$Specified[2]<-"Yes"}}
  if(exists("Type") & is.vector(Type)){if(Type[1]!="LucyWestenra"){Table.Checks$Specified[2]<-"Yes"}}

  # Check if Type is in the correct format and one of the valid options

  if(Table.Checks$Specified[2]=="Yes"){
    if(is.vector(Type) &  length(Type)==1){
      if(Type[1]!="LucyWestenra"){
        if(Type[1]=="Percentage" | Type[1]=="Frequency"){
          Table.Checks$Format[2]<-"Correct"}}}}

  # Display appropriate warnings re. required inputs

  Checks.OK<-"Yes" # Conditional switch that is altered to toggle additional warning messages and completion of function

  if(length(Table.Checks$Specified[Table.Checks$Specified=="No"])==1){
    warning(paste("Function aborted. Argument missing - ",toString(Table.Checks$Require[Table.Checks$Specified=="No"]),sep=""))
    Checks.OK<-"No"}
  if(length(Table.Checks$Specified[Table.Checks$Specified=="No"])==2){
    warning(paste("Function Aborted. Arguments missing - ",toString(Table.Checks$Require[Table.Checks$Specified=="No"]),sep=""))
    Checks.OK<-"No"}

  if(Checks.OK=="Yes"){
    if(length(Table.Checks$Format[Table.Checks$Format=="Incorrect"])==1){
      warning(paste("Function aborted. Input in incorrect format - ",toString(Table.Checks$Require[Table.Checks$Format=="Incorrect"]),". Please see ?fnPltBarChart for required format.",sep=""))
      Checks.OK<-"No"}
    if(length(Table.Checks$Format[Table.Checks$Format=="Incorrect"])==2){
      warning(paste("Function Aborted. Inputs in incorrect format - ",toString(Table.Checks$Require[Table.Checks$Format=="Incorrect"]),". Please see ?fnPltBarChart for required format.",sep=""))
      Checks.OK<-"No"}}

  # Check to see if inputs and formats are ok.

  if(length(Table.Checks$Specified[Table.Checks$Specified=="Yes"])<2 | length(Table.Checks$Format[Table.Checks$Format=="Correct"])<2){
    Checks.OK<-"No"
    # do nothng if inputs incorrect or missing; errors will have been displayed by preceeding lines
  }else{
    Checks.OK<-"Yes"} # If all inputs ok, set Checks.OK to "Yes" and continue.

  # From here, each section is within a Checks.OK=="Yes" conditional, and where further checks are carried out, Checks.OK status is changed if errors are encountered

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Create single-variable data frame
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  if(Checks.OK=="Yes"){
    Data.Plot<-data.frame(variable=as.character(Data))
    Data.Plot$variable<-as.character(Data.Plot$variable)} # to make sure Data.Plot$variable is not factorised (if factorised, replacement of values is thrown by level numbers being used in some versions of R)

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Determine Scheme; whether data is U/B/S/E, C/NI/NC, or C/DK/I
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  if(Checks.OK=="Yes"){

    # Create dataframe of all possible grade/response options and which scheme they're from, add freqs

    Data.Scheme<-data.frame(
      Value=c("U","B","S","E","Unsatisfactory","Borderline","Satisfactory","Excellent","C","C","I","DK","Correct","Incorrect","DontKnow","Dont Know", "Don't Know","NI","NC","Competent","Needs Improvement","Not Competent", "NeedsImprovement","NotCompetent","u","b","s","e","unsatisfactory", "borderline", "satisfactory", "excellent","dk","correct","incorrect","dontknow","dont know","don't know","ni","nc","competent","needs improvement","not competent","needsimprovement","notcompetent","Pass","Fail","pass","fail","p","f","P","F","1","0","-0.25","Male","Female","male","female","M","F","m","f","c","c","White","white","Asian","asian","Other","other","Black","black","Arab","arab","No known disability","Specific learning difficulty","Other disability","No Known Disability","Specific Learning Difficulty","Other Disability","no known disability","specific learning difficulty","other disability","SLD","sld","Other","other","none","None","OTHER","PASS","FAIL","E","e","Excellent","excellent","i"),
      Scheme=c("UBSE","UBSE","UBSE","UBSE","UBSE","UBSE","UBSE","UBSE","CIDK","CNINC","CIDK","CIDK","CIDK","CIDK","CIDK","CIDK", "CIDK","CNINC","CNINC","CNINC","CNINC","CNINC", "CNINC","CNINC","UBSE","UBSE","UBSE","UBSE","UBSE","UBSE","UBSE","UBSE","CIDK","CIDK","CIDK","CIDK","CIDK","CIDK","CNINC","CNINC","CNINC","CNINC","CNINC","CNINC","CNINC","PFE","PFE","PFE","PFE","PFE","PFE","PFE","PFE","CIDK","CIDK","CIDK","Gender","Gender","Gender","Gender","Gender","Gender","Gender","Gender","CIDK","CNINC","Ethnicity","Ethnicity","Ethnicity","Ethnicity","Ethnicity","Ethnicity","Ethnicity","Ethnicity","Ethnicity","Ethnicity","Disability","Disability","Disability","Disability","Disability","Disability","Disability","Disability","Disability","Disability","Disability","Disability","Disability","Disability","Disability","Disability","PFE","PFE","PFE","PFE","PFE","PFE","CIDK"),
      Freq=0,
      Replacement=c("Unsatisfactory","Borderline","Satisfactory","Excellent","Unsatisfactory","Borderline","Satisfactory","Excellent","Correct","Competent","Incorrect","Don't Know","Correct","Incorrect","Don't Know","Don't Know", "Don't Know","Needs Improvement","Not Competent","Competent","Needs Improvement","Not Competent", "Needs Improvement","Not Competent","Unsatisfactory","Borderline","Satisfactory","Excellent","Unsatisfactory","Borderline","Satisfactory","Excellent","Don't Know","Correct","Incorrect","Don't Know","Don't Know","Don't Know","Needs Improvement","Not Competent","Competent","Needs Improvement","Not Competent","Needs Improvement","Not Competent","Pass","Fail","Pass","Fail","Pass","Fail","Pass","Fail","Correct","Don't Know","Incorrect","Male","Female","Male","Female","Male","Female","Male","Female","Correct","Competent","White","White","Asian","Asian","Other","Other","Other","Other","Other","Other","No Known Disability","Specific Learning Difficulty","Other Disability","No Known Disability","Specific Learning Difficulty","Other Disability","No Known Disability","Specific Learning Difficulty","Other Disability","Specific Learning Difficulty","Specific Learning Difficulty","Other Disability","Other Disability","No Known Disability","No Known Disability","Other Disability","Pass","Fail","Excellent","Excellent","Excellent","Excellent","Incorrect"))

    Data.Scheme$Replacement<-as.character(Data.Scheme$Replacement) # to make sure Data.Scheme$Replacement is not factorised (if factorised, replacement of values is thrown by level numbers being used in some versions/settings of R)
    Data.Scheme$Value<-as.character(Data.Scheme$Value) # ditto

    for(i in Data.Scheme$Value){Data.Scheme$Freq[Data.Scheme$Value==i]<-length(Data.Plot$variable[Data.Plot$variable==i])}

    # Look for any unknown values in the input Data (currently stored in Data.Plot$variable), and abort with warning if Unknown.Values>=1

    Data.ValuesCheck<-data.frame(Data=unique(Data.Plot$variable), Freq=0)
    Data.ValuesCheck$Data<-as.character(Data.ValuesCheck$Data) # to clear any automatic factoring in some R builds/settings.

    for(i in unique(Data.ValuesCheck$Data)){Data.ValuesCheck$Freq[Data.ValuesCheck$Data==i]<-length(Data.Scheme$Value[Data.Scheme$Value==i])}

    Unknown.Values<-Data.ValuesCheck$Data[Data.ValuesCheck$Freq==0]

  }

  # Create dataframe to determine most likely scheme (the scheme which accounts for the highest number of values in Data, and accounts for at least 95% of them). If two schemes or none meet the criteria, levels are determined from Data. Otherwise levels and labels are set by values in Data.Scheme

  if(Checks.OK=="Yes"){

    Estimated.Scheme<-"Unknown" # Default to Estimated.Scheme=="Unknown"

    if(length(Unknown.Values)==0){

      # Temp line to check code issue with CIDK/CNINC resulting in "Scheme not found".
      # If you have nested conditionals which base based on evaluation an object, but the object doesn't exist
      # (e.g. Scheme is Scheme doesn't exist), it aborts the entire nested block... BUT DOESNT GIVE YOU AN ERROR MESSAGE.
      # This is a work-around to create a dummy Scheme for it to evaluate outside the nested conditionals that follow
      # (and hence avoid the abortive error); a proper fix would be to make each nested conditional dependent on
      # exists("Scheme")==TRUE, but I have other things to add before making it pretty lol.
      Scheme<-"Unknown" # dummy work-around, see comments above

      # Return to normal script after work-around

      for(i in unique(Data.Scheme$Scheme)){
        if(i==unique(Data.Scheme$Scheme)[1]){Data.Scheme.Determination<-data.frame(Scheme=character(),Freq=numeric())}
        Temp<-data.frame(Scheme=i,Freq=sum(Data.Scheme$Freq[Data.Scheme$Scheme==i]))
        Data.Scheme.Determination<-rbind(Data.Scheme.Determination,Temp)}

      Data.Scheme.Determination$Perc<-100*(Data.Scheme.Determination$Freq/length(Data.Plot$variable))

      Estimated.Scheme<-as.character(Data.Scheme.Determination$Scheme[Data.Scheme.Determination$Freq==max(Data.Scheme.Determination$Freq) & Data.Scheme.Determination$Perc>=95.00])

      # Conditional to display warning if indeterminate scheme and no Force.Scheme statement.

      if(Force.Scheme!="USE" & Force.Scheme!="CIDK" & Force.Scheme!="CNINC"){
        if(length(Estimated.Scheme)>1 | length(Estimated.Scheme)==0){
          warning("Could not determine scheme. Using levels as they appear in Data")
          Scheme<-"Unknown"
        }}

      # Nested conditionals to determine effect of Force.Scheme=="USE"

      if(Force.Scheme=="USE"){
        ### add check here incase USE is being force by accident/inappropriately ###
        Scheme<-Estimated.Scheme
        if(Scheme=="UBSE"){

          Temp<-Data.Plot
          for(i in unique(Temp$variable)){
            Temp$variable[Temp$variable==i]<-Data.Scheme$Replacement[Data.Scheme$Value==i & Data.Scheme$Scheme=="UBSE"]}
          Temp$variable<-as.character(Temp$variable)

          if(length(Temp$variable[Temp$variable=="Borderline"])==0){

            Scheme<-"USE"
            Scheme.Text<-(paste("Scheme overridden. Using: ",Scheme,sep=""))}

          else{if(length(Estimated.Scheme)==1){
            Scheme<-Estimated.Scheme
            Scheme.Text<-(paste("Scheme determined: ",Scheme,sep=""))}}

        }else{if(length(Estimated.Scheme)==1){
          Scheme<-Estimated.Scheme
          Scheme.Text<-(paste("Scheme determined: ",Scheme,sep=""))}}

      }else{

        if(length(Estimated.Scheme)==1){
          Scheme<-Estimated.Scheme
          Scheme.Text<-(paste("Scheme determined: ",Scheme,sep=""))}}

      # Nested conditionals to determine effect of Force.Scheme="PF"

      if(Force.Scheme=="PF"){
        ### add check here incase PF is being force by accident/inappropriately ###
        Scheme<-Estimated.Scheme
        if(Scheme=="PFE"){

          Temp<-Data.Plot
          for(i in unique(Temp$variable)){
            Temp$variable[Temp$variable==i]<-Data.Scheme$Replacement[Data.Scheme$Value==i & Data.Scheme$Scheme=="PFE"]}
          Temp$variable<-as.character(Temp$variable)

          if(length(Temp$variable[Temp$variable=="Excellent"])==0){

            Scheme<-"PF"
            Scheme.Text<-(paste("Scheme overridden. Using: ",Scheme,sep=""))}

          else{if(length(Estimated.Scheme)==1){
            Scheme<-Estimated.Scheme
            Scheme.Text<-(paste("Scheme determined: ",Scheme,sep=""))}}

        }else{if(length(Estimated.Scheme)==1){
          Scheme<-Estimated.Scheme
          Scheme.Text<-(paste("Scheme determined: ",Scheme,sep=""))}}

      }else{

        if(length(Estimated.Scheme)==1 & Scheme!="USE"){
          Scheme<-Estimated.Scheme
          Scheme.Text<-(paste("Scheme determined: ",Scheme,sep=""))}}

      # Nested conditionals to determine effect of Force.Scheme="UBS"

      if(Force.Scheme=="UBS"){
        ### add check here incase PF is being force by accident/inappropriately ###
        Scheme<-Estimated.Scheme
        if(Scheme=="UBSE"){

          Temp<-Data.Plot
          for(i in unique(Temp$variable)){
            Temp$variable[Temp$variable==i]<-Data.Scheme$Replacement[Data.Scheme$Value==i & Data.Scheme$Scheme=="UBSE"]}
          Temp$variable<-as.character(Temp$variable)

          if(length(Temp$variable[Temp$variable=="Excellent"])==0){

            Scheme<-"UBS"
            Scheme.Text<-(paste("Scheme overridden. Using: ",Scheme,sep=""))}

          else{if(length(Estimated.Scheme)==1){
            Scheme<-Estimated.Scheme
            Scheme.Text<-(paste("Scheme determined: ",Scheme,sep=""))}}

        }else{if(length(Estimated.Scheme)==1){
          Scheme<-Estimated.Scheme
          Scheme.Text<-(paste("Scheme determined: ",Scheme,sep=""))}}

      }else{

        if(length(Estimated.Scheme)==1 & Scheme!="USE" & Scheme!="PF"){
          Scheme<-Estimated.Scheme
          Scheme.Text<-(paste("Scheme determined: ",Scheme,sep=""))}}

      # Conditionals for handling Force.Scheme=="CIDK"/"CNINC"

      if(Force.Scheme=="CIDK" | Force.Scheme=="CNINC"){
        if(!exists("Scheme")){
          Scheme<-Force.Scheme
          Scheme.Text<-(paste("Scheme overridden. Using: ",Scheme,sep=""))}}

      if(Force.Scheme=="CIDK" | Force.Scheme=="CNINC"){
        if(Scheme!="UBSE" & Scheme!="USE" & Scheme!="PF" & Scheme!="PFE" & Scheme!="CIDK" & Scheme!="CNINC" & Scheme!="UBS"){
          Scheme<-Force.Scheme
          Scheme.Text<-(paste("Scheme overridden. Using: ",Scheme,sep=""))}}

      if(Force.Scheme=="USE" | Force.Scheme=="PF" | Force.Scheme=="UBS"){
        if(Estimated.Scheme[1]=="CIDK" | Estimated.Scheme[1]=="CNINC"){
          Scheme<-Estimated.Scheme[1]
          Scheme.Text<-(paste("Scheme determined: ",Scheme,sep=""))
          if(length(Estimated.Scheme)>1|length(Estimated.Scheme)==0){
            warning("IGNORE THE PREVIOUS WARNINGS. I was just having a think through the options and talking to myself. I still couldn't quite work out the scheme, so took a guess. If it's not what you wanted, please try adding Force.Scheme=\"CIDK\" or Force.Scheme=\"CNINC\" to the function call. Thank you.")}
        }
      }

      if(Scheme!="Unknown"){

        # Commented out to suppress dialogue
        #print(Scheme.Text)

      }

    } # Close Unknown.Values==0 conditional


    if(length(Unknown.Values)>=1){
      if(length(Unknown.Values)==1){warning(paste("Could not determine scheme - Unknown value in Data: ",toString(Unknown.Values),". Using levels as they appear in Data",sep=""))}
      if(length(Unknown.Values)>1){warning(paste("Could not determine scheme - Unknown values in Data: ",toString(Unknown.Values),". Using levels as they appear in Data",sep=""))}
      Scheme<-"Unknown"}

    # Use Scheme to set xlab

    if(Scheme=="Unknown"){xlab.text<-"Unknown Scheme"}
    if(Scheme=="UBSE"){xlab.text<-"Grade"}
    if(Scheme=="CIDK"){xlab.text<-"Response"}
    if(Scheme=="CNINC"){xlab.text<-"Grade"}
    if(Scheme=="PFE" | Scheme=="PF"){xlab.text<-"Grade"}
    if(Scheme=="USE"){xlab.text<-"Grade"}
    if(Scheme=="Gender"){xlab.text<-"Gender"}
    if(Scheme=="Ethnicity"){xlab.text<-"Ethnicity"}
    if(Scheme=="Disability"){xlab.text<-"Disability"}
    if(Scheme=="UBS"){xlab.text<-"Grade"}

    # Correct values in Data.Plot$variable to Data.Scheme$Replacement and factorise accordingly

    UBSE.Levels<-c("Unsatisfactory","Borderline","Satisfactory","Excellent")
    CIDK.Levels<-c("Correct","Incorrect","Don't Know")
    CNINC.Levels<-c("Competent","Needs Improvement","Not Competent")
    PFE.Levels<-c("Excellent","Pass","Fail")
    PF.Levels<-c("Pass","Fail")
    USE.Levels<-c("Unsatisfactory","Satisfactory","Excellent")
    Gender.Levels<-c("Female","Male")
    Ethnicity.Levels<-unique(subset(Data.Scheme, Data.Scheme$Scheme=="Ethnicity")$Replacement)
    Disability.Levels<-unique(subset(Data.Scheme, Data.Scheme$Scheme=="Disability")$Replacement)
    UBS.Levels<-c("Unsatisfactory","Borderline","Satisfactory")

    if(Scheme=="Unknown"){
      Data.Plot$variable<-factor(Data.Plot$variable)}

    if(Scheme=="UBSE"){
      for(i in unique(Data.Plot$variable)){
        Data.Plot$variable[Data.Plot$variable==i]<-Data.Scheme$Replacement[Data.Scheme$Value==i & Data.Scheme$Scheme=="UBSE"]}
      Data.Plot$variable<-factor(Data.Plot$variable, levels=UBSE.Levels)}

    if(Scheme=="CIDK"){
      for(i in unique(Data.Plot$variable)){
        Data.Plot$variable[Data.Plot$variable==i]<-Data.Scheme$Replacement[Data.Scheme$Value==i & Data.Scheme$Scheme=="CIDK"]}
      Data.Plot$variable<-factor(Data.Plot$variable, levels=CIDK.Levels)}

    if(Scheme=="CNINC"){
      for(i in unique(Data.Plot$variable)){
        Data.Plot$variable[Data.Plot$variable==i]<-Data.Scheme$Replacement[Data.Scheme$Value==i & Data.Scheme$Scheme=="CNINC"]}
      Data.Plot$variable<-factor(Data.Plot$variable, levels=CNINC.Levels)}

    if(Scheme=="PFE"){
      for(i in unique(Data.Plot$variable)){
        Data.Plot$variable[Data.Plot$variable==i]<-Data.Scheme$Replacement[Data.Scheme$Value==i & Data.Scheme$Scheme=="PFE"]}
      Data.Plot$variable<-factor(Data.Plot$variable, levels=PFE.Levels)}

    if(Scheme=="PF"){
      Temp<-subset(Data.Scheme,Data.Scheme$Scheme=="PFE")
      Temp<-subset(Temp,Temp$Replacement!="Excellent")
      for(i in unique(Data.Plot$variable)){
        Data.Plot$variable[Data.Plot$variable==i]<-Temp$Replacement[Temp$Value==i]}
      Data.Plot$variable<-factor(Data.Plot$variable, levels=PF.Levels)}

    if(Scheme=="USE"){
      Temp<-subset(Data.Scheme,Data.Scheme$Scheme=="UBSE")
      Temp<-subset(Temp,Temp$Replacement!="Borderline")
      for(i in unique(Data.Plot$variable)){
        Data.Plot$variable[Data.Plot$variable==i]<-Temp$Replacement[Temp$Value==i]}
      Data.Plot$variable<-factor(Data.Plot$variable, levels=USE.Levels)}

    if(Scheme=="UBS"){
      Temp<-subset(Data.Scheme,Data.Scheme$Scheme=="UBSE")
      Temp<-subset(Temp,Temp$Replacement!="Excellent")
      for(i in unique(Data.Plot$variable)){
        Data.Plot$variable[Data.Plot$variable==i]<-Temp$Replacement[Temp$Value==i]}
      Data.Plot$variable<-factor(Data.Plot$variable, levels=UBS.Levels)}

    if(Scheme=="Gender"){
      for(i in unique(Data.Plot$variable)){
        Data.Plot$variable[Data.Plot$variable==i]<-Data.Scheme$Replacement[Data.Scheme$Value==i & Data.Scheme$Scheme=="Gender"]}
      Data.Plot$variable<-factor(Data.Plot$variable, levels=Gender.Levels)}

    if(Scheme=="Ethnicity"){
      for(i in unique(Data.Plot$variable)){
        Data.Plot$variable[Data.Plot$variable==i]<-Data.Scheme$Replacement[Data.Scheme$Value==i & Data.Scheme$Scheme=="Ethnicity"]}
      Data.Plot$variable<-factor(Data.Plot$variable, levels=Ethnicity.Levels)}

    if(Scheme=="Disability"){
      for(i in unique(Data.Plot$variable)){
        Data.Plot$variable[Data.Plot$variable==i]<-Data.Scheme$Replacement[Data.Scheme$Value==i & Data.Scheme$Scheme=="Disability"]}
      Data.Plot$variable<-factor(Data.Plot$variable, levels=Disability.Levels)}

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Determine colours based on Scheme
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    #if(Scheme=="Unknown"){Colours<-rep("darkblue",length(unique(Data.Plot$variable)))}
    #if(Scheme=="UBSE"){Colours<-c("darkred","darkorange","darkgreen","darkblue")}
    #if(Scheme=="USE"){Colours<-c("darkred","darkgreen","darkblue")}
    #if(Scheme=="CIDK"){Colours<-c("darkgreen","darkred","darkorange")}
    #if(Scheme=="CNINC"){Colours<-c("darkgreen","darkorange","darkred")}
    #if(Scheme=="PFE"){Colours<-c("darkblue","darkgreen","darkred")}
    #if(Scheme=="PF"){Colours<-c("darkgreen","darkred")}
    #if(Scheme=="Gender"){Colours<-c("#114477","#4477AA")}
    #if(Scheme=="Ethnicity"){Colours<-rep("darkblue",length(Ethnicity.Levels))}
    #if(Scheme=="Disability"){Colours<-rep("darkblue",length(Disability.Levels))}
    #if(Scheme=="UBS"){Colours<-c("darkred","darkorange","darkgreen")}

    if(Scheme=="Unknown"){Colours<-rep("#3D52A1",length(unique(Data.Plot$variable)))}
    if(Scheme=="UBSE"){Colours<-c("#D92120","#E68B33","#86BB6A","#3D52A1")}
    if(Scheme=="USE"){Colours<-c("#D92120","#86BB6A","#3D52A1")}
    if(Scheme=="CIDK"){Colours<-c("#86BB6A","#D92120","#E68B33")}
    if(Scheme=="CNINC"){Colours<-c("#86BB6A","#E68B33","#D92120")}
    if(Scheme=="PFE"){Colours<-c("#3D52A1","#86BB6A","#D92120")}
    if(Scheme=="PF"){Colours<-c("#86BB6A","#D92120")}
    if(Scheme=="Gender"){Colours<-c("#44AA77","#4477AA")}
    if(Scheme=="Ethnicity"){Colours<-rep("#44AA77",length(Ethnicity.Levels))}
    if(Scheme=="Disability"){Colours<-rep("#44AA77",length(Disability.Levels))}
    if(Scheme=="UBS"){Colours<-c("#D92120","#E68B33","#86BB6A")}

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Custom ggplot theme - replace/update when changed
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    theme_psmd <- function(){
      Text.Size<-10
      Text.Basic <- element_text(size = Text.Size, colour = "black", face = "plain")
      Text.Bold <- element_text(size = Text.Size, colour = "black", face = "bold")
      Text.Title <- element_text(size = 1.1*Text.Size, colour = "black", face = "bold")
      theme_bw() +
        theme(
          legend.key = element_blank(),
          strip.background = element_blank(),
          text = Text.Basic,
          plot.title = Text.Title,
          axis.title = Text.Bold,
          axis.text = Text.Basic,
          legend.title = Text.Bold,
          legend.text = Text.Basic,
          panel.border = element_rect(fill=NA, colour="#D3D3D3"),
          panel.grid.major = element_line(colour = "#D3D3D3"),
          panel.grid.minor = element_line(colour = "#F5F5F5"),
          axis.line.x = element_line(colour = "#000000"),
          axis.line.y = element_line(colour = "#000000"))}

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Create basic plot
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    if(Type=="Frequency"){

      # Create frequency table and determine frequencies; easier to do this in the same way as the percentage plot to maintain colours across missing categories than use geom_bar(stat="count") with conditional colour determination.
      Data.Frequencies<-data.frame(variable=c(unique(levels(Data.Plot$variable))), Frequency=0)

      for(i in Data.Frequencies$variable){
        Data.Frequencies$Frequency[Data.Frequencies$variable==i]<-(length(Data.Plot$variable[Data.Plot$variable==i]))
      }

      # Factorise according to Scheme

      if(Scheme=="UBSE"){Data.Frequencies$variable<-factor(Data.Frequencies$variable, levels=UBSE.Levels)}
      if(Scheme=="CIDK"){Data.Frequencies$variable<-factor(Data.Frequencies$variable, levels=CIDK.Levels)}
      if(Scheme=="CNINC"){Data.Frequencies$variable<-factor(Data.Frequencies$variable, levels=CNINC.Levels)}
      if(Scheme=="PFE"){Data.Frequencies$variable<-factor(Data.Frequencies$variable, levels=PFE.Levels)}
      if(Scheme=="PF"){Data.Frequencies$variable<-factor(Data.Frequencies$variable, levels=PF.Levels)}
      if(Scheme=="USE"){Data.Frequencies$variable<-factor(Data.Frequencies$variable, levels=USE.Levels)}
      if(Scheme=="Unknown"){Data.Frequencies$variable<-factor(Data.Frequencies$variable)}
      if(Scheme=="Gender"){Data.Frequencies$variable<-factor(Data.Frequencies$variable, levels=Gender.Levels)}
      if(Scheme=="Ethnicity"){Data.Frequencies$variable<-factor(Data.Frequencies$variable, levels=Ethnicity.Levels)}
      if(Scheme=="Disability"){Data.Frequencies$variable<-factor(Data.Frequencies$variable, levels=Disability.Levels)}
      if(Scheme=="UBS"){Data.Frequencies$variable<-factor(Data.Frequencies$variable, levels=UBS.Levels)}

      # Create Plot

      Plot.1<-ggplot(data=Data.Frequencies, aes(x=variable, y=Frequency, fill=variable)) +
        geom_bar(stat="identity") +
        scale_fill_manual(values=Colours) +
        guides(fill= "none") +
        ylab("Frequency") +
        xlab(xlab.text) +
        scale_x_discrete(drop=FALSE) +
        theme_psmd()}

    if(Type=="Percentage"){

      # Calculate Percentages of each level

      Data.Percentage<-data.frame(variable=c(unique(levels(Data.Plot$variable))), Percentage=0)

      for(i in Data.Percentage$variable){
        Data.Percentage$Percentage[Data.Percentage$variable==i]<-100*(length(Data.Plot$variable[Data.Plot$variable==i])/length(Data.Plot$variable))
      }

      # Factorise according to Scheme

      if(Scheme=="UBSE"){Data.Percentage$variable<-factor(Data.Percentage$variable, levels=UBSE.Levels)}
      if(Scheme=="CIDK"){Data.Percentage$variable<-factor(Data.Percentage$variable, levels=CIDK.Levels)}
      if(Scheme=="CNINC"){Data.Percentage$variable<-factor(Data.Percentage$variable, levels=CNINC.Levels)}
      if(Scheme=="USE"){Data.Percentage$variable<-factor(Data.Percentage$variable, levels=USE.Levels)}
      if(Scheme=="Unknown"){Data.Percentage$variable<-factor(Data.Percentage$variable)}
      if(Scheme=="PFE"){Data.Percentage$variable<-factor(Data.Percentage$variable, levels=PFE.Levels)}
      if(Scheme=="PF"){Data.Percentage$variable<-factor(Data.Percentage$variable, levels=PF.Levels)}
      if(Scheme=="Gender"){Data.Percentage$variable<-factor(Data.Percentage$variable, levels=Gender.Levels)}
      if(Scheme=="Ethnicity"){Data.Percentage$variable<-factor(Data.Percentage$variable, levels=Ethnicity.Levels)}
      if(Scheme=="Disability"){Data.Percentage$variable<-factor(Data.Percentage$variable, levels=Disability.Levels)}
      if(Scheme=="UBS"){Data.Percentage$variable<-factor(Data.Percentage$variable, levels=UBS.Levels)}

      # Create Plot

      Plot.1<-ggplot(data=Data.Percentage, aes(x=variable, y=Percentage, fill=variable)) +
        geom_bar(stat="identity") +
        scale_fill_manual(values=Colours) +
        guides(fill= "none") +
        ylim(0,100) +
        ylab("Percentage") +
        xlab(xlab.text) +
        scale_x_discrete(drop=FALSE) +
        theme_psmd()}

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Check for lack of-B grades and question if UBSE is correct
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    if(Scheme=="UBSE"){
      if(length(Data.Plot$variable[Data.Plot$variable=="Borderline"])==0){
        warning("We determined the scheme was UBSE but detected there were no Borderline grades in the data. If this is incorrect, and the scheme to use is USE, please add the argument Force.Scheme=\"USE\" to the function call. Thank you.")
      }}

    if(Scheme=="PFE"){
      if(length(Data.Plot$variable[Data.Plot$variable=="Excellent"])==0){
        warning("We determined the scheme was PFE but detected there were no Excellent grades in the data. If this is incorrect, and the scheme to use is PF, please add the argument Force.Scheme=\"PF\" to the function call. Thank you.")
      }}

    if(length(Estimated.Scheme)==2 & Scheme!="CIDK" & Scheme!="CNINC"){
      warning("We were unable to determine whether the scheme was CIDK or CNINC. Please add the argument Force.Scheme=\"CIDK\" or Force.Scheme=\"CNINC\" to the function call as appropriate. Thank you.")
    }

    if(Scheme=="UBSE"){
      if(length(Data.Plot$variable[Data.Plot$variable=="Excellent"])==0){
        warning("We determined the scheme was UBSE but detected there were no Excellent grades in the data. If this is incorrect, and the scheme to use is USB, please add the argument Force.Scheme=\"UBS\" to the function call. Thank you.")
      }}

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Returns
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    if(Checks.OK=="Yes"){return(Plot.1)}

  } # Close inputs check ok conditional
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
