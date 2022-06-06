#' Produces a score boxplot by grade, stage, assessor, programme (for BDS & DTH only), station/substation
#' (ISCE), gender, ethnicity or disability. Two variables can be specified to produce split boxplots
#' within each group. 
#' 
#' @description This function takes a group column and a score column (and an optional fill 
#' column) and outputs a ggplot2 object (score boxplot) which can then be customised. The y-axis will default
#' to a value of 100 but this can be amended (use maxScore=). For Stage and Assessor
#' groups the data needs to be numeric/integer values; for all other groups the data needs to
#' be character strings (e.g. "Stn1" or "Sub2A" for ISCE stations/substations). The fillBy parameter does
#' not operate for station or substation data.
#'
#' @usage fnBoxplot(df, xName=NULL, yName=NULL, maxScore=100, forceScheme=NULL, fillBy=NULL)
#'       
#' @param df The name of the data frame.
#' @param xName The "group" column name (must be in quotation marks) or column number within the data frame.
#' @param yName The "score" column name (must be in quotation marks) or column number within the data frame.
#' @param maxScore Optional (default value is 100). The maximum possible score available.
#' @param forceScheme Optional. Use when the group scheme determined is "USE" but "UBSE" is required,
#' "PF" but "PFE" is required or "Station" but "Substation" is required (type forceScheme="Y"). 
#' @param fillBy The (optional) "fillBy" column name (must be in quotation marks) or column number within the
#' data frame when wanting to produce split boxplots (by a second variable).  
#' 
#' @examples
#' Basic usage: Examples use dataExample in psychometricsPSMD package. 
#' 
#' fnBoxplot(dataExample,"Grade.UBSE","Score.Perc")
#' 
#' fnBoxplot(dataExample,"Grade.UBSE","Score.Perc",fillBy="Gender")
#'
#' fnBoxplot(dataExample,"Ethnicity","Score.Restricted",25)
#'
#' fnBoxplot(dataExample,"Ethnicity","Score.Restricted",maxScore=25,fillBy="Gender")
#' 
#' fnBoxplot(dataExample,5,7,125)
#' 
#' fnBoxplot(dataExample,5,7,10,125)
#' 
#' fnBoxplot(dataExample,"Grade.PF","Score.Perc",forceScheme="Y")
#' 
#' fnBoxplot(dataExample,"Stage","Score.Restricted",25,fillBy="Grade.PF")
#' 
#' @export

########################################

fnBoxplot<-function(df, xName=NULL, yName=NULL, maxScore=100, forceScheme=NULL, fillBy=NULL){  
  
  if(is.null(fillBy)) {   
    
    xAxis<-df[[xName]]
    yAxis<-df[[yName]]
    dataPlot<-data.frame(xAxis,yAxis)
    dataPlot<-dataPlot[complete.cases(dataPlot),] }
  
  if(!is.null(fillBy)) {
    
    xAxis<-df[[xName]]
    yAxis<-df[[yName]]
    fill<-df[[fillBy]]
    dataPlot<-data.frame(xAxis,yAxis,fill)
    dataPlot<-dataPlot[complete.cases(dataPlot),] }
  
  # remove trailing spaces and convert to character strings
  dataPlot<-data.frame(lapply(dataPlot, trimws), stringsAsFactors=FALSE)
  dataPlot$yAxis<-as.numeric(dataPlot$yAxis)
  
  # Amend character strings for NI/Needs Improvement/Needs improvement grades (as substr,1,1 could be the same for two variables)   
  toChangeX1<-which(grepl("NI",dataPlot$xAxis)| grepl("Needs Improvement",dataPlot$xAxis) | grepl("Needs improvement",dataPlot$xAxis))
  if(!is.null(fillBy)){toChangeY1<-which(grepl("NI",dataPlot$fill)| grepl("Needs Improvement",dataPlot$fill) | grepl("Needs improvement",dataPlot$fill))}
  dataPlot$xAxis[toChangeX1]<-"I"
  if(!is.null(fillBy)){dataPlot$fill[toChangeY1]<-"I"}
  
  # Amend character strings for Programme codes (as substr,1,1 could be the same for two or more variables)   
  toChangeX2<-which(grepl("BDS",dataPlot$xAxis))
  toChangeX3<-which(grepl("BScDTH",dataPlot$xAxis) | grepl("DTH",dataPlot$xAxis))
  if(!is.null(fillBy)){toChangeY2<-which(grepl("BDS",dataPlot$fill))}
  if(!is.null(fillBy)){toChangeY3<-which(grepl("BScDTH",dataPlot$fill) | grepl("DTH",dataPlot$fill))}
  
  dataPlot$xAxis[toChangeX2]<-"D"
  dataPlot$xAxis[toChangeX3]<-"T"
  if(!is.null(fillBy)){dataPlot$fill [toChangeY2]<-"D"}
  if(!is.null(fillBy)){dataPlot$fill [toChangeY3]<-"T"}
  
  
  # Identify 'xScheme' from the data (column 1)
  dataPlot$code1<-substr(dataPlot[,1],1,1)
  variables1<-as.character(unique(dataPlot$code1)) # a list of the unique (non-numeric) variables values in this column          
  variablesCode1<-sort(toupper(variables1))
  
  # Loop to get levels as a string
  groupCode<-""
  for (i in 1:length(variablesCode1)){           # loop to bring it together as a groupCode
    text1<-variablesCode1[i]
    groupCode<-paste(groupCode,text1,sep="") }
  
  # Determine the xScheme from the character strings 
  xScheme<-NULL
  nCharX<-NULL
  if(groupCode=="BESU"|groupCode=="BES"|groupCode=="ES")      {xScheme<-"UBSE"}  
  if(groupCode=="ESU")                                        {xScheme<-"USE"}
  if(groupCode=="SU")                                         {xScheme<-"US"}
  if(groupCode=="BEHLU"|groupCode=="BEHL"|groupCode=="EHL"|groupCode=="EH")  {xScheme<-"UBLHE"} 
  if(groupCode=="EFP"|groupCode=="EP")                        {xScheme<-"PFE"}
  if(groupCode=="FP"|groupCode=="P")                          {xScheme<-"PF"}
  if(groupCode=="CIN"|groupCode=="CI"|groupCode=="CN"|groupCode=="N"|groupCode=="C") {xScheme<-"NIC"}
  if(groupCode=="DT"|groupCode=="D")                          {xScheme<-"Programme"}
  if(groupCode=="S" & substr(dataPlot$xAxis[1],2,2)=="t")     {xScheme<-"Station"}
  if(groupCode=="S" & substr(dataPlot$xAxis[1],2,2)=="u")     {xScheme<-"SubStn"}
  if(groupCode=="FM")                                         {xScheme<-"Gender"}
  if(groupCode=="AOW"|groupCode=="AW")                        {xScheme<-"Ethnicity"} 
  if(groupCode=="NOS"|groupCode=="NS"|groupCode=="N")         {xScheme<-"Disability"}
  if(groupCode=="1234"|groupCode=="123456"|groupCode=="1789"|groupCode=="123456789") {xScheme<-"Station"} 
  if(groupCode=="1234" & max(nchar(dataPlot$xAxis)>1)){xScheme<-"SubStn"}
  
  if(is.null(xScheme)){
    nCharX<-nchar(dataPlot$xAxis)
    nCharXMin<-min(nCharX)
    nCharXMax<-max(nCharX)
    if(nCharXMin==1 & nCharXMax==1) {xScheme<-"Stage"}
    if(nCharXMin==2 & nCharXMax==2) {xScheme<-"SubStn"}
    if(nCharXMax>2)                 {xScheme<-"Assessor"} }
  
  # Change the xScheme if forceScheme has been used   
  if((xScheme=="USE")     & (!is.null(forceScheme))) {xScheme<-"UBSE"}    # Change the xScheme from USE (determined) to UBSE (forceScheme) if the parameter is "Y"
  if((xScheme=="US")      & (!is.null(forceScheme))) {xScheme<-"USE"}     # Change the xScheme from US (determined) to USE (forceScheme) if the parameter is "Y"
  if((xScheme=="PF")      & (!is.null(forceScheme))) {xScheme<-"PFE"}     # Change the xScheme from PassFail (determined) to PassFailEx (forceScheme) if the parameter is "Y"
  if((xScheme=="Station") & (!is.null(forceScheme))) {xScheme<-"Stage"}   # Change the xScheme from Station (determined) to SubStn (forceScheme) if the parameter is "Y"
  if((xScheme=="Stage")   & (!is.null(forceScheme))) {xScheme<-"Station"} # Change the xScheme from Stage (determined) to Station (forceScheme) if the parameter is "Y"
  if((xScheme=="NIC")     & (!is.null(forceScheme))) {xScheme<-"NC"}      # Change the xScheme from NIC (determined) to NC (forceScheme) if the parameter is "Y"
  
  # Convert all abbreviated grades etc in code1 col to long form
  if(xScheme=="UBSE" | xScheme=="USE" | xScheme=="US" | xScheme=="UBLHE"){dataPlot$code1[dataPlot$code1=="U"] <-"Unsatisfactory"}
  if(xScheme=="UBSE" | xScheme=="UBLHE")                 {dataPlot$code1[dataPlot$code1=="B"] <-"Borderline"}
  if(xScheme=="UBSE" | xScheme=="USE" | xScheme=="US")                   {dataPlot$code1[dataPlot$code1=="S"] <-"Satisfactory"}
  if(xScheme=="UBSE" | xScheme=="USE" | xScheme=="UBLHE" | xScheme=="PFE"){dataPlot$code1[dataPlot$code1=="E"] <-"Excellent"}
  if(xScheme=="UBLHE")                                   {dataPlot$code1[dataPlot$code1=="L"]<-"Low Satisfactory"}
  if(xScheme=="UBLHE")                                   {dataPlot$code1[dataPlot$code1=="H"]<-"High Satisfactory"} 
  if(xScheme=="PFE" | xScheme=="PF"){dataPlot$code1[dataPlot$code1=="P"]<-"Pass"}
  if(xScheme=="PFE" | xScheme=="PF"){dataPlot$code1[dataPlot$code1=="F"]<-"Fail"}
  if(xScheme=="NIC" | xScheme=="NC"){dataPlot$code1[dataPlot$code1=="N"]<-"Not competent"}
  if(xScheme=="NIC"){dataPlot$code1[dataPlot$code1=="I"]<-"Needs improvement"}
  if(xScheme=="NIC" | xScheme=="NC"){dataPlot$code1[dataPlot$code1=="C"]<-"Competent"}
  if(xScheme=="Gender") {dataPlot$code1[dataPlot$code1=="F"] <-"Female"}
  if(xScheme=="Gender") {dataPlot$code1[dataPlot$code1=="M"] <-"Male"}
  if(xScheme=="Ethnicity") {dataPlot$code1[dataPlot$code1=="A"] <-"Asian"}
  if(xScheme=="Ethnicity") {dataPlot$code1[dataPlot$code1=="O"] <-"Other"}
  if(xScheme=="Ethnicity") {dataPlot$code1[dataPlot$code1=="W"] <-"White"}
  if(xScheme=="Disability") {dataPlot$code1[dataPlot$code1=="N"] <-"No known disability"}
  if(xScheme=="Disability") {dataPlot$code1[dataPlot$code1=="O"] <-"Other disability"}
  if(xScheme=="Disability") {dataPlot$code1[dataPlot$code1=="S"] <-"Specific learning difficulty"} 
  if(xScheme=="Programme") {dataPlot$code1[dataPlot$code1=="D"] <-"BDS"}
  if(xScheme=="Programme") {dataPlot$code1[dataPlot$code1=="T"] <-"BScDTH"}
  if(xScheme=="Station" | xScheme=="SubStn") {dataPlot$code1<-dataPlot$xAxis}
  if(xScheme=="Stage" | xScheme=="Assessor") {dataPlot$code1<-as.numeric(dataPlot$xAxis)}
  
  dataPlot$xAxis<-dataPlot$code1
  
  # Factor/order the xAxis data so appear in correct order in plot (and ordered by Median for Assessors)   
  if(xScheme=="UBSE"){dataPlot$xAxis<-factor(dataPlot$xAxis, levels=c("Unsatisfactory","Borderline","Satisfactory","Excellent"),ordered=TRUE )}
  if(xScheme=="USE"){dataPlot$xAxis<-factor(dataPlot$xAxis, levels=c("Unsatisfactory","Satisfactory","Excellent"),ordered=TRUE)}
  if(xScheme=="US"){dataPlot$xAxis<-factor(dataPlot$xAxis, levels=c("Unsatisfactory","Satisfactory"),ordered=TRUE)}
  if(xScheme=="UBLHE"){dataPlot$xAxis<-factor(dataPlot$xAxis, levels=c("Unsatisfactory","Borderline","Low Satisfactory","High Satisfactory","Excellent"),ordered=TRUE)}
  if(xScheme=="PFE"){dataPlot$xAxis<-factor(dataPlot$xAxis, levels=c("Fail","Pass","Excellent"),ordered=TRUE)}
  if(xScheme=="PF"){dataPlot$xAxis<-factor(dataPlot$xAxis, levels=c("Fail","Pass"),ordered=TRUE)}
  if(xScheme=="NC"){dataPlot$xAxis<-factor(dataPlot$xAxis, levels=c("Not competent","Competent"),ordered=TRUE)}
  if(xScheme=="NIC"){dataPlot$xAxis<-factor(dataPlot$xAxis, levels=c("Not competent","Needs improvement","Competent"),ordered=TRUE)}
  if(xScheme=="Stage"){dataPlot$xAxis<-as.factor(dataPlot$xAxis)}  
  if(xScheme=="Assessor"){dataPlot$xAxis<-as.factor(dataPlot$xAxis)}
  if(xScheme=="Gender"){dataPlot$xAxis<-factor(dataPlot$xAxis, levels=c("Female","Male"),ordered=TRUE)}
  if(xScheme=="Ethnicity"){dataPlot$xAxis<-factor(dataPlot$xAxis, levels=c("Asian","Other","White"),ordered=TRUE)}
  if(xScheme=="Disability"){dataPlot$xAxis<-factor(dataPlot$xAxis, levels=c("No known disability","Other disability","Specific learning difficulty"),ordered=TRUE)}
  if(xScheme=="Programme"){dataPlot$xAxis<-factor(dataPlot$xAxis, levels=c("BDS","BScDTH"),ordered=TRUE)}
  if(xScheme=="Station" &  substr(dataPlot$xAxis[1],1,1)=="S"){dataPlot$xAxis<-as.numeric(substr(dataPlot$xAxis,4,6))}
  if(xScheme=="Station" & !substr(dataPlot$xAxis[1],1,1)=="S"){dataPlot$xAxis<-as.numeric(dataPlot$xAxis)}
  if(xScheme=="SubStn"  &  substr(dataPlot$xAxis[1],1,1)=="S"){dataPlot$xAxis<-substr(dataPlot$xAxis,4,6)}
  if(xScheme=="SubStn"  & !substr(dataPlot$xAxis[1],1,1)=="S"){dataPlot$xAxis<-dataPlot$xAxis}
  
  if(xScheme=="Assessor") {
    
    oind<-order(as.numeric(by(dataPlot$yAxis,dataPlot$xAxis,median)))
    dataPlot$xAxis<-ordered(dataPlot$xAxis,levels=levels(dataPlot$xAxis)[oind])
    oind<-order(as.numeric(by(dataPlot$yAxis,dataPlot$xAxis,mean)))
    dataPlot$xAxis<-ordered(dataPlot$xAxis,levels=levels(dataPlot$xAxis)[oind])
    oind<-order(as.numeric(by(dataPlot$yAxis,dataPlot$xAxis,median)))
    dataPlot$xAxis<-ordered(dataPlot$xAxis,levels=levels(dataPlot$xAxis)[oind])
    
  }
  
  # Messages and Warnings re schemes
  if(xScheme=="Stage")       {print("The group scheme determined is Stage")}
  if(xScheme=="Assessor")    {print("The group scheme determined is Assessor")}
  if(xScheme=="Gender")      {print("The group scheme determined is Gender")}
  if(xScheme=="Ethnicity")   {print("The group scheme determined is Ethnicity")}
  if(xScheme=="Disability")  {print("The group scheme determined is Disability")}
  if(xScheme=="UBSE")        {print("The group scheme determined is UBSE grade")}
  if(xScheme=="Programme")   {print("The group scheme determined is Programme")}
  if(xScheme=="UBLHE")       {print("The group scheme determined is UBLHE grade")}
  if(xScheme=="PFE")         {print("The group scheme determined is PFE grade")}
  if(xScheme=="NC")          {print("The group scheme determined is NC grade")}
  if(xScheme=="SubStn")      {print("The group scheme determined is ISCE Substation")}
  if(xScheme=="Station" & groupCode=="123456" | groupCode=="1789" | groupCode=="123456789") {print("The group scheme determined is ISCE Station")} 
  if(xScheme=="Station" & groupCode=="S") {print("The group scheme determined is ISCE Station")} 
  if(xScheme=="Station" & groupCode=="1234") {cat(paste('The group scheme determined is ISCE Station; if Stage is required add the parameter forceScheme="',"Y",'".',sep=""))} 
  if(xScheme=="Stage")   {cat(paste('The group scheme determined is Stage; if Station is required add the parameter forceScheme="',"Y",'".',sep=""))} 
  if(xScheme=="USE")     {cat(paste('The group scheme determined is USE grade; if UBSE is required add the parameter forceScheme="',"Y",'".',sep=""))}
  if(xScheme=="US")      {cat(paste('The group scheme determined is US grade; if USE is required add the parameter forceScheme="',"Y",'".',sep=""))} 
  if(xScheme=="PF")      {cat(paste('The group scheme determined is PF grade; if PFE is required add the parameter forceScheme="',"Y",'".',sep=""))}
  if(xScheme=="NIC")     {cat(paste('The group scheme determined is NIC grade; if NC is required add the parameter forceScheme="',"Y",'".',sep=""))} 
  
  if(is.null(fillBy)) {   
    # Colours for each group (one variable)
    if(groupCode=="BESU"){colFill<-c("#D92120","#E68B33","#86BB6A","#3D52A1")}
    if(groupCode=="BES"){colFill<-c("#E68B33","#86BB6A","#3D52A1")}
    if(groupCode=="ES"){colFill<-c("#86BB6A","#3D52A1")}
    if(groupCode=="SU"){colFill<-c("#D92120","#86BB6A")}
    if(groupCode=="ESU"){colFill<-c("#D92120","#86BB6A","#3D52A1")}
    if(groupCode=="BEHLU"){colFill<-c("#D92120","#E68B33","#B1BE4E","#6DB388","#3D52A1")}
    if(groupCode=="BEHL"){colFill<-c("#E68B33","#B1BE4E","#6DB388","#3D52A1")}
    if(groupCode=="EHL"){colFill<-c("#B1BE4E","#6DB388","#3D52A1")}
    if(groupCode=="EH"){colFill<-c("#6DB388","#3D52A1")}
    if(groupCode=="EFP"){colFill<-c("#D92120","#86BB6A","#3D52A1")}
    if(groupCode=="FP"){colFill<-c("#D92120","#86BB6A")}
    if(groupCode=="P"){colFill<-c("#86BB6A")}
    if(groupCode=="EP"){colFill<-c("#86BB6A","#3D52A1")}
    if(groupCode=="CIN"){colFill<-c("#D92120","#E68B33","#86BB6A")}
    if(groupCode=="CN"){colFill<-c("#D92120","#86BB6A")}
    if(xScheme=="Stage"){colFill<-c("#AA4455","#AA7744","#AAAA44","#44AA77","#4477AA")[1:length(unique(dataPlot$xAxis))]}
    if(xScheme=="Assessor"){colFill<-c(rep("#88CCEE",length(unique(dataPlot$xAxis))))} 
    if(xScheme=="Gender"){colFill<-c("#800000","#88CCEE")[1:length(unique(dataPlot$xAxis))]}
    if(xScheme=="Ethnicity"){colFill<-c("#800000","#88CCEE","#AA7744")[1:length(unique(dataPlot$xAxis))]}
    if(xScheme=="Disability"){colFill<-c("#800000","#88CCEE","#AA7744")[1:length(unique(dataPlot$xAxis))]}
    if(xScheme=="Programme"){colFill<-c("#800000","#88CCEE")[1:length(unique(dataPlot$xAxis))]}
    if(xScheme=="Station"){colFill<-c("#AA4455","#AA7744","#AAAA44","#44AA77","#44AAAA","#4477AA","#AA4455","#AA7744","#AAAA44","#44AA77","#44AAAA","#4477AA")[1:length(unique(dataPlot$xAxis))]}
    if(xScheme=="SubStn"){colFill<-c("#AA4455","#AA4455","#AA4455","#AA7744","#AA7744","#AA7744","#AAAA44","#AAAA44","#AAAA44","#44AA77","#44AA77","#44AA77")[1:length(unique(dataPlot$xAxis))]}
  } 
  ###
  ###
  ###
  ###
  #####
  ####
  ###
  ##
  #
  if(!is.null(fillBy)) { 
    
    # Identify 'fillScheme' from the data (column 3)
    dataPlot$code2<-substr(dataPlot$fill,1,1)
    variables2<-as.character(unique(dataPlot$code2)) # a list of the unique (non-numeric) variables values in this column          
    variablesCode2<-sort(toupper(variables2))
    
    
    # Loop to get levels as a string
    fillCode<-""
    for (i in 1:length(variablesCode2)){           # loop to bring it together as a groupCode
      text2<-variablesCode2[i]
      fillCode<-paste(fillCode,text2,sep="") }
    
    
    # Determine the fillScheme from the character strings 
    fillScheme<-NULL
    nCharF<-NULL
    if(fillCode=="BESU"|fillCode=="BES"|fillCode=="ES")        {fillScheme<-"UBSE"}  
    if(fillCode=="ESU")                                        {fillScheme<-"USE"}
    if(fillCode=="SU")                                         {fillScheme<-"US"}
    if(fillCode=="BEHLU"|fillCode=="BEHL"| fillCode=="EHL"|fillCode=="EH")  {fillScheme<-"UBLHE"} 
    if(fillCode=="EFP"|fillCode=="EP")                        {fillScheme<-"PFE"}
    if(fillCode=="FP"|fillCode=="P")                          {fillScheme<-"PF"}
    if(fillCode=="CIN"|fillCode=="CI"|fillCode=="CN"|fillCode=="N"|fillCode=="C") {fillScheme<-"NIC"}
    if(fillCode=="DT"|fillCode=="D")                          {fillScheme<-"Programme"}
    if(fillCode=="S" & substr(dataPlot$fill[1],2,2)=="t")     {fillScheme<-"Station"}
    if(fillCode=="S" & substr(dataPlot$fill[1],2,2)=="u")     {fillScheme<-"SubStn"}
    if(fillCode=="FM")                                         {fillScheme<-"Gender"}
    if(fillCode=="AOW"|fillCode=="AW")                        {fillScheme<-"Ethnicity"} 
    if(fillCode=="NOS"|fillCode=="NS"|fillCode=="N")         {fillScheme<-"Disability"}
    if(fillCode=="1234"|fillCode=="123456"|fillCode=="1789"|fillCode=="123456789") {fillScheme<-"Station"} 
    if(fillCode=="1234" & max(nchar(dataPlot$fill)>1)){fillScheme<-"SubStn"}
    
    if(is.null(fillScheme)){
      nCharF<-nchar(dataPlot$fill)
      nCharFMin<-min(nCharF)
      nCharFMax<-max(nCharF)
      if(nCharFMin==1 & nCharFMax==1) {fillScheme<-"Stage"}
      if(nCharFMin==2 & nCharFMax==2) {fillScheme<-"SubStn"}
      if(nCharFMax>2)                 {fillScheme<-"Assessor"} }
    
    # Change the fillScheme if forceScheme has been used   
    if((fillScheme=="USE")     & (!is.null(forceScheme))) {fillScheme<-"UBSE"}    # Change the fillScheme from USE (determined) to UBSE (forceScheme) if the parameter is "Y"
    if((fillScheme=="US")      & (!is.null(forceScheme))) {fillScheme<-"USE"}     # Change the fillScheme from US (determined) to USE (forceScheme) if the parameter is "Y"
    if((fillScheme=="PF")      & (!is.null(forceScheme))) {fillScheme<-"PFE"}     # Change the fillScheme from PF (determined) to PFE (forceScheme) if the parameter is "Y"
    if((fillScheme=="Station") & (!is.null(forceScheme))) {fillScheme<-"Stage"}   # Change the fillScheme from Station (determined) to SubStn (forceScheme) if the parameter is "Y"
    if((fillScheme=="Stage")   & (!is.null(forceScheme))) {fillScheme<-"Station"} # Change the fillScheme from Stage (determined) to Station (forceScheme) if the parameter is "Y"
    if((fillScheme=="NIC")     & (!is.null(forceScheme))) {fillScheme<-"NC"}      # Change the fillScheme from NIC (determined) to NC (forceScheme) if the parameter is "Y"
    
    
    
    
    # Convert all abbreviated grades etc in code2 col to long form
    if(fillScheme=="UBSE" | fillScheme=="USE" | fillScheme=="UBLHE" | fillScheme=="US"){dataPlot$code2[dataPlot$code2=="U"] <-"Unsatisfactory"}
    if(fillScheme=="UBSE" | fillScheme=="UBLHE")                 {dataPlot$code2[dataPlot$code2=="B"] <-"Borderline"}
    if(fillScheme=="UBSE" | fillScheme=="USE" | fillScheme=="US")                   {dataPlot$code2[dataPlot$code2=="S"] <-"Satisfactory"}
    if(fillScheme=="UBSE" | fillScheme=="USE" | fillScheme=="UBLHE" | fillScheme=="PFE"){dataPlot$code2[dataPlot$code2=="E"] <-"Excellent"}
    if(fillScheme=="UBLHE")                                   {dataPlot$code2[dataPlot$code2=="L"]<-"Low Satisfactory"}
    if(fillScheme=="UBLHE")                                   {dataPlot$code2[dataPlot$code2=="H"]<-"High Satisfactory"} 
    if(fillScheme=="PFE" | fillScheme=="PF"){dataPlot$code2[dataPlot$code2=="P"]<-"Pass"}
    if(fillScheme=="PFE" | fillScheme=="PF"){dataPlot$code2[dataPlot$code2=="F"]<-"Fail"}
    if(fillScheme=="NIC" | fillScheme=="NC"){dataPlot$code2[dataPlot$code2=="N"]<-"Not competent"}
    if(fillScheme=="NIC"){dataPlot$code2[dataPlot$code2=="I"]<-"Needs improvement"}
    if(fillScheme=="NIC" | fillScheme=="NC"){dataPlot$code2[dataPlot$code2=="C"]<-"Competent"}
    if(fillScheme=="Gender") {dataPlot$code2[dataPlot$code2=="F"] <-"Female"}
    if(fillScheme=="Gender") {dataPlot$code2[dataPlot$code2=="M"] <-"Male"}
    if(fillScheme=="Ethnicity") {dataPlot$code2[dataPlot$code2=="A"] <-"Asian"}
    if(fillScheme=="Ethnicity") {dataPlot$code2[dataPlot$code2=="O"] <-"Other"}
    if(fillScheme=="Ethnicity") {dataPlot$code2[dataPlot$code2=="W"] <-"White"}
    if(fillScheme=="Disability") {dataPlot$code2[dataPlot$code2=="N"] <-"No known disability"}
    if(fillScheme=="Disability") {dataPlot$code2[dataPlot$code2=="O"] <-"Other disability"}
    if(fillScheme=="Disability") {dataPlot$code2[dataPlot$code2=="S"] <-"Specific learning difficulty"} 
    if(fillScheme=="Programme") {dataPlot$code2[dataPlot$code2=="D"] <-"BDS"}
    if(fillScheme=="Programme") {dataPlot$code2[dataPlot$code2=="T"] <-"BScDTH"}
    if(fillScheme=="Station" | fillScheme=="SubStn" | fillScheme=="Stage" | fillScheme=="Assessor") {dataPlot$code2<-dataPlot$fill}
    
    dataPlot$fill<-dataPlot$code2
    dataPlot<-dataPlot[,-4]
    
    
    # Factor/order the fill data so appear in correct order in plot (and ordered by Median for Assessors)   
    if(fillScheme=="UBSE"){dataPlot$fill<-factor(dataPlot$fill, levels=c("Unsatisfactory","Borderline","Satisfactory","Excellent"),ordered=TRUE )}
    if(fillScheme=="USE"){dataPlot$fill<-factor(dataPlot$fill, levels=c("Unsatisfactory","Satisfactory","Excellent"),ordered=TRUE)}
    if(fillScheme=="US"){dataPlot$fill<-factor(dataPlot$fill, levels=c("Unsatisfactory","Satisfactory"),ordered=TRUE)}
    if(fillScheme=="UBLHE"){dataPlot$fill<-factor(dataPlot$fill, levels=c("Unsatisfactory","Borderline","Low Satisfactory","High Satisfactory","Excellent"),ordered=TRUE)}
    if(fillScheme=="PFE"){dataPlot$fill<-factor(dataPlot$fill, levels=c("Fail","Pass","Excellent"),ordered=TRUE)}
    if(fillScheme=="PF"){dataPlot$fill<-factor(dataPlot$fill, levels=c("Fail","Pass"),ordered=TRUE)}
    if(fillScheme=="NC"){dataPlot$fill<-factor(dataPlot$fill, levels=c("Not competent","Competent"),ordered=TRUE)}
    if(fillScheme=="NIC"){dataPlot$fill<-factor(dataPlot$fill, levels=c("Not competent","Needs improvement","Competent"),ordered=TRUE)}
    if(fillScheme=="Stage"){dataPlot$fill<-factor(dataPlot$fill)}  
    if(fillScheme=="Assessor"){dataPlot$fill<-factor(dataPlot$fill)}
    if(fillScheme=="Gender"){dataPlot$fill<-factor(dataPlot$fill, levels=c("Female","Male"),ordered=TRUE)}
    if(fillScheme=="Ethnicity"){dataPlot$fill<-factor(dataPlot$fill, levels=c("Asian","Other","White"),ordered=TRUE)}
    if(fillScheme=="Disability"){dataPlot$fill<-factor(dataPlot$fill, levels=c("No known disability","Other disability","Specific learning difficulty"),ordered=TRUE)}
    if(fillScheme=="Programme"){dataPlot$fill<-factor(dataPlot$fill, levels=c("BDS","BScDTH"),ordered=TRUE)}
    #if(fillScheme=="Stage" | fillScheme=="Assessor"){dataPlot$fill<-as.numeric(dataPlot$fill)}
    if(fillScheme=="Station" &  substr(dataPlot$fill[1],1,1)=="S"){dataPlot$fill<-as.numeric(substr(dataPlot$fill,4,6))}
    if(fillScheme=="Station" & !substr(dataPlot$fill[1],1,1)=="S"){dataPlot$fill<-as.numeric(dataPlot$fill)}
    if(fillScheme=="SubStn"  &  substr(dataPlot$fill[1],1,1)=="S"){dataPlot$fill<-substr(dataPlot$fill,4,6)}
    if(fillScheme=="SubStn"  & !substr(dataPlot$fill[1],1,1)=="S"){dataPlot$fill<-dataPlot$fill}
    
    if(fillScheme=="Assessor") {
      oind<-order(as.numeric(by(dataPlot$yAxis,dataPlot$fill,median)))
      dataPlot$fill<-ordered(dataPlot$fill,levels=levels(dataPlot$fill)[oind])
      oind<-order(as.numeric(by(dataPlot$yAxis,dataPlot$fill,mean)))
      dataPlot$fill<-ordered(dataPlot$fill,levels=levels(dataPlot$fill)[oind])
      oind<-order(as.numeric(by(dataPlot$yAxis,dataPlot$fill,median)))
      dataPlot$fill<-ordered(dataPlot$fill,levels=levels(dataPlot$fill)[oind])
      
    }
    
    # Messages and Warnings re fillScheme
    if(fillScheme=="Stage")       {print("The fill scheme determined is Stage")}
    if(fillScheme=="Assessor")    {print("The fill scheme determined is Assessor")}
    if(fillScheme=="Gender")      {print("The fill scheme determined is Gender")}
    if(fillScheme=="Ethnicity")   {print("The fill scheme determined is Ethnicity")}
    if(fillScheme=="Disability")  {print("The fill scheme determined is Disability")}
    if(fillScheme=="UBSE")        {print("The fill scheme determined is UBSE grade")}
    if(fillScheme=="Programme")   {print("The fill scheme determined is Programme")}
    if(fillScheme=="UBLHE")       {print("The fill scheme determined is UBLHE grade")}
    if(fillScheme=="PFE")         {print("The fill scheme determined is PFE grade")}
    if(fillScheme=="NC")          {print("The fill scheme determined is NC grade")}
    if(fillScheme=="SubStn")      {print("The fill scheme determined is ISCE Substation")}
    if(fillScheme=="Station" & fillCode=="123456" | fillCode=="1789" | fillCode=="123456789") {print("The fill scheme determined is ISCE Station")} 
    if(fillScheme=="Station" & fillCode=="S") {print("The fill scheme determined is ISCE Station")} 
    if(fillScheme=="Station" & fillCode=="1234") {cat(paste('The fill scheme determined is ISCE Station; if Stage is required add the parameter forceScheme="',"Y",'".',sep=""))} 
    if(fillScheme=="Stage")   {cat(paste('The fill scheme determined is Stage; if Station is required add the parameter forceScheme="',"Y",'".',sep=""))} 
    if(fillScheme=="USE")     {cat(paste('The fill scheme determined is USE grade; if UBSE is required add the parameter forceScheme="',"Y",'".',sep=""))} 
    if(fillScheme=="US")      {cat(paste('The fill scheme determined is US grade; if USE is required add the parameter forceScheme="',"Y",'".',sep=""))} 
    if(fillScheme=="PF")      {cat(paste('The fill scheme determined is PF grade; if PFE is required add the parameter forceScheme="',"Y",'".',sep=""))}
    if(fillScheme=="NIC")     {cat(paste('The fill scheme determined is NIC grade; if NC is required add the parameter forceScheme="',"Y",'".',sep=""))} 
    
    
    # Colours for each group (one variable)
    if(fillCode=="BESU"){colFill<-c("#D92120","#E68B33","#86BB6A","#3D52A1")}
    if(fillCode=="BES"){colFill<-c("#E68B33","#86BB6A","#3D52A1")}
    if(fillCode=="ES"){colFill<-c("#86BB6A","#3D52A1")}
    if(fillCode=="ESU"){colFill<-c("#D92120","#86BB6A","#3D52A1")}
    if(fillCode=="SU"){colFill<-c("#D92120","#86BB6A")}
    if(fillCode=="BEHLU"){colFill<-c("#D92120","#E68B33","#B1BE4E","#6DB388","#3D52A1")}
    if(fillCode=="BEHL"){colFill<-c("#E68B33","#B1BE4E","#6DB388","#3D52A1")}
    if(fillCode=="EHL"){colFill<-c("#B1BE4E","#6DB388","#3D52A1")}
    if(fillCode=="EH"){colFill<-c("#6DB388","#3D52A1")}
    if(fillCode=="EFP"){colFill<-c("#D92120","#86BB6A","#3D52A1")}
    if(fillCode=="FP"){colFill<-c("#D92120","#86BB6A")}
    if(fillCode=="P"){colFill<-c("#86BB6A")}
    if(fillCode=="EP"){colFill<-c("#86BB6A","#3D52A1")}
    if(fillCode=="CIN"){colFill<-c("#D92120","#E68B33","#86BB6A")}
    if(fillCode=="CN"){colFill<-c("#D92120","#86BB6A")}
    if(fillScheme=="Stage"){colFill<-c("#AA4455","#AA7744","#AAAA44","#44AA77","#4477AA")[1:length(unique(dataPlot$xAxis))]}
    if(fillScheme=="Assessor"){colFill<-c(rep("#88CCEE",length(unique(dataPlot$fill))))} 
    if(fillScheme=="Gender"){colFill<-c("#800000","#88CCEE")[1:length(unique(dataPlot$fill))]}
    if(fillScheme=="Ethnicity"){colFill<-c("#800000","#88CCEE","#AA7744")[1:length(unique(dataPlot$fill))]}
    if(fillScheme=="Disability"){colFill<-c("#800000","#88CCEE","#AA7744")[1:length(unique(dataPlot$fill))]}
    if(fillScheme=="Programme"){colFill<-c("#800000","#88CCEE")[1:length(unique(dataPlot$fill))]}
    if(fillScheme=="Station"){colFill<-c("#AA4455","#AA7744","#AAAA44","#44AA77","#44AAAA","#4477AA","#AA4455","#AA7744","#AAAA44","#44AA77","#44AAAA","#4477AA")[1:length(unique(dataPlot$fill))]}
    if(fillScheme=="SubStn"){colFill<-c("#AA4455","#AA4455","#AA4455","#AA7744","#AA7744","#AA7744","#AAAA44","#AAAA44","#AAAA44","#44AA77","#44AA77","#44AA77")[1:length(unique(dataPlot$fill))]}
    
  }  
  
  #################### Prep to create plots       
  # to order the values for Assessor & Station schemes 
  if(xScheme=="Assessor" | xScheme=="Station" | xScheme=="Stage"){dataPlot$xAxis<-factor(dataPlot$xAxis)}
  
  # label for x-axis
  labelX<-NULL
  if(xScheme=="Assessor"){labelX<-"Assessor ID"}
  if(xScheme=="SubStn"){labelX<-"Substation"}
  if(xScheme=="Stage"|xScheme=="Gender"|xScheme=="Ethnicity"|xScheme=="Disability"|xScheme=="Station"|xScheme=="Programme"){labelX<-xScheme}
  if(is.null(labelX)){labelX<-"Grade"}
  
  #################### Create plots
  if(is.null(fillBy)) {
    ggplot(dataPlot, aes(x=xAxis, y=yAxis, group=xAxis)) +
      geom_boxplot(fill=colFill) + scale_x_discrete(drop=FALSE) +
      expand_limits(y=c(0,maxScore)) +
      ylab(ifelse(maxScore==100,"Score (%)","Score")) +
      xlab(labelX) +
      stat_summary(fun.y="mean", geom="point", shape=8, size=3.5, position=position_dodge(width=0.75), color="white") +
      theme_psmd() }
  
  else{
    
    if(fillScheme=="Assessor" | fillScheme=="Station" | fillScheme=="Stage"){dataPlot$fill<-factor(dataPlot$fill)}
    ggplot(dataPlot, aes(x=xAxis, y=yAxis, fill=fill)) +
      geom_boxplot() + scale_x_discrete(drop=FALSE) +
      scale_fill_manual(values=colFill, drop=FALSE) +
      expand_limits(y=c(0,maxScore)) +
      ylab(ifelse(maxScore==100,"Score (%)","Score")) +
      xlab(labelX) +
      stat_summary(fun.y="mean", geom="point", shape=8, size=3.5, position=position_dodge(width=0.75), color="white") +
      labs(fill=ifelse(fillScheme=="Assessor","Assessor ID",ifelse(fillScheme=="Stage",fillScheme,ifelse(fillScheme=="Gender",fillScheme,ifelse(fillScheme=="Ethnicity",fillScheme,ifelse(fillScheme=="Disability",fillScheme,"Grade")))))) +
      theme_psmd() + theme(legend.position=c(1,0), legend.justification=c(1,0))	}
  
} # THE END

