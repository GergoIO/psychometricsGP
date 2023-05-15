#' Carries out the Hofstee standard setting method and displays the method and the result on a graph.
#'
#' @description fnGPHofstee plots a cumulative relative frequency curve (ogive) of assessment scores and adds a graphical display of the Hofstee standard setting method.
#' The output is a ggplot object, which can be saved or customised as needed, together with the coordinates of the Hofstee point.
#' See Examples for more details.
#'
#' @usage fnGPHofstee(x, pars=c(62.5,75,0,20), cutDigits=2)
#'
#' @param x A vector of percentage scores.
#' @param pars A vector of length 4 containing the Hofstee parameters in this order:
#' "Minimum cut score", "Maximum cut score", "Minimum failure rate", "Maximum failure rate".
#' @param cutDigits Number of decimal places to which the Hofstee cut score is displayed on the plot.
#' Defaults to 2 but in situations where the cut score is to be used in further calculations (e.g. AD(T)K tests) change this to 4.
#'
#' @note
#' This function uses \code{\link[ggplot2]{ggplot2}} for the underlying graphics and \code{\link[spatstat]{spatstat}} to calculate the Hofstee intersection point.
#'
#' @import ggplot2 spatstat
#'
#' @return The function exports either 3 or 4 objects into the Global Environment.
#'  \enumerate{
#'   \item HofsteePlot: a ggplot object displaying the method and the solution point.
#'   \item HofsteePoint: a length 2 vector containing the x,y coordinates of the Hofstee solution point.
#'   \item HofsteeFail: a logical TRUE/FALSE indicating whether the method failed with the supplied parameters.
#'   \item HofsteeFailText: A character string describing how the parameters have been changed to effect a solution (only created when HofsteeFail=TRUE).
#' }
#'
#' @examples
#' fnGPHofstee(x=rnorm(90,75,7))
#' fnGPHofstee(x=rnorm(90,60,10),pars=c(45,65,0,20),cutDigits=4)
#' fnGPHofstee(rnorm(90,50,10),c(45,60,0,10))
#' fnGPHofstee(rnorm(90,80,5),c(50,60,5,20))
#'
#' @source Written by Martin Roberts (psychometrics@plymouth.ac.uk)
#'
#' @export

fnGPHofstee<-function(x,pars=c(62.5,75,0,20),cutDigits=2){
  # Written by: Martin Roberts
  # Last updated: 12/02/2019
  ################################################
  x<-na.omit(x)	# Remove NA values
  nonPC<-sum(x>100|x<0)	# Identify (& remove) non-percentage scores
  if(nonPC>0){
    x<-x[x>=0 & x<=100]
    stop(paste0("Your percentage score vector contains ",fnNumToWord(nonPC)," value",
                if(nonPC>1){"s"}," outside the range 0 to 100. Please check your data."))
  }
  if(length(pars)!=4) stop("The argument 'pars' must be a vector of length 4")
  minCut<-pars[1]
  maxCut<-pars[2]
  minFail<-pars[3]
  maxFail<-pars[4]
  if(minCut>maxCut) stop("The minimum cut score must be less than the maximum cut score")
  if(minFail>maxFail) stop("The minimum failure rate must be less than the maximum failure rate")
  assign("HofsteeFail",FALSE,.GlobalEnv)
  # Create 100Ã—3 cumulative frequency table for the Hofstee calculations and graph
  ogiveData<-data.frame(Score=0:100,Frequency=as.vector(table(cut(x,breaks=-1:100,include.lowest=TRUE))))
  ogiveData$CumFreqPercent<-cumsum(ogiveData$Frequency)/max(cumsum(ogiveData$Frequency))*100
  s<-seq(length(ogiveData$Score)-1)
  a<-psp(ogiveData$Score[s], ogiveData$CumFreqPercent[s], ogiveData$Score[s+1], ogiveData$CumFreqPercent[s+1], window=owin(xrange=c(0,100), yrange=c(0,100))) #Ogive
  b<- psp(minCut,maxFail,maxCut,minFail, window=owin(xrange=c(0,100), yrange=c(0,100)))	#Hofstee diagonal
  intersection<-data.frame(crossing.psp(a,b))#find out where the two lines cross and convert crossing point to data frame
  assign("HofsteePoint",c(x=intersection[1,1],y=intersection[1,2]),.GlobalEnv)
  if(is.na(HofsteePoint[1])){
    assign("HofsteeFail",TRUE,.GlobalEnv)
    failRate<-sum(x<minCut)/length(x)*100
    if(failRate>maxFail) {
      maxFailNew<-fnRound(ceiling(failRate*100)/100,2)
      assign("HofsteeFailText",paste0("Using the maximum failure rate of ",maxFail,
                                      "% the method was unsuccessful so the Hofstee cut score was set to the minimum value of ",
                                      minCut,"."),.GlobalEnv)
      assign("HofsteePoint",c(x=minCut,y=maxFailNew),.GlobalEnv)
    } else {
      failRate<-sum(x<maxCut)/length(x)*100
      minFailNew<-fnRound(floor(failRate*100)/100,2)
      assign("HofsteeFailText",paste0("Using the minimum failure rate of ",minFail,
                                      "% the method was unsuccessful so the Hofstee cut score was set to the maximum value of ",
                                      maxCut,"."),.GlobalEnv)
      assign("HofsteePoint",c(x=maxCut,y=minFailNew),.GlobalEnv)
    }
  }
  if(HofsteeFail){
    cat("\nNOTE 1: The method failed. The following 'solution point' is saved as 'HofsteePoint' (vector of length 2).\n")
  }else{
    cat("\nNOTE 1: The intersection of the cumulative frequency curve with the Hofstee diagonal is saved as 'HofsteePoint' (vector of length 2).\n")
  }
  print(HofsteePoint)
  box<-data.frame(x=c(minCut,maxCut),y=c(minFail,maxFail))
  hofsteeText1 <- "Hofstee cut score = "
  hofsteeText2 <- formatC(fnRound(HofsteePoint[1],cutDigits), digits=cutDigits, format="f")
  redLineHeight<-ifelse(HofsteeFail,max(HofsteePoint[2]+10,maxFail*2),maxFail*2)
  plot<-ggplot() + theme_psmd() +
    theme(plot.margin=margin(0.3,0.5,0.3,0.5,"cm")) +
    scale_x_continuous(limits=c(0,100), breaks=seq(0,100,10), expand=c(0,0)) +
    scale_y_continuous(limits=c(-0.3,100.3), breaks=seq(0,100,10), expand=c(0,0)) +
    geom_rect(aes(xmin=minCut,xmax=maxCut,ymin=minFail,ymax=maxFail),size=0.5,colour="black",fill="grey90") + #Hofstee box
    geom_vline(xintercept=minCut,size=0.5,colour="black") + #Line from Hofstee box
    geom_vline(xintercept=maxCut,size=0.5,colour="black") + #Line from Hofstee box
    geom_hline(yintercept=minFail,size=0.5,colour="black") + #Line from Hofstee box
    geom_hline(yintercept=maxFail,size=0.5,colour="black") + #Line from Hofstee box
    geom_segment(aes(x=minCut, y=maxFail, xend=maxCut, yend=minFail), size=1.2,colour="black") + #Hofstee diagonal
    geom_line(data=ogiveData, aes(x=Score, y=CumFreqPercent),size=1.2,colour="blue") +  #Ogive
    geom_point(aes(x=HofsteePoint[1], y=HofsteePoint[2]),size=2,colour="red") + #Hofstee point
    geom_segment(aes(x=HofsteePoint[1], y=min(100,redLineHeight), xend=HofsteePoint[1], yend=0), size=1.2,colour="red",linetype="dashed") + #Red line at cut score
    annotate(geom="text", x=HofsteePoint[1], y=min(98,redLineHeight+1), label=hofsteeText1, fontface=2, color="red", vjust="bottom", hjust="right") +
    annotate(geom="text", x=HofsteePoint[1], y=min(98,redLineHeight+1), label=hofsteeText2, fontface=2, color="red", vjust="bottom", hjust="left") +
    annotate(geom="text", x=2, y=minFail+1, label="Minimum failure rate", fontface=3, size=3, vjust="bottom", hjust="left") +
    annotate(geom="text", x=2, y=maxFail+1, label="Maximum failure rate", fontface=3, size=3, vjust="bottom", hjust="left") +
    annotate(geom="text", x=minCut-1, y=98, label="Minimum cut score", fontface=3, size=3, angle=90, vjust="bottom", hjust="right") +
    annotate(geom="text", x=maxCut-1, y=98, label="Maximum cut score", fontface=3, size=3, angle=90, vjust="bottom", hjust="right") +
    xlab("Score (%)") + ylab("Cumulative Frequency (%)")
  assign("HofsteePlot",plot,.GlobalEnv)
  cat("\nNOTE 2: The graph showing the Hofstee method is saved as 'HofsteePlot' (ggplot object).\n")
  print(HofsteePlot)
  if(HofsteeFail){
    cat("\nWARNING: The method failed with the original parameters.\n")
    cat("         The variable 'HofsteeFail' has been set to TRUE and the following sentence has been saved as 'HofsteeFailText'.\n")
    cat(HofsteeFailText)
    cat("\n")
  }
}
