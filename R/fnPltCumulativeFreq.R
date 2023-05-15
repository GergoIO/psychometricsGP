#' Produces a cumulative frequency plot of facilities by Stage.
#'
#' @description This function takes a data frame with a column of facilities for each stage group,
#' and a row for each included item, and outputs a ggplot2 object which can then be customised. The stage
#' number only needs to be added where the plot is just for one stage. NEEDS ggplot2, plyr, stats,
#' stringr and tidyr packages.
#'
#' @usage fnPltCumulativeFreq(df,stage=NULL)
#'
#' @param df The name of the data frame containing a facility column for each stage and
#' a row for each included item). The data frame should NOT include the removed items.
#' @param stage This needs to be entered if there is only one stage (default=NULL).
#'
#' @examples
#' df1<-data.frame(Item=1:100,Y1fac=runif(100),Y2fac=runif(100),Y3fac=runif(100),
#' Y4fac=runif(100),Y5fac=runif(100))
#' df2<-data.frame(Item=1:100,Y1facility=runif(100))
#'
#' fnPltCumulativeFreq(df1)
#' fnPltCumulativeFreq(df2)
#' fnPltCumulativeFreq(df2,stage=1)
#'
#' @author Jo Cockerill, \email{jo.cockerill@plymouth.ac.uk}
#'
#' @export

##############################################################

fnPltCumulativeFreq<-function(df,stage=NULL){

  if(any(grepl("Fac",colnames(df)))){dataFac<-df[,c(grep("Fac",colnames(df)))]} else {dataFac<-df[,c(grep("fac",colnames(df)))]}

  if(is.null(ncol(dataFac)) & is.null(stage)){stop("You need to provide the stage as an integer")}

  if(is.null(ncol(dataFac)) & !is.null(stage)){dataFac2<-data.frame(value=dataFac,Stage=stage)} else
  {dataFac2<-gather(dataFac)
  regexp<-"[[:digit:]]+"
  dataFac2$Stage<-as.numeric(str_extract(dataFac2$key,regexp))
  dataFac2<-dataFac2[,-1]}

  if(max(dataFac2$value)>1){dataFac2$value<-dataFac2$value/100}
  nStages<-length(unique(dataFac2$Stage))
  Stages<-unique(dataFac2$Stage)

  dataCumFac<-ddply(dataFac2,.(Stage), transform, Cum=ecdf(value)(value))

  rowsToAdd<-data.frame(matrix(nrow=2*nStages,ncol=ncol(dataCumFac)))
  rowsToAdd[,c(1,3)]<-c(rep(0,nStages),rep(1,nStages))
  rowsToAdd[,c(2)]<-c(rep(Stages,2))

  colnames(rowsToAdd)<-colnames(dataCumFac)
  dataCumFac<-rbind(dataCumFac,rowsToAdd)
  dataCumFac$Stage<-as.character(dataCumFac$Stage)
  dataCumFac$Cum<-dataCumFac$Cum*100

  plot1<-ggplot(dataCumFac, aes(x=value, y=Cum)) + geom_line(aes(group=Stage, colour=Stage), size=1) +
    scale_colour_manual(values = fnGPColours("Stage")) + themeGP() +
    xlab("Item Facility") + scale_x_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1)) +
    ylab("Cumulative Frequency (% of items)") + themeGP() + scale_y_continuous(breaks=c(0,20,40,60,80,100))
  plot1
}
