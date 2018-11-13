library(tidyverse)
source('~/R/projects/RMNPStreamTemp/tempFitFunctions.R')
tempData=buildTempData()

tempData_lake=tempData[tempData$SiteIDX!=19,]# drop glacier knobs (site w/ no LoI)

days=seq.Date(from=as.Date("2014-08-01"),to=as.Date("2015-07-31"),by="day")

#for debug
#days=days[50:52]

fitDF=data.frame(day=numeric(),elev_r2=numeric(),uaa_r2=numeric(),lf_r2=numeric(),el_r2=numeric(),N=numeric())


sampleFit8=function(day){
  sample8=function(sites,...){
    return(sample(sites,8))
  }
  
  fit8=function(set){
    setData=thisData[thisData$site %in% set,]
    return(fitSimpleModels(setData))
  }
  
  thisData=tempData_lake[tempData_lake$day==day,]
  sites=unique(thisData$site)
  
  if(length(sites)>=8){
    n=10 # number of sets of 8 to fit
    sets=lapply(1:n,sample8,sites=sites)
    return(t(sapply(sets,fit8)))
  }else(
    return(data.frame(day=numeric(),elev_r2=numeric(),uaa_r2=numeric(),lf_r2=numeric(),el_r2=numeric(),N=numeric()))
  )
}

for(d in days){
  fitDF=rbind(fitDF,sampleFit8(d))
}

fitDF$day=as.Date(unlist(fitDF$day),origin=as.Date("1970-01-01"))
fitDF$elev_r2=unlist(as.numeric(fitDF$elev_r2))
fitDF$el_r2=unlist(as.numeric(fitDF$el_r2))

getQuantile=function(metric,day,quantile,period){
  tempVect=fitDF[abs(fitDF$day-day)<period/2,metric]
  q=quantile(tempVect,quantile)
}

fitDF$dayNumeric=as.numeric(fitDF$day)
fitDF$med_stream=sapply(fitDF$day,getQuantile,metric="elev_r2",quantile=.5,period=5)
fitDF$med_lake=sapply(fitDF$day,getQuantile,metric="el_r2",quantile=.5,period=5)

#tail end of dataset has only 8 sampling points, causing no variability in this calculation of r^2.  shorten dataset:
fitDF=fitDF[fitDF$day<=as.Date("2015-05-30"),]


################---------------make r^2 plot----------------##################

png(filename="streamLake_r2.png",width=10,height=7.5, units="in", res=300)

par(mar=c(7,4,1,1))
plot(fitDF$day,fitDF$elev_r2,pch=1,cex=1.1,ylim=c(0,1.08),xlab="",ylab="",axes=F,col=rgb(0,0,0,0.3))
points(fitDF$day,fitDF$el_r2,pch=4,cex=1.1,col=rgb(0,0,0,0.3))


lines(fitDF$day,fitDF$med_stream,lwd=8,col="white")
lines(fitDF$day,fitDF$med_lake,lwd=8,col="white")

lines(fitDF$day,fitDF$med_stream,lwd=2.5,lty=1)
lines(fitDF$day,fitDF$med_lake,lwd=2.5,lty=3)


axis.Date(side=1,at=seq.Date(from=as.Date("2014-08-01"),to=as.Date("2015-07-01"),by="month"), las=3, format="%e-%b-%Y")
axis(side=2,at=c(0,0.2,0.4,0.6,0.8,1))

mtext(expression('Explanatory r'^'2'),side = 2,line=2)

legend(x="topright",legend=c("Stream Elevation fit (equation 4)","5-day Median Stream Elevation fit","Lake Elevation fit (equation 7)","5-day Median Lake Elevation fit"),
       pch=c(1,NA,4,NA),lty=c(NA,1,NA,3),lwd=c(1,2.5,1,2.5), 
       bty="n")
dev.off()



# #El fit vs flow
# flow=read.csv("bigThomMoraineParkFlow.csv",stringsAsFactors = F)
# flow$DateTime=as.POSIXct(flow$dateTimeString)
# 
# #liters per foot^3:
# flow$Flow_ls=flow$Flow*((12*2.54)^3/10^3)
# 
# aggMeanFun=function(x){
#   if (is.numeric(x)){
#     return(mean(x,na.rm=T))
#   } else {
#     if(length(unique(x)==1)){
#       return(x[1])
#     } else { return("Multiple Records")} 
#   }
# }
# 
# flowDay=aggregate(flow,by=list(day=flow$Date),FUN = aggMeanFun)
# flowDay$day=as.Date(flowDay$day)
# fitDF=left_join(fitDF,flowDay[c("day","Flow_ls")])
# 
# fitDF$month_numeric=as.numeric(format.Date(fitDF$day,format="%m"))
# 
# 
# windows()
# layout(mat=matrix(data=1:9,ncol=3,byrow=T))
# for(m in c(3,4,5,6,8,9,10,11,12)){
#   m_data=fitDF[fitDF$month_numeric==m,]
#   plot(m_data$Flow_ls,m_data$el_r2,main=paste("month = ",m))
#   
# }
