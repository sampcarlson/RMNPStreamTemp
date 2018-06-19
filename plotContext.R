plotContext=function(startDate,endDate){
  startDate=as.Date("2014-08-23")
  endDate=as.Date("2014-09-25")
  #startDate=startDate-86400
  #endDate=endDate+86400
  windows()
  xlim=as.Date(c(startDate,endDate))
  #plot flow, airtemp, precip
  precip=read.csv("NiwotRidgePrecip.csv")
  precip$date=as.Date(precip$date,format="%m/%d/%Y")
  precip=precip[precip$date>=as.Date(startDate) & precip$date<=as.Date(endDate),]
  
  temp=read.csv("NiwotRidgeAirTemp.csv")
  temp$date=as.Date(temp$date,format="%m/%d/%Y")
  temp=temp[temp$date>=as.Date(startDate) & temp$date<=as.Date(endDate),]
  
  flow=read.csv("bigThomMoraineParkFlow.csv")
  flow$DateTime=as.POSIXct(flow$dateTimeString)
  flow=flow[flow$DateTime>=as.POSIXct(startDate) & flow$DateTime<=as.POSIXct(endDate),]
  
  par(oma=c(0,3,0,1))
  plot(precip$date,precip$ppt_tot,xlim=xlim,ylim=c(max(precip$ppt_tot,na.rm=T)+10,0.8),type="h",lwd=6,col="lightBlue",xlab="date",ylab="precip")
  #abline(v=startDate+86400)
  #abline(v=endDate-86400)
  par(new=T)
  plot(temp$date,temp$airtemp_avg,axes=F,xlab="",ylab="",xlim=xlim,ylim=c(min(temp$airtemp_min,na.rm=T),max(temp$airtemp_max,na.rm=T)+4),type="l")
  axis(side=4)
  lines(temp$date,temp$airtemp_max,lty=2)
  lines(temp$date,temp$airtemp_min,lty=3)
  par(new=T)
  plot(flow$DateTime,flow$Flow,type="l",axes=F,xlab="",ylab="",col="blue",xlim=xlim,ylim=c(min(flow$Flow),1.5*max(flow$Flow)))
  axis(side=2,outer=T)
}