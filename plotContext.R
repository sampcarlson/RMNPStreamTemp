plotContext=function(startDate,endDate){
  #startDate=startDate-86400
  #endDate=endDate+86400
  xlim=c(startDate,endDate)
  #plot flow, airtemp, precip
  precip=read.csv("NiwotRidgePrecip.csv")
  precip$date=as.Date(precip$date,format="%m/%d/%Y")
  precip=precip[precip$date>=startDate & precip$date<=endDate,]
  
  temp=read.csv("NiwotRidgeAirTemp.csv")
  temp$date=as.Date(temp$date,format="%m/%d/%Y")
  temp=temp[temp$date>=startDate & temp$date<=endDate,]
  
  flow=read.csv("bigThomMoraineParkFlow.csv")
  flow$DateTime=as.POSIXct(flow$dateTimeString)
  flow=flow[flow$DateTime>=startDate & flow$DateTime<=endDate,]
  
  par(oma=c(0,3,0,1))
  plot(precip$date,precip$ppt_tot,ylim=c(max(precip$ppt_tot,na.rm=T)+10,0.8),type="h",lwd=6,col="lightBlue",xlab="date",ylab="precip")
  #abline(v=startDate+86400)
  #abline(v=endDate-86400)
  par(new=T)
  plot(temp$date,temp$airtemp_avg,axes=F,xlab="",ylab="",ylim=c(min(temp$airtemp_min,na.rm=T),max(temp$airtemp_max,na.rm=T)+4),type="l")
  axis(side=4)
  lines(temp$date,temp$airtemp_max,lty=2)
  lines(temp$date,temp$airtemp_min,lty=3)
  par(new=T)
  plot(flow$DateTime,flow$Flow,type="l",axes=F,xlab="",ylab="",col="blue",ylim=c(min(flow$Flow),1.5*max(flow$Flow)))
  axis(side=2,outer=T)
}