require(RODBC)

#timeseries plot for paper
startDate=as.Date("2014-08-01")
endDate=as.Date("2015-08-01")
#startDate=startDate-86400
#endDate=endDate+86400
xlim=c(startDate,endDate)
#plot flow, airtemp, precip
precip=read.csv("NiwotRidgePrecip.csv",stringsAsFactors = F)
precip$date=as.Date(precip$date,format="%m/%d/%Y")
precip=precip[precip$date>=startDate & precip$date<=endDate,]
#prevent display where precip = 0
plotPrecip=precip
plotPrecip$ppt_tot[plotPrecip$ppt_tot==0]=NaN

temp=read.csv("NiwotRidgeAirTemp.csv",stringsAsFactors = F)
temp$date=as.Date(temp$date,format="%m/%d/%Y")
temp=temp[temp$date>=startDate & temp$date<=endDate,]

flow=read.csv("bigThomMoraineParkFlow.csv",stringsAsFactors = F)
flow$DateTime=as.POSIXct(flow$dateTimeString)
flow=flow[flow$DateTime>=startDate & flow$DateTime<=endDate,]

TempDB=odbcConnectAccess2007('C:/Users/Sam/Documents/LeakyRivers/Data/LeakyRiversTemperature_UPDATE.accdb')
dayTemp=data.frame(days=seq.Date(from=startDate,to=endDate,by='day'))
meanT=function(day){
  return(mean(sqlQuery(TempDB,paste0("SELECT AVG(Observation) FROM Data WHERE Status = 1 AND ABS(DATEDIFF('d',DateTimeLocal, #",day,"#))<1 GROUP BY DeploymentIDX"))[,1]))
}
minT=function(day){
  return(min(sqlQuery(TempDB,paste0("SELECT MIN(Observation) FROM Data WHERE Status = 1 AND ABS(DATEDIFF('d',DateTimeLocal, #",day,"#))<1 GROUP BY DeploymentIDX"))[,1]))
}
maxT=function(day){
  return(max(sqlQuery(TempDB,paste0("SELECT MAX(Observation) FROM Data WHERE Status = 1 AND ABS(DATEDIFF('d',DateTimeLocal, #",day,"#))<1 GROUP BY DeploymentIDX"))[,1]))
}
nT=function(day){
  return(length(unique(sqlQuery(TempDB,paste0("SELECT DeploymentIDX FROM Data WHERE Status = 1 AND ABS(DATEDIFF('d',DateTimeLocal, #",day,"#))<1")))$DeploymentIDX))
}
#this takes a long time
dayTemp$mean=sapply(dayTemp$days,meanT)
dayTemp$min=sapply(dayTemp$days,minT)
dayTemp$max=sapply(dayTemp$days,maxT)
dayTemp$n=sapply(dayTemp$days,nT)



#make plot
windows(width=7.5, height=10)
#precip
par(oma=c(29,1,0,2))
plot(plotPrecip$date,plotPrecip$ppt_tot,ylim=c(max(precip$ppt_tot,na.rm=T),1),
     type="h",lwd=3,lend=2,col="snow4",axes=F,xlab='',xlim=xlim,
     ylab='',main="Precipitation, Streamflow, Air Temperature, and Stream Temperature")
#graphics::box(bty="7")
axis(side=2)
mtext(text="Precipitation (mm)", side=2,line=2.2,col="snow4", font=2)

#streamflow
par(new=T,oma=c(25,1,4,2))
plot(flow$DateTime,flow$Flow,type="l",axes=F,xlab="",ylab="",
     col="blue",ylim=c(min(flow$Flow),max(flow$Flow)))
axis(side=4)
mtext(text="Streamflow (cfs)",side=4,line=2.2, col="blue", font=2)

#airTemp
par(new=T,oma=c(15,1,11,2))
plot(temp$date,temp$airtemp_avg,axes=F,xlab="",ylab="",xlim=xlim,
     ylim=c(min(temp$airtemp_min,na.rm=T),max(temp$airtemp_max,na.rm=T)+3),type="n")
segments(x0=temp$date,y0=temp$airtemp_min,y1=temp$airtemp_max,lwd=5,col="grey")
lines(temp$date,temp$airtemp_avg,lwd=2)
axis(side=2,at=c(-20,-10,0,10,20))
mtext(text="Air Temperature (C)",side=2,line=2.2,font=2)

#streamTemp
par(new=T,oma=c(8,1,19,2))
plot(dayTemp$days,dayTemp$mean,xlim=xlim,ylim=c(min(dayTemp$min,na.rm=T),max(dayTemp$max,na.rm=T)),
     axes=F,xlab='',ylab='',type="n",bty="n")
segments(x0=dayTemp$days,y0=dayTemp$min,y1=dayTemp$max,lwd=5,col="grey")
lines(dayTemp$days,dayTemp$mean,lwd=2)
axis(side=4)
mtext(text='Stream Temperature (C)',font=2,side=4,line=2.2)

#n
par(new=T,oma=c(2,1,29,2))
plot(dayTemp$days,dayTemp$n,xlim=xlim,axes=F,xlab='',ylab='',type='l',ylim=c(0,20))
abline(h=10,lty=3)
axis(side=2,at=c(0,10,20))
mtext(text="# of Observations",side=2, font=2, line=2.2)
axis.Date(side=1,at=seq.Date(from=startDate,to=endDate,by='month'), las=3, format="%e-%b-%Y")
