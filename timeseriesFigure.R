makeTsPlot=function(startDate=as.Date("2014-08-01"),endDate=as.Date("2015-08-01")){
  require(RSQLite)
  require(dplyr)
  tempLite=dbConnect(SQLite(),"C:\\Users\\Sam\\Documents\\LeakyRivers\\Data\\temperature\\leakyTemp.db3")
  startDate=as.Date(startDate)
  endDate=as.Date(endDate)

  xlim=c(startDate,endDate)
  #plot flow, airtemp, precip
  precip=read.csv("NiwotRidgePrecip.csv",stringsAsFactors = F)
  precip$date=as.Date(precip$date,format="%m/%d/%Y")
  precip=precip[precip$date>=startDate & precip$date<=endDate,]
  #prevent display where precip = 0
  plotPrecip=precip
  plotPrecip$ppt_tot[plotPrecip$ppt_tot==0]=NA
  
  temp=read.csv("NiwotRidgeAirTemp.csv",stringsAsFactors = F)
  temp$date=as.Date(temp$date,format="%m/%d/%Y")
  temp=temp[temp$date>=startDate & temp$date<=endDate,]
  
  flow=read.csv("bigThomMoraineParkFlow.csv",stringsAsFactors = F)
  flow$DateTime=as.POSIXct(flow$dateTimeString)
  flow=flow[as.Date(flow$DateTime)>=startDate & as.Date(flow$DateTime)<=endDate,]
  
  aggMeanFun=function(x){
    if (is.numeric(x)){
      return(mean(x,na.rm=T))
    } else {
      if(length(unique(x)==1)){
        return(x[1])
      } else { return("Multiple Records")} 
    }
  }
  
  aggNFun=function(x){
    u=unique(x)
    return(length(u))
  }
  
  TempDB=dbConnect(SQLite(),"C:\\Users\\Sam\\Documents\\LeakyRivers\\Data\\temperature\\leakyTemp.db3")
  allData=dbGetQuery(TempDB,"SELECT Data.Observation, Data.DateTimeLocal, Deployments.SiteIDX FROM Data LEFT JOIN Deployments ON Data.DeploymentIDX = Deployments.DeploymentIDX WHERE Data.Status = 1 AND Deployments.SiteIDX <= 19")
  dbDisconnect(TempDB)
  
  ag_result=aggregate(allData,by=list(date=allData$DateTimeLocal),FUN=min)
  dayTemp=data.frame(date=ag_result$date,min=ag_result$Observation, stringsAsFactors = F)
  
  ag_result=aggregate(allData,by=list(date=allData$DateTimeLocal),FUN=aggMeanFun)[c("date","Observation")]
  names(ag_result)[2]='mean'
  dayTemp=left_join(dayTemp,ag_result)
  
  ag_result=aggregate(allData,by=list(date=allData$DateTimeLocal),FUN=max)[c("date","Observation")]
  names(ag_result)[2]='max'
  dayTemp=left_join(dayTemp,ag_result)
  
  ag_result=aggregate(allData,by=list(date=allData$DateTimeLocal),FUN=aggNFun)[c("date","SiteIDX")]
  names(ag_result)[2]='n'
  dayTemp=left_join(dayTemp,ag_result)
  dayTemp=dayTemp[as.Date(dayTemp$date)>=startDate & as.Date(dayTemp$date)<=endDate,]
  dayTemp$date=as.Date(dayTemp$date)
  
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
  plot(dayTemp$date,dayTemp$mean,xlim=xlim,ylim=c(0,max(dayTemp$max,na.rm=T)),
       axes=F,xlab='',ylab='',type="n",bty="n")
  segments(x0=dayTemp$date,y0=dayTemp$min,y1=dayTemp$max,lwd=5,col="grey")
  lines(dayTemp$date,dayTemp$mean,lwd=2)
  axis(side=4)
  mtext(text='Stream Temperature (C)',font=2,side=4,line=2.2)
  
  #n
  par(new=T,oma=c(1,1,28,2))
  plot(dayTemp$date,dayTemp$n,xlim=xlim,axes=F,xlab='',ylab='',type='l',ylim=c(0,20))
  abline(h=10,lty=3)
  #temp elev r2*10 line.  relies on muchData from ElevAcrossTime.R
  #lines(muchData$day,muchData$elev_r2*10,lty=1,lwd=2)
  axis(side=2,at=c(0,5,10,15))
  mtext(text="# of Sites",side=2, font=2, line=2.2)
  axis.Date(side=1,at=seq.Date(from=startDate,to=endDate,length.out=12), las=3, format="%e-%b-%Y")
  
  #fitted Times
  #abline(v=as.Date("2015-01-01"))
}
