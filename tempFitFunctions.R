#tempFitFunctions

buildTempData=function(){
  require(tidyverse)
  require(reshape2)
  require(RSQLite)
  
  TempDB=dbConnect(SQLite(),"C:\\Users\\Sam\\Documents\\LeakyRivers\\Data\\temperature\\leakyTemp.db3")
  
  
  #deps_nsv_gc=left_join(dbGetQuery(TempDB,"SELECT DeploymentIDX, SiteIDX FROM Deployments"),
  #                      dbGetQuery(TempDB,"SELECT SiteIDX, SiteName, Description FROM Sites"))
  #deps_nsv_gc=deps_nsv_gc[deps_nsv_gc$SiteIDX<=19,] #limit to NSV and GC sites (exclude SFP)
  #
  #tempData=dbGetQuery(TempDB,"SELECT Observation, DateTimeLocal, DeploymentIDX FROM Data WHERE Status = 1")
  #
  #tempData=inner_join(tempData,deps_nsv_gc)
  
  tempData=dbGetQuery(TempDB,"SELECT Data.Observation, Data.DateTimeLocal, Data.DeploymentIDX, Deployments.SiteIDX FROM Data LEFT JOIN Deployments ON Data.DeploymentIDX = Deployments.DeploymentIDX WHERE Data.Status = 1 AND Deployments.SiteIDX <= 19")
  dbDisconnect(TempDB)
  
  lakeData=read.csv("C:\\Users\\Sam\\Desktop\\spatial\\QgisEnvironment\\Active\\sitesLakeInfluence_tempPaper2\\min_10000\\lakeFractionAndElevation.csv",header = F)
  names(lakeData)=c("SiteIDX","LF","FlowWtMeanLakeElev")
  tempData=left_join(tempData,lakeData)
  
  siteData=read.csv("C:\\Users\\Sam\\Desktop\\spatial\\sites\\sites_elevation_uaa_corrected.csv",header = T)[c("SiteIDX","Elevation","uaa")]
  tempData=left_join(tempData,siteData)
  
  aggMeanFun=function(x){
    if (is.numeric(x)){
      return(mean(x,na.rm=T))
    } else {
      if(length(unique(x)==1)){
        return(x[1])
      } else { return("Multiple Records")} 
    }
  }
  
  
  tempData=aggregate(tempData[c("Observation","DateTimeLocal","DeploymentIDX","SiteIDX","LF","FlowWtMeanLakeElev","Elevation","uaa")],by=list(site=tempData$SiteIDX,day=tempData$DateTimeLocal),FUN=aggMeanFun)
  
  tempData$DateTimeLocal=as.Date(tempData$DateTimeLocal)
  
  return(tempData)
}

fitElevUaa=function(tempData){
  
  fitGetR_elev=function(date){
    date=as.Date(date)
    tempDataDay=tempData[tempData$DateTimeLocal==date,]
    r=summary(lm(tempDataDay$Observation~tempDataDay$Elevation))$r.squared
    return(r)
  }
  fitGetR_uaa=function(date){
    date=as.Date(date)
    tempDataDay=tempData[tempData$DateTimeLocal==date,]
    r=summary(lm(tempDataDay$Observation~tempDataDay$uaa))$r.squared
    return(r)
  }
  getN=function(date){
    date=as.Date(date)
    tempDataDay=tempData[tempData$DateTimeLocal==date,]
    n=length(unique(tempDataDay$SiteIDX))
    return(n)
  }
  
  fitDF=data.frame(day=unique(tempData$DateTimeLocal))
  fitDF$elev_r2=sapply(fitDF$day,FUN=fitGetR_elev)
  fitDF$uaa_r2=sapply(fitDF$day,FUN=fitGetR_uaa)
  fitDF$N=sapply(fitDF$day,FUN=getN)
  
  return(fitDF)
  
}

filterToSet=function(set,thisTempData=tempData){
  days=unique(thisTempData$day)
  
  isDayInSet=function(d,set,thisTempData){
    tempDayData=thisTempData[thisTempData$day==d,]
    daySites=unique(tempDayData$SiteIDX)
    return(all(unlist(set)%in%daySites))
  }
  
  inSet=sapply(days,isDayInSet,set=set,thisTempData=thisTempData)
  
  thisTempData=thisTempData[thisTempData$SiteIDX%in%unlist(set),]
  return(thisTempData[thisTempData$day%in%days[inSet],])
}

getSetByDate=function(d){
  d=as.Date(d)
  thisSet=unique(tempData$site[tempData$day==d])
  print(thisSet)
  thisSet=filterToSet(thisSet)
  return(thisSet)
}

getAllSets=function(tempData){
  
  countSetOccurance=function(set,thisTempData){
    days=unique(thisTempData$day)
    
    isDayInSet=function(d,set,thisTempData){
      tempDayData=thisTempData[thisTempData$day==d,]
      daySites=unique(tempDayData$SiteIDX)
      return(all(unlist(set)%in%daySites))
    }
    
    inSet=sapply(days,isDayInSet,set=set,thisTempData=thisTempData)
    return(sum(inSet))
  }
  
  getDaySet=function(d){
    tempDayData=tempData[tempData$day==d,]
    daySites=unique(tempDayData$SiteIDX)
    return(daySites)
  }
  
  days=unique(tempData$day)
  allSets=lapply(days,FUN=getDaySet)
  
  df=data.frame(day=days,
                setOccuranceCount=sapply(allSets,FUN=countSetOccurance,thisTempData=tempData),
                setLength=sapply(allSets,FUN=length))
  
  return(df)
  
  
}

fitStreamTemp=function(tempData, plotContext=F){
  
  
  #get set of sites which are consistent across whole period
  n_all=length(unique(tempData$SiteIDX))
  allSites=unique(tempData$SiteIDX)
  numDays=length(unique(tempData$day))
  daysThisSite=function(Site){
    dayCount=length(unique(tempData$day[tempData$SiteIDX==Site]))
    return(dayCount)
  }
  fullSites=allSites[sapply(allSites,daysThisSite)==numDays]
  tempData=tempData[tempData$SiteIDX%in%fullSites,]
  n=length(unique(tempData$SiteIDX))
  
  if(n == n_all){
    print(paste("n =",n))
  } else {
    print(paste0("n = ",n,", ",n_all-n," incomplete observations removed"))
  }
  #plot
  if(plotContext){
    makeTsPlot(min(tempData$day),max(tempData$day))
  }
  
  
  aggMeanFun=function(x){
    if (is.numeric(x)){
      return(mean(x,na.rm=T))
    } else {
      if(length(unique(x)==1)){
        return(x[1])
      } else { return("Multiple Records")} 
    }
  }
  
  tempData=aggregate(tempData,by=list(s=tempData$site),FUN=aggMeanFun)
  
  require(MuMIn)
  km2_per_pixel=(9.14308^2)/(1000^2)
  tempData$uaa=tempData$uaa*km2_per_pixel
  
  #make the 'global'model huge (and redundant) for aic comparison:
  linearCombined=glm(Observation~I(FlowWtMeanLakeElev*LF)+LF+I(Elevation*(1-LF))+I(uaa*(1-LF))+Elevation+uaa+FlowWtMeanLakeElev,data=tempData,na.action=na.fail)
  summary(linearCombined)
  
  
  d=dredge(linearCombined,m.lim=c(1,6),
           subset=(!("Elevation" && "I(Elevation * (1 - LF))")
                   && !("FlowWtMeanLakeElev" && "I(FlowWtMeanLakeElev * LF)")),
           extra='R^2')
  
  d$`R^2`=round(d$`R^2`,7)
  d$internalModelID=row.names(d)
  
  #8 hypothesized equations: one term: (streamElev, streamuaa, lakeElev, Fuaa_L), 
  #multi term: (streamElev+streamuaa, FuaaL(El)+FuaaS(Es), FuaaL(El)+FuaaS(Us), FuaaL(El)+FuaaS(Es+Us))
  
  r2_nls=function(nlsName){
    return(summary(lm(tempData$Observation~fitted(nlsName)))$r.squared)
  }
  
  modelNames=data.frame(name=c('elevFit','uaaFit','lakeElevFit','lakeFit','streamElevuaa','weightedEl_Es','weightedEl_Us','weightedEl_Es_Us','flat_El_Es','flat_El_Es_Us'
                               ,'flat_El_Us','flat_Lf_El','flat_Lf_El_Es','flat_Lf_El_Es_Us','flat_Lf_El_Us','flat_Lf_Es','flat_Lf_Es_Us','flat_Lf_Us'),internalModelID=0,stringsAsFactors = F)
  
  modelNames$internalModelID[modelNames$name=='elevFit']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='uaaFit']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ uaa + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='lakeElevFit']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ FlowWtMeanLakeElev + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='lakeFit']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ LF + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='streamElevuaa']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + uaa + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='weightedEl_Es']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ I(Elevation * (1 - LF)) + I(FlowWtMeanLakeElev * LF) + LF + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='weightedEl_Us']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ I(FlowWtMeanLakeElev * LF) + LF + I(uaa * (1 - LF)) + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='weightedEl_Es_Us']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ I(Elevation * (1 - LF)) + I(FlowWtMeanLakeElev * LF) + LF + I(uaa * (1 - LF)) + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_El_Es']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + FlowWtMeanLakeElev + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_El_Es_Us']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + FlowWtMeanLakeElev + uaa + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_El_Us']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ FlowWtMeanLakeElev + uaa + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_Lf_El']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ FlowWtMeanLakeElev + LF + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_Lf_El_Es']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + FlowWtMeanLakeElev + LF + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_Lf_El_Es_Us']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + FlowWtMeanLakeElev + LF + uaa + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_Lf_El_Us']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ FlowWtMeanLakeElev + LF + uaa + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_Lf_Es']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + LF + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_Lf_Es_Us']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + LF + uaa + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_Lf_Us']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ LF + uaa + 1, data = tempData, na.action = na.fail)"]
  
  d = d[d$internalModelID %in% modelNames$internalModelID]
  display=data.frame(internalModelID=d$internalModelID,r_2=d$`R^2`,AICc=d$AICc,deltaAICc=d$delta)
  display=merge(x=display,y=modelNames,by='internalModelID',all.x=TRUE,sort=FALSE)[,c('name','r_2','AICc','deltaAICc')]
  
  return(display)
}

makeTsPlot=function(startDate=as.Date("2014-08-01"),endDate=as.Date("2015-08-01")){
  require(RSQLite)
  require(dplyr)
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
  dayTemp=left_join(dayTemp,ag_result, by="date")
  
  ag_result=aggregate(allData,by=list(date=allData$DateTimeLocal),FUN=max)[c("date","Observation")]
  names(ag_result)[2]='max'
  dayTemp=left_join(dayTemp,ag_result, by="date")
  
  ag_result=aggregate(allData,by=list(date=allData$DateTimeLocal),FUN=aggNFun)[c("date","SiteIDX")]
  names(ag_result)[2]='n'
  dayTemp=left_join(dayTemp,ag_result, by="date")
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
  if(length(flow$Flow>=1)){
    par(new=T,oma=c(25,1,4,2))
    plot(flow$DateTime,flow$Flow,type="l",axes=F,xlab="",ylab="",
         col="blue",ylim=c(min(flow$Flow),max(flow$Flow)))
    axis(side=4)
    mtext(text="Streamflow (cfs)",side=4,line=2.2, col="blue", font=2)
  }
  
  #airTemp
  if(length(temp$airtemp_max>=1)){
    par(new=T,oma=c(15,1,11,2))
    plot(temp$date,temp$airtemp_avg,axes=F,xlab="",ylab="",xlim=xlim,
         ylim=c(min(temp$airtemp_min,na.rm=T),max(temp$airtemp_max,na.rm=T)+3),type="n")
    segments(x0=temp$date,y0=temp$airtemp_min,y1=temp$airtemp_max,lwd=5,col="grey")
    lines(temp$date,temp$airtemp_avg,lwd=2)
    axis(side=2,at=c(-20,-10,0,10,20))
    mtext(text="Air Temperature (C)",side=2,line=2.2,font=2)
  }
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
