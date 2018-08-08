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
  
  #siteData=read.csv("C:\\Users\\Sam\\Desktop\\spatial\\sites\\sites_elevation_uaa_corrected.csv",header = T)[c("SiteIDX","Elevation","uaa")]  
  
  siteData=read.csv("C:\\Users\\Sam\\Desktop\\spatial\\QgisEnvironment\\Active\\sitesLakeInfluence_tempPaper2\\Inputs\\NSV_GC_Loggers.csv",header = T)[c("SiteIDX","Elevation","UAA","Descriptio")]
  names(siteData)[names(siteData)=="UAA"]="uaa"
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
  
  tempData$day=as.Date(tempData$DateTimeLocal)
  tempData=aggregate(tempData[c("Observation","day","DeploymentIDX","SiteIDX","LF","FlowWtMeanLakeElev","Elevation","uaa","Descriptio")],by=list(site=tempData$SiteIDX,day=tempData$day),FUN=aggMeanFun)
  km2_per_pixel=(9.14308^2)/(1000^2)
  tempData$uaa=tempData$uaa*km2_per_pixel
  #leave as 0, as 0*0 is allowable in weighted models
  #tempData$FlowWtMeanLakeElev[tempData$FlowWtMeanLakeElev==0]=NA_real_
  
  return(tempData)
}

fitSimpleModels=function(tempData, plot=T){
  
  fitGetR_elev=function(date){
    date=as.Date(date)
    tempDataDay=tempData[tempData$day==date,]
    r=summary(lm(tempDataDay$Observation~tempDataDay$Elevation))$r.squared
    return(r)
  }
  fitGetR_uaa=function(date){
    date=as.Date(date)
    tempDataDay=tempData[tempData$day==date,]
    r=summary(lm(tempDataDay$Observation~tempDataDay$uaa))$r.squared
    return(r)
  }
  fitGetR_lf=function(date){
    date=as.Date(date)
    tempDataDay=tempData[tempData$day==date,]
    r=summary(lm(tempDataDay$Observation~tempDataDay$LF))$r.squared
    return(r)
  }
  fitGetR_el=function(date){
    date=as.Date(date)
    tempDataDay=tempData[tempData$day==date,]
    r=summary(lm(tempDataDay$Observation~tempDataDay$FlowWtMeanLakeElev))$r.squared
    return(r)
  }
  getN=function(date){
    date=as.Date(date)
    tempDataDay=tempData[tempData$day==date,]
    n=length(unique(tempDataDay$SiteIDX))
    return(n)
  }
  
  fitDF=data.frame(day=unique(tempData$day))
  fitDF$elev_r2=sapply(fitDF$day,FUN=fitGetR_elev)
  fitDF$uaa_r2=sapply(fitDF$day,FUN=fitGetR_uaa)
  fitDF$lf_r2=sapply(fitDF$day,FUN=fitGetR_lf)
  fitDF$el_r2=sapply(fitDF$day,FUN=fitGetR_el)
  fitDF$N=sapply(fitDF$day,FUN=getN)
  if(plot==T){
    plotSimpleModels(fitDF)
  }
  return(fitDF)
  
}

plotSimpleModels=function(fitDF){
  
  allDays=as.Date(min(fitDF$day):max(fitDF$day),origin="1970-01-01")
  
  plot(allDays,rep(1,length(allDays)),ylim=c(0,1),type="n",xlab="date",ylab="r^2")
  
  lines(fitDF$day,fitDF$elev_r2,col="blue",lwd=2)
  lines(fitDF$day,fitDF$uaa_r2,col="blue",lwd=2,lty=3)
  
  lines(fitDF$day,fitDF$el_r2,col="darkgreen",lwd=2)
  lines(fitDF$day,fitDF$lf_r2,col="darkgreen",lwd=2,lty=3)
  
  missing=is.na(sapply(allDays,match,fitDF$day))
  
  rect(xleft=allDays[missing][-1]-1,ybottom=0,xright=allDays[missing][-1],ytop=1,
       col="white",border="white")
  
  legend(x="topright",legend=c("stream elev","stream uaa","lake elev","lake frac"),
         col=c("blue","blue","darkgreen","darkgreen"),lty=c(1,3,1,3),lwd=2,bty="n",y.intersp=0.8)
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

  
  #make the 'global'model huge (and redundant) for aic comparison:
  linearCombined=glm(Observation~I(FlowWtMeanLakeElev*LF)+LF+I(Elevation*(1-LF))+I(uaa*(1-LF))+Elevation+uaa,data=tempData,na.action=na.fail)
  summary(linearCombined)
  
  
  d=dredge(linearCombined,m.lim=c(1,6),
           subset=!("Elevation" && "I(Elevation * (1 - LF))"),
           extra='R^2')
  
  d$`R^2`=round(d$`R^2`,7)
  d$internalModelID=row.names(d)
  
  #8 hypothesized equations: one term: (streamElev, streamuaa, lakeElev, Fuaa_L), 
  #multi term: (streamElev+streamuaa, FuaaL(El)+FuaaS(Es), FuaaL(El)+FuaaS(Us), FuaaL(El)+FuaaS(Es+Us))
  
  r2_nls=function(nlsName){
    return(summary(lm(tempData$Observation~fitted(nlsName)))$r.squared)
  }
  
  modelNames=data.frame(name=c('Es','Us','Fl','Es_Us','Fl(Es_El)','Fl(Us_El)','Fl(Es_Us_El)'
                               ,'Es_Fl','Us_Fl','Es_Us_Fl'),internalModelID=0,stringsAsFactors = F)
  
  modelNames$internalModelID[modelNames$name=='Es']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='Us']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ uaa + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='Fl']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ LF + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='Es_Us']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + uaa + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='Fl(Es_El)']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ I(Elevation * (1 - LF)) + I(FlowWtMeanLakeElev * LF) + LF + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='Fl(Us_El)']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ I(FlowWtMeanLakeElev * LF) + LF + I(uaa * (1 - LF)) + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='Fl(Es_Us_El)']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ I(Elevation * (1 - LF)) + I(FlowWtMeanLakeElev * LF) + LF + I(uaa * (1 - LF)) + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='Es_El']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + FlowWtMeanLakeElev + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='Es_Fl']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + LF + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='Es_Us_Fl']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + LF + uaa + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='Us_Fl']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ LF + uaa + 1, data = tempData, na.action = na.fail)"]
  
  d = d[d$internalModelID %in% modelNames$internalModelID]
  display=data.frame(internalModelID=d$internalModelID,r_2=d$`R^2`,AICc=d$AICc,deltaAICc=d$delta)
  display=merge(x=display,y=modelNames,by='internalModelID',all.x=TRUE,sort=FALSE)[,c('name','r_2','AICc','deltaAICc')]
  display$dAICc_0=display$deltaAICc-min(display$deltaAICc)
  return(display[,c("name","r_2","dAICc_0")])
}

makeTsPlot=function(startDate=as.Date("2014-08-01"),endDate=as.Date("2015-08-01"), highlight=data.frame(start="2014-01-01",end="2014-01-02")){
  
  drawHighlight=function(cords=highlight){
    cords$start=as.Date(cords$start)
    cords$end=as.Date(cords$end)
    cords$bottom=-20
    cords$top=1000
    rect(cords$start,cords$bottom,cords$end,cords$top, col=rgb(0.8,0.8,0.8,alpha=0.5),border=NA)
  }
  
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
  #windows(width=7.5, height=10)
  png(filename="StreamTempContext.png",width=7.5,height=10, units="in", res=120)
  
  
  #backround highlights plot
  par(oma=c(1,1,0,2))
  plot(plotPrecip$date,plotPrecip$ppt_tot,ylim=c(max(precip$ppt_tot,na.rm=T),1),
       type="n",axes=F,xlab='',xlim=xlim,ylab='')
  drawHighlight()
  
  #precip
  par(new=T,oma=c(34,1,0,2))
  plot(plotPrecip$date,plotPrecip$ppt_tot,ylim=c(max(precip$ppt_tot,na.rm=T),1),
       type="h",lwd=3,lend=2,col="snow4",axes=F,xlab='',xlim=xlim,
       ylab='',main="Precipitation, Streamflow, Air Temperature, and Stream Temperature", cex.main=1.1)
  #graphics::box(bty="7")
  axis(side=2, at=c(0,25,50))
  mtext(text="Precipitation (mm)", side=2,line=2.2,col="snow4", font=2)
  
  #streamflow
  if(length(flow$Flow>=1)){
    par(new=T,oma=c(26,1,4,2))
    plot(flow$DateTime,flow$Flow,type="p",pch=20,cex=0.5,axes=F,xlab="",ylab="",log="y",
         col="blue",ylim=c(min(flow$Flow),max(flow$Flow)))
    axis(side=4, at=c(10, 100, 500))
    mtext(text="Streamflow (cfs)",side=4,line=2.2, col="blue", font=2)
  }
  
  
  #airTemp
  if(length(temp$airtemp_max>=1)){
    par(new=T,oma=c(16,1,12,2))
    plot(temp$date,temp$airtemp_avg,axes=F,xlab="",ylab="",xlim=xlim,
         ylim=c(min(temp$airtemp_min,na.rm=T),max(temp$airtemp_max,na.rm=T)+3),type="n")
    segments(x0=temp$date,y0=temp$airtemp_min,y1=temp$airtemp_max,lwd=5,col="grey")
    lines(temp$date,temp$airtemp_avg,lwd=2)
    axis(side=2,at=c(-20,-10,0,10,20))
    mtext(text="       Air Temperature (C)",side=2,line=2.2,font=2)
  }
  
  
  #streamTemp
  par(new=T,oma=c(9,1,20,2))
  plot(dayTemp$date,dayTemp$mean,xlim=xlim,ylim=c(0,max(dayTemp$max,na.rm=T)),
       axes=F,xlab='',ylab='',type="n",bty="n")
  segments(x0=dayTemp$date,y0=dayTemp$min,y1=dayTemp$max,lwd=5,col="grey")
  lines(dayTemp$date,dayTemp$mean,lwd=2)
  axis(side=4)
  mtext(text='Stream Temperature (C)',font=2,side=4,line=2.2)
  
  
  #n
  par(new=T,oma=c(1,1,30,2))
  plot(dayTemp$date,dayTemp$n,xlim=xlim,axes=F,xlab='',ylab='',type='l',ylim=c(0,20))
  #abline(h=10,lty=3)
  #temp elev r2*10 line.  relies on muchData from ElevAcrossTime.R
  #lines(muchData$day,muchData$elev_r2*10,lty=1,lwd=2)
  axis(side=2,at=c(0,5,10,15))
  mtext(text="# of Sites",side=2, font=2, line=2.2)
  axis.Date(side=1,at=seq.Date(from=startDate,to=endDate,length.out=13), las=3, format="%e-%b-%Y")
  
  dev.off()
  
}

plotSet=function(sitesSet){
  sitesSet=unique(sitesSet$SiteIDX)
  library(sp)
  library(raster)
  library(sf)
  library(units)
  
  gc_net=st_read("C:/Users/Sam/Desktop/spatial/QgisEnvironment/Active/d8_500_simple/GlacStreamSegments.shp",quiet=T)
  gc_net$length=st_length(gc_net)
  
  nsv_net=st_read("C:/Users/Sam/Desktop/spatial/QgisEnvironment/Active/d8_500_simple/NSVStreamSegments.shp",quiet=T)
  #drop 1-point lines from nev multiline
  nsv_net$length=st_length(nsv_net)
  nsv_net=nsv_net[nsv_net$length>=set_units(10,m),]
  par(mfrow=c(1,1))
  
  plot(st_geometry(rbind(nsv_net,gc_net)),lwd=2,col="grey")
  
  #load all points:
  loggerSites=st_read("C:/Users/Sam/Desktop/spatial/QgisEnvironment/Active/sitesLakeInfluence_tempPaper2/Inputs/NSVGCloggers.shp",quiet = T)
  plot(st_geometry(loggerSites),add=T,pch=9,cex=1.5,lab=theseLoggers$SiteIDX,col="grey")
  
  theseLoggers=loggerSites[loggerSites$SiteIDX %in% sitesSet,]
  plot(st_geometry(theseLoggers),add=T,pch=9,cex=1.5,lab=theseLoggers$SiteIDX)
  
}

setLength=function(set){
  f=filterToSet(set)
  return(length(unique(f$day)))
}

setEnd=function(set){
  f=filterToSet(set)
  return(max(f$day))
}

smoothByWindow=function(set,window){
  set$day=as.Date(set$day)
  set$periodMean=0
  sites=unique(set$SiteIDX)
  days=unique(set$day)
  for(s in sites){
    for(d in days){
      d=as.Date(d,origin ="1970-01-01")
      set[set$SiteIDX==s & set$day==d,"periodMean"]=mean(
        set[set$site==s & set$day>d-(window/2) & set$day<d+(window/2),"Observation"]
      )
    }
  }
  set$Observation=set$periodMean
  return(set[,-11])
}

fitAllModels=function(set){
  days=as.Date(unique(set$day))
  fitModelsByDay=function(day,set){
    thisSet=set[set$day==as.Date(day),]
    return(fitStreamTemp(thisSet))
  }
  allFits=lapply(days,fitModelsByDay,set=set)
  
  modelAicR=data.frame(day=days)
  getModelMetric=function(fitDay,model,metric){
    return(fitDay[fitDay$name==model,names(fitDay)==metric])
  }
  models=c('Es','Us','El','Fl','Es_Us','Fl(Es_El)','Fl(Us_El)','Fl(Es_Us_El)','Es_El','Es_Us_El'
           ,'Us_El','El_Fl','Es_El_Fl','Es_Us_El_Fl','Us_El_Fl','Es_Fl','Es_Us_Fl','Us_Fl')
  
  for(model in models){
    modelAicR$temp=sapply(allFits,getModelMetric,model=model,metric="dAICc_0")
    names(modelAicR)[names(modelAicR)=="temp"]=paste0(model,"_dAIC")
    modelAicR$temp=sapply(allFits,getModelMetric,model=model,metric="r_2")
    names(modelAicR)[names(modelAicR)=="temp"]=paste0(model,"_r2")
  }
  return(modelAicR)
}

plotMultipleSets=function(setIDs,allsets=all8Sets){
  library(tidyverse)
  df=data.frame(day=numeric(),
                streamElev_r2=numeric(),
                lakeElev_r2=numeric())
  for (setID in setIDs){
    set=all8Sets[setID]
    thisSetData=filterToSet(set)
    #thisSetData=smoothByWindow(thisSetData,10)
    thisSetFit=fitSimpleModels(thisSetData,F)
    df=rbind(df,data.frame(day=as.Date(thisSetFit$day),
                           streamElev_r2=thisSetFit$elev_r2,
                           lakeElev_r2=thisSetFit$el_r2))
  }
  getQuantile=function(metric,day,quantile){
    tempVect=df[df$day==day,metric]
    q=quantile(tempVect,quantile)
  }
  df$dayNumeric=as.numeric(df$day)
  df$med_stream=sapply(df$day,getQuantile,metric="streamElev_r2",quantile=.5)
  df$med_lake=sapply(df$day,getQuantile,metric="lakeElev_r2",quantile=.5)
  

  #loess_span=1/6 #~12 months represented, so span (window) ~ = 2 months
  
  #l_stream=loess(df$streamElev_r2~df$dayNumeric, span=loess_span)
  #l_lake=loess(df$lakeElev_r2~df$dayNumeric, span=loess_span)
  # summaryDF=left_join(data.frame(day=l_stream$x,str_lowess=l_stream$fitted),
  #                     data.frame(day=l_lake$x,lak_loess=l_lake$fitted),by=c("df.dayNumeric"="df.dayNumeric"))
  # names(summaryDF)=c("day","str_loess","lak_loess")
  # summaryDF$day=as.Date(summaryDF$day, origin=as.Date("1970-01-01"))
  #summaryDF=summaryDF[order(summaryDF),]
  df=df[order(df$day),]
  png(filename="streamLake_r2.png",width=10,height=7.5, units="in", res=300)
  par(mar=c(7,4,1,1))
  plot(df$day,df$streamElev_r2,pch=1,cex=1,ylim=c(0,1),xlab="",ylab="r^2",axes=F,col=rgb(r=96,g=142,b=211,alpha=60,maxColorValue = 255)) #col=rgb(r=96,g=142,b=211,alpha=120,maxColorValue = 255)
  points(df$day,df$lakeElev_r2,pch=2,cex=1,col=rgb(r=211,g=96,b=96,alpha=60,maxColorValue = 255)) #col=rgb(r=211,g=96,b=96,alpha=30,maxColorValue = 255)
  
  lines(df$day,df$med_stream,lwd=6,col="white")
  lines(df$day,df$med_lake,lwd=6,col="white")
  
  lines(df$day,df$med_lake,lwd=3,lty=3,col=rgb(r=165,g=76,b=76,maxColorValue = 255)) #col=rgb(r=165,g=76,b=76,maxColorValue = 255)
  lines(df$day,df$med_stream,lwd=3,lty=1,col=rgb(r=45,g=92,b=163,maxColorValue = 255)) #col=rgb(r=45,g=92,b=163,maxColorValue = 255)
  
  
  
  # lines(summaryDF$day,summaryDF$str_loess,lwd=6,col="white")
  # lines(summaryDF$day,summaryDF$str_loess,lwd=3,lty=1) #col=rgb(r=45,g=92,b=163,maxColorValue = 255)
  # 
  # lines(summaryDF$day,summaryDF$lak_loess,lwd=6,col="white")
  # lines(summaryDF$day,summaryDF$lak_loess,lwd=3,lty=3) #col=rgb(r=165,g=76,b=76,maxColorValue = 255)
  
  
  
  axis.Date(side=1,at=seq.Date(from=as.Date("2014-08-01"),to=as.Date("2015-08-01"),by="month"), las=3, format="%e-%b-%Y")
  axis(side=2,at=c(0,0.2,0.4,0.6,0.8,1))
  
  legend(x="topright",legend=c("Stream Elevation individual fit","Stream Elevation loess fit","Lake Elevation individual fit","Lake Elevation loess fit"),
         pch=c(1,NA,2,NA),lty=c(NA,1,NA,3),lwd=c(1,3,1,3),col=c(rgb(r=45,g=92,b=163,maxColorValue = 255),rgb(r=165,g=76,b=76,maxColorValue = 255)), 
         bty="n")
  dev.off()
}
