require(RODBC)
require(tidyverse)
require(reshape2)

TempDB=odbcConnectAccess2007('C:/Users/Sam/Documents/LeakyRivers/Data/LeakyRiversTemperature_UPDATE.accdb')


deps_nsv_gc=left_join(sqlQuery(TempDB,"SELECT DeploymentIDX, SiteIDX FROM Deployments"),
                      sqlQuery(TempDB,"SELECT SiteIDX, SiteName, Description FROM Sites"))
deps_nsv_gc=deps_nsv_gc[deps_nsv_gc$SiteIDX<20,]

tempData=sqlQuery(TempDB,"SELECT Observation, DateTimeLocal, DeploymentIDX FROM Data WHERE Status = 1")

tempData=inner_join(tempData,deps_nsv_gc)

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


tempData=aggregate(tempData[c("Description", "Observation","DateTimeLocal","DeploymentIDX","SiteName","SiteIDX","LF","FlowWtMeanLakeElev","Elevation","uaa")],by=list(dep=tempData$DeploymentIDX,day=tempData$DateTimeLocal),FUN=aggMeanFun)

tempData$DateTimeLocal=as.Date(tempData$DateTimeLocal)


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
  n=length(unique(tempDataDay$DeploymentIDX))
  return(n)
}

fitDF=data.frame(day=unique(tempData$DateTimeLocal))
fitDF$elev_r2=sapply(fitDF$day,FUN=fitGetR_elev)
fitDF$uaa_r2=sapply(fitDF$day,FUN=fitGetR_uaa)
fitDF$N=sapply(fitDF$day,FUN=getN)

muchData=fitDF[fitDF$N>=10,]
plot(muchData$day,muchData$elev_r2,type="l")
lines(muchData$day,muchData$uaa_r2,lty=2)

#sept no rain period
tempData1=tempData[tempData$DateTimeLocal>=as.Date("2014-09-17") & tempData$DateTimeLocal<=as.Date("2014-09-17"),]
plot(tempData1$Observation,tempData1$Elevation)
tempData1=aggregate(tempData1,by=list(d=tempData1$dep),FUN=aggMeanFun)
summary(lm(Observation~Elevation, data=tempData1))


source('~/R Projects/RMNPStreamTemp/fitStreamtemp_function.R')
#moderate precip, early as possible w/ good n
tempData1=tempData[tempData$DateTimeLocal>=as.Date("2014-08-23") & tempData$DateTimeLocal<=as.Date("2014-09-02"),]
fitStreamTemp(tempData1)

#much precip?
tempData2=tempData[tempData$DateTimeLocal>=as.Date("2014-08-01") & tempData$DateTimeLocal<=as.Date("2014-10-13"),]
fitStreamTemp(tempData2)

#runoff
tempData3=tempData[tempData$DateTimeLocal>=as.Date("2015-05-05") & tempData$DateTimeLocal<=as.Date("2015-05-15"),]
fitStreamTemp(tempData3)

#plot elev r2 against Q