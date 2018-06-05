require(RODBC)
require(tidyverse)
require(reshape2)
TempDB=odbcConnectAccess2007('C:/Users/Sam/Documents/LeakyRivers/Data/LeakyRiversTemperature_UPDATE.accdb')


deps_nsv_gc=left_join(sqlQuery(TempDB,"SELECT DeploymentIDX, SiteIDX FROM Deployments"),
                      sqlQuery(TempDB,"SELECT SiteIDX, SiteName FROM Sites"))
deps_nsv_gc=deps_nsv_gc[deps_nsv_gc$SiteIDX<20,]

tempData=sqlQuery(TempDB,"SELECT Observation, DateTimeLocal, DeploymentIDX FROM Data WHERE Status = 1")

tempData=inner_join(tempData,deps_nsv_gc)

lakeData=read.csv("C:\\Users\\Sam\\Desktop\\spatial\\QgisEnvironment\\Active\\sitesLakeInfluence_tempPaper2\\min_10000\\lakeFractionAndElevation.csv",header = F)
names(lakeData)=c("SiteIDX","LF","FlowWtMeanLakeElev")
tempData=left_join(tempData,lakeData)

siteData=read.csv("C:\\Users\\Sam\\Desktop\\spatial\\sites\\sites_elevation_uaa_corrected.csv",header = T)[c("SiteIDX","Elevation","uaa")]
tempData=left_join(tempData,siteData)

write.csv(tempData,"NSV_GC_TempData.csv")
