require(RODBC)
require(tidyverse)
require(reshape2)
TempDB=odbcConnectAccess2007('C:/Users/Sam/Documents/LeakyRivers/Data/LeakyRiversTemperature_UPDATE.accdb')

deps_nsv_gc=left_join(sqlQuery(TempDB,"SELECT DeploymentIDX, SiteIDX FROM Deployments"),
                      sqlQuery(TempDB,"SELECT SiteIDX, SiteName FROM Sites"))
deps_nsv_gc=deps_nsv_gc[deps_nsv_gc$SiteIDX<20,]

deps_nsv_gc$DeploymentIDX
startDay='2014-07-01'
add=0:400
period=1
formatDate = as.Date(startDay,format="%Y-%m-%d")+add
getDepCount=function(formatDate){
  deps=sqlQuery(TempDB,
                paste0("SELECT DISTINCT DeploymentIDX FROM Data 
                     WHERE ABS(DateDiff('d', DateTimeLocal, #",format(formatDate,format="%Y-%m-%d"),"# ))<",period," 
                     AND Status=1 AND MetricIDX=1 AND DeploymentIDX IN (",paste(deps_nsv_gc$DeploymentIDX,collapse=" ,"),")" ))$DeploymentIDX
  return(length(deps))
}

goodDeploymentCount=sapply(formatDate,getDepCount)

deploymentDays=data.frame(days=formatDate,deps=goodDeploymentCount)

windows(width=25)
plot(deploymentDays,xaxt='n',xlim=as.Date(c('2014-07-01','2015-09-01')))
axis.Date(side=1,at=formatDate[seq(from=1,to=400,by=15)], labels=formatDate[seq(from=1,to=400,by=15)],las=2)
abline(h=10)

