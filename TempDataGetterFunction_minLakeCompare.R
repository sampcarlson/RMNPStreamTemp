
getDayData=function(day,siteInfoPath){
  period=1
  outputPlusLakes=NULL
  require(RODBC)
  TempDB=odbcConnectAccess2007('C:/Users/Sam/Documents/LeakyRivers/Data/LeakyRiversTemperature_UPDATE.accdb')
  
  sitesLakeInfo <- read.csv(siteInfoPath,header=F)
  names(sitesLakeInfo)=c("SiteIDX","AboveLakeFrac","wtLakeElev")
  
  formatDate = as.Date(day,format="%Y-%m-%d")
  deps=sqlQuery(TempDB,paste0("SELECT DISTINCT DeploymentIDX FROM Data WHERE ABS(DateDiff('d', DateTimeLocal, #",format(formatDate,format="%Y-%m-%d"),"# ))<",period," AND Status=1 AND MetricIDX=1"))$DeploymentIDX
  
  allDeployments=sqlQuery(TempDB,"SELECT * FROM Deployments")
  useDeps=allDeployments[allDeployments$SiteIDX %in% c(grep("NSV",sqlQuery(TempDB,"SELECT * FROM Sites")$SiteName),
                                                       grep("GC",sqlQuery(TempDB,"SELECT * FROM Sites")$SiteName)),]
  #useDeps=allDeployments[allDeployments$SiteIDX %in% grep("NSV",sqlQuery(TempDB,"SELECT * FROM Sites")$SiteName),]
  
  deps=deps[deps %in% useDeps$DeploymentIDX]
  
  output=data.frame(depIDX=integer(),siteIDX=integer(),siteName=character(),mean=double(),min=double(),max=double(),elevation=double(),uaa=double())
  if(length(deps)>0){
    for(i in 1:length(deps)){
      thisSiteIDX=sqlQuery(TempDB,paste0("SELECT SiteIDX FROM Deployments WHERE DeploymentIDX=",deps[i]))
      output=rbind(output,
                   data.frame(
                     depIDX=deps[i],
                     siteIDX=thisSiteIDX,
                     siteName=sqlQuery(TempDB,paste0("SELECT SiteName FROM Sites WHERE SiteIDX=",thisSiteIDX)),
                     mean=mean(sqlQuery(TempDB,paste0("SELECT Observation, DateTimeLocal FROM Data WHERE ABS(DateDiff('d', DateTimeLocal, #",format(formatDate,format="%Y-%m-%d"),"# ))<",period," AND Status=1 AND MetricIDX=1 AND DeploymentIDX=",deps[i]))$Observation),
                     min=min(sqlQuery(TempDB,paste0("SELECT Observation, DateTimeLocal FROM Data WHERE ABS(DateDiff('d', DateTimeLocal, #",format(formatDate,format="%Y-%m-%d"),"# ))<",period," AND Status=1 AND MetricIDX=1 AND DeploymentIDX=",deps[i]))$Observation),
                     max=max(sqlQuery(TempDB,paste0("SELECT Observation, DateTimeLocal FROM Data WHERE ABS(DateDiff('d', DateTimeLocal, #",format(formatDate,format="%Y-%m-%d"),"# ))<",period," AND Status=1 AND MetricIDX=1 AND DeploymentIDX=",deps[i]))$Observation),
                     elevation=sqlQuery(TempDB,paste0("SELECT Elevation FROM Sites WHERE SiteIDX = ",thisSiteIDX)),
                     uaa=sqlQuery(TempDB,paste0("SELECT Uaa FROM Sites WHERE SiteIDX = ",thisSiteIDX))
                   )
      )
    }
    #plot(output$Elevation,output$mean,pch="*",main=paste("mean temps for",day," +- ",period,"days"),xlim=c(min(output$Elevation)-100,max(output$Elevation)+100))
    #text(output$Elevation,output$mean,labels=output$SiteName)
    output=unique(output)
    outputPlusLakes=merge(output,sitesLakeInfo,by="SiteIDX")
  }
  
  odbcCloseAll()
  return(outputPlusLakes)
}