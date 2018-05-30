TempDB=odbcConnectAccess2007('C:/Users/Sam/Documents/LeakyRivers/Data/LeakyRiversTemperature_UPDATE.accdb')
day='2014-09-20'
formatDate = as.Date(day,format="%Y-%m-%d")
period=1

sqlQuery(TempDB,"SELECT * From Sites")
sqlQuery(TempDB,"SELECT * From Deployments")

sqlQuery(TempDB,paste0("SELECT * FROM Data WHERE ABS(DateDiff('d', DateTimeLocal, #",format(formatDate,format="%Y-%m-%d"),"# ))<",period," AND MetricIDX=1 AND DeploymentIDX=4"))

#was 3
sqlQuery(TempDB,paste0("UPDATE Data SET Status=1 WHERE ABS(DateDiff('d', DateTimeLocal, #",format(formatDate,format="%Y-%m-%d"),"# ))<",period," AND MetricIDX=1 AND DeploymentIDX=4"))
