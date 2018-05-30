setStatus=function(date,depID,status){
  date=as.Date(date,format="%Y/%m/%d")
  if(status=="in"){status=1}
  if(status=="out"){status=0}
  sqlQuery(TempDB,paste0("UPDATE Data SET Status = ",status," WHERE DeploymentIDX = ",depID," AND DATEDIFF('d', DateTimeLocal, '",date,"')<0 "))
}