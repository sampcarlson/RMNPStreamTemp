#look at depIDX's (14, 31, 39, 33), 8, 18, 2, 
#31,39,8

setAllStatus=function(){
  
  
  TempDB=dbConnect(SQLite(),"C:\\Users\\Sam\\Documents\\LeakyRivers\\Data\\temperature\\leakyTemp.db3")
  
  
  setStatus=function(date,depID,status){
    date=as.Date(date,format="%Y/%m/%d")
    if(status=="in"){status=1}
    if(status=="out"){status=0}
    #  dbGetQuery(TempDB,paste0("UPDATE Data SET Status = ",status," WHERE DeploymentIDX = ",depID," AND DATEDIFF('d', DateTimeLocal, '",date,"')<0 "))
    dbExecute(TempDB,paste0("UPDATE Data SET Status = ",status," WHERE DeploymentIDX = ",depID," AND ((julianday(DateTimeLocal) - julianday('",date,"')) > 1 )"))
    
  }
  #Set all data as bad
  dbExecute(TempDB,"UPDATE Data SET Status = 0")
  #set as good where appropriate.  must go in chronological order
  setStatus("2013/7/4",depID = 31, status = 1)#ouzel
  setStatus("2013/7/4",depID = 34, status = 1)
  setStatus("2013/7/9",depID = 33, status = 1)
  setStatus("2013/8/1",depID = 1, status = 1)
  setStatus("2013/8/23",depID = 15, status = 1)
  setStatus("2013/9/12",depID = 26, status = 1)
  setStatus("2013/9/18",depID = 31, status = 0)#ouzel
  setStatus("2013/9/18",depID = 34, status = 0)
  setStatus("2013/10/3",depID = 33, status = 0)
  setStatus("2013/10/3",depID = 1, status = 0)
  setStatus("2013/10/16",depID = 26, status = 0)
  setStatus("2014/4/24",depID = 33, status = 1)
  setStatus("2014/5/5",depID = 26, status = 1)
  setStatus("2014/5/11",depID = 33, status = 0)
  setStatus("2014/5/15",depID = 33, status = 1)
  setStatus("2014/5/20",depID = 31, status = 1)#ouzel
  setStatus("2014/5/30",depID = 34, status = 1)
  setStatus("2014/6/2",depID = 1, status = 1)
  setStatus("2014/7/1",depID = 34, status = 0)
  setStatus("2014/7/18",depID = 33, status = 0)
  setStatus("2014/8/4",depID = 5, status = 1)
  setStatus("2014/8/4",depID = 6, status = 1)
  setStatus("2014/8/4",depID = 4, status = 1)
  setStatus("2014/8/4",depID = 3, status = 1)
  setStatus("2014/8/5",depID = 4, status = 1)
  setStatus("2014/8/4",depID = 7, status = 1)
  setStatus("2014/8/5",depID = 8, status = 1)
  setStatus("2014/8/6",depID = 11, status = 1)
  setStatus("2014/8/6",depID = 10, status = 1)
  setStatus("2014/8/6",depID = 9, status = 1)
  setStatus("2014/8/11",depID = 12, status = 1)
  setStatus("2014/8/12",depID = 13, status = 1)
  setStatus("2014/8/18",depID = 15, status = 0)
  setStatus("2014/8/18",depID = 1, status = 0)
  #setStatus("2014/8/19",depID = 20, status = 1)  #redundant icy brook deployment
  setStatus("2014/8/19",depID = 17, status = 1)
  setStatus("2014/8/19",depID = 18, status = 1)
  setStatus("2014/8/19",depID = 21, status = 1)
  setStatus("2014/8/19",depID = 31, status = 0) #ouzel
  setStatus("2014/8/19",depID = 14, status = 1) #ouzel
  setStatus("2014/8/20",depID = 16, status = 1)
  setStatus("2014/8/23",depID = 19, status = 1)
  setStatus("2014/9/1",depID = 26, status = 0)
  setStatus("2014/9/2",depID = 28, status = 1)
  setStatus("2014/9/2",depID = 22, status = 1)
  setStatus("2014/9/2",depID = 29, status = 1)
  setStatus("2014/9/5",depID = 8, status = 0)
  setStatus("2014/9/7",depID = 30, status = 1)
  setStatus("2014/9/4",depID = 11, status = 0)
  setStatus("2014/9/7",depID = 11, status = 1)
  setStatus("2014/9/12",depID = 22, status = 0)
  setStatus("2014/9/12",depID = 13, status = 0)
  setStatus("2014/9/12",depID = 13, status = 0)
  setStatus("2014/9/13",depID = 31, status = 0) #ouzel
  setStatus("2014/9/13",depID = 14, status = 0)#ouzel
  setStatus("2014/9/13",depID = 38, status = 1)
  setStatus("2014/9/14",depID = 39, status = 1) #ouzel
  setStatus("2014/9/15",depID = 12, status = 0)
  setStatus("2014/9/16",depID = 8, status = 1)
  setStatus("2014/9/16",depID = 12, status = 1)
  setStatus("2014/10/4",depID = 28, status = 0)
  setStatus("2014/10/13",depID = 4, status = 0)
  setStatus("2014/10/15",depID = 10, status = 0)
  setStatus("2014/10/23",depID = 11, status = 0)
  setStatus("2014/10/25",depID = 18, status = 0)
  setStatus("2014/10/26",depID = 30, status = 0)
  setStatus("2014/10/26",depID = 6, status = 0)
  setStatus("2014/10/27",depID = 3, status = 0)
  setStatus("2014/10/27",depID = 17, status = 0)
  setStatus("2014/10/28",depID = 3, status = 1)
  setStatus("2014/10/28",depID = 6, status = 1)
  setStatus("2014/10/29",depID = 2, status = 0)
  setStatus("2014/11/03",depID = 6, status = 0)
  setStatus("2014/11/03",depID = 3, status = 0)
  setStatus("2014/11/11",depID = 39, status = 1) #ouzel
  setStatus("2014/11/11",depID = 3, status = 1)
  setStatus("2014/11/11",depID = 6, status = 1)
  #setStatus("2014/11/12",depID = 16, status = 1)
  setStatus("2014/11/13",depID = 22, status = 1)
  setStatus("2014/11/18",depID = 22, status = 0)
  setStatus("2014/11/23",depID = 3, status = 0)
  setStatus("2014/12/18",depID = 38, status = 0)
  setStatus("2014/12/21",depID = 38, status = 1)
  setStatus("2015/1/3",depID = 38, status = 0)
  setStatus("2015/1/4",depID = 2, status = 1)
  setStatus("2015/1/27",depID = 10, status = 1)
  setStatus("2015/2/15",depID = 38, status = 1)
  setStatus("2015/3/15",depID = 17, status = 1)
  setStatus("2015/3/17",depID = 30, status = 1)
  setStatus("2015/3/19",depID = 11, status = 1)
  setStatus("2015/3/22",depID = 6, status = 0)
  setStatus("2015/3/27",depID = 3, status = 1)
  setStatus("2015/4/14",depID = 6, status = 1)
  #setStatus("2015/4/14",depID = 20, status = 1) #redundant icy brook deployment
  setStatus("2015/4/22",depID = 28, status = 1)
  setStatus("2015/4/5",depID = 38, status = 0)
  setStatus("2015/4/17",depID = 38, status = 1)
  setStatus("2015/5/1",depID = 4, status = 1)
  setStatus("2015/5/1",depID = 22, status = 1)
  setStatus("2015/6/1",depID = 28, status = 0)
  setStatus("2015/8/20",depID = 28, status = 1)
  setStatus("2015/8/21",depID = 22, status = 0)
  setStatus("2015/10/4",depID = 12, status = 0)
  setStatus("2015/10/5",depID = 29, status = 0)
  setStatus("2015/10/7",depID = 16, status = 0)
  setStatus("2015/10/7",depID = 17, status = 0)
  setStatus("2015/10/8",depID = 39, status = 0) #ouzel
}



