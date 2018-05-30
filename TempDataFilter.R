
#list of status definitions:
# 1: good
# 2: incomplete
# 3: temp is changing too fast
# 4: frozen
# 5: 
# 6: daily range too big
# 7: 
# 8: mean is unreasonable
# 9: after last retrieval


BigThomMorainePark=read.table(
  "http://nwis.waterdata.usgs.gov/nwis/uv?cb_00010=on&format=rdb&site_no=402114105350101&period=&begin_date=2013-07-01&end_date=2015-12-30",
  comment.char="#",skip=28,sep="",as.is=T,col.names=c("Agency","Site_no","Date","Time","Timezone","WaterTemp","ObservationStatus"))
for (i in 1:nrow(BigThomMorainePark)){
  BigThomMorainePark$DateTime[i]=as.POSIXct(paste(toString(BigThomMorainePark$Date[i]),toString(BigThomMorainePark$Time[i])))
}
BigThomMorainePark$DateTime=format(BigThomMorainePark$DateTime,tz="Etc/GMT+6")
library(RODBC)
conn=odbcConnectAccess2007('C:/Users/Sam/Documents/LeakyRivers/Data/LeakyRiversTemperature_UPDATE.accdb')
#conn <- odbcConnect("LeakyRiversTemp")
#windows(rescale="R",xpos=0,ypos=0)

DepIDs=sqlQuery(conn,"SELECT DeploymentIDX FROM Deployments")$DeploymentIDX

for (i in 1:length(DepIDs)){    
  data=sqlQuery(conn,paste("SELECT * FROM Data WHERE DeploymentIDX = ",DepIDs[i]," ORDER BY DateTimeLocal"),as.is=T)
  data$DateTime=as.POSIXct(data$DateTimeLocal,tz="Etc/GMT+6",format="%Y-%m-%d %H:%M:%S")
  addToData=data.frame(data$DatumIDX,data$DateTime)
  
  if(nrow(data)>1){
    #readline("press enter for next plot")
    
    days=as.POSIXct(seq.POSIXt(min(data$DateTime),max(data$DateTime),by="day"))
    inStream=rep("1",length(days))
    
    
    #determine if sensor is in or out of water
    for (d in 1:(length(days))){
      dayBigT=as.numeric(BigThomMorainePark$WaterTemp[as.numeric(BigThomMorainePark$DateTime)-as.numeric(days[d])<=86400 & as.numeric(BigThomMorainePark$DateTime)-as.numeric(days[d])>0 ])
      if(any(is.na(dayBigT))){
        dayBigT=NA
      }
      dayDataLogical=(as.numeric(data$DateTime)-as.numeric(days[d])<=86400 & as.numeric(data$DateTime)-as.numeric(days[d])>0)
      dayData=data[dayDataLogical,]$Observation
      
      dataStepMax=0
      dataStepTemp=0
      bigTStepMax=0
      bigTStepTemp=0
      if(length(dayData)==48){
        for(s in 1:(length(dayData)-1)){
          dataStepTemp=abs(dayData[s+1]-dayData[s])
          if(is.na(dataStepTemp)){dataStepTemp=0}
          dataStepMax=max(dataStepMax,dataStepTemp)
          b=1+(s-1)*2   #BigT is in 15 minute, my data is in 30 min
          bigTStepTemp=abs(dayBigT[b+2]-dayBigT[b])
          bigTStepMax=max(bigTStepMax,bigTStepTemp)
          
          if(is.na(bigTStepMax)){bigTStepMax=2}  #no data from big t when frozen, therefore this number is the max allowable 30min change in wintertime stream temp
        }
      }
      
      if(length(dayData)<48){
        inStream[d]=2
      }
      
      if (days[d]>"2015-10-10 GMT+6"){
        inStream[d]="9"   # after last retrieval
      }
      
      if(dataStepMax>=bigTStepMax*1.5){
        #temp is changing too fast: out of water
        inStream[d]="3"
      }
      
      if(mean(dayData)<1){
        #Frozen
        inStream[d]="4"
      }
      
      if (length(dayBigT)==96){             #prevously (returns warnings): (is.finite(max(dayBigT))){
        if (abs(mean(dayData)-mean(dayBigT))>=6){
          #mean is way off, out of water
          inStream[d]="8"
        }else if (max(dayData)-min(dayData)>1.5*max(dayBigT)-min(dayBigT)){
          #Range too big:  out of water
          inStream[d]="6"
        }
      }
      
      # Record Stream Status
      
      addToData$data.Status[dayDataLogical]=inStream[d]
    }
    names(addToData)=c("DatumIDX","DateTimeSimple","Status")
    #print(head(data))
    #print(head(addToData))
    #Add Date and Status data to Access Database
    sqlUpdate(conn,addToData,tablename="Data",index="DatumIDX")
    
    #generate labels and plot
    #windows()
    siteIDX=sqlQuery(conn,paste("SELECT * FROM Deployments WHERE DeploymentIDX = ",DepIDs[i]))
    siteNote=sqlQuery(conn,paste("SELECT * FROM Sites WHERE SiteIDX = ",siteIDX$SiteIDX))
    #pick point color based on in/out of water
    pointCol=25+7*(data$Status>1)
    plot(data$DateTime,data$Observation,main=paste("DeploymentIDX:",DepIDs[i]," SiteDesc:",siteNote$Description),pch="*",col=pointCol,xaxt="n",xlab="")
    axis.POSIXct(1,data$DateTime,at=seq(min(data$DateTime),max(data$DateTime)+86400,"days"),labels=F)
    axis.POSIXct(1,data$DateTime,at=seq(min(data$DateTime),max(data$DateTime)+86400,"weeks"),format="%m-%d",las=3,tck=-0.02)
    points(BigThomMorainePark$DateTime,BigThomMorainePark$WaterTemp,pch=".")
    points(days+43200,rep(16,length(days)),pch=16,col=as.numeric(inStream))
    legend("topright",c("Good","Frozen","Temp change per 1/2 hour > 1.5 * BigThom",
                        "Mean day temp is 6 or more c different than BigThom","Range is > 1.5* BigThom range","Incomplete Data","After last retrieval"),
           col=c(1,4,3,8,6,2,5),pch=16)
    legend("topleft",c("SamData","USGS BigThom"),pch=c("*","."))
  }
}
odbcCloseAll()
