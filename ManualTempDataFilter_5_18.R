require(tidyverse)
require(reshape2)
require(RSQLite)
source('~/R Projects/RMNPStreamTemp/plotTempDataFunction.R')
source('~/R Projects/RMNPStreamTemp/setDepStatus.R')

TempDB=dbConnect(SQLite(),"C:\\Users\\Sam\\Documents\\LeakyRivers\\Data\\temperature\\leakyTemp.db3")


#Get Moraine Park usgs data. Could use waterData package, but i want high temporal resolution not daily mean
BigThomMorainePark_Temp=read.table(
  "http://nwis.waterdata.usgs.gov/nwis/uv?cb_00010=on&format=rdb&site_no=402114105350101&period=&begin_date=2013-07-01&end_date=2015-10-30",
  comment.char="#",skip=28,sep="",as.is=T,col.names=c("Agency","Site_no","Date","Time","Timezone","Temperature","ObservationStatus"))
for (i in 1:nrow(BigThomMorainePark_Temp)){
  BigThomMorainePark_Temp$DateTime[i]=as.POSIXct(paste(toString(BigThomMorainePark_Temp$Date[i]),toString(BigThomMorainePark_Temp$Time[i])))
}
class(BigThomMorainePark_Temp$DateTime)=c('POSIXt','POSIXct')


BigThomMorainePark_Flow=read.table(
  "http://nwis.waterdata.usgs.gov/nwis/uv?cb_00060=on&format=rdb&site_no=402114105350101&period=&begin_date=2013-07-01&end_date=2015-12-30",
  comment.char="#",skip=28,sep="",as.is=T,col.names=c("Agency","Site_no","Date","Time","Timezone","Flow","ObservationStatus"))
for (i in 1:nrow(BigThomMorainePark_Flow)){
  BigThomMorainePark_Flow$DateTime[i]=as.POSIXct(paste(toString(BigThomMorainePark_Flow$Date[i]),toString(BigThomMorainePark_Flow$Time[i])))
}
class(BigThomMorainePark_Flow$DateTime)=c('POSIXt','POSIXct')


BigThom=left_join(BigThomMorainePark_Flow,BigThomMorainePark_Temp)[c("DateTime","Flow","Temperature")]



#get nsv, gc deps
# nah, look at all data instead

# deps_nsv_gc=left_join(dbGetQuery(TempDB,"SELECT DeploymentIDX, SiteIDX FROM Deployments"),
#                       dbGetQuery(TempDB,"SELECT SiteIDX, SiteName FROM Sites"))
# deps_nsv_gc=deps_nsv_gc[deps_nsv_gc$SiteIDX<20,]


setAllStatus()

days=seq.Date(from=as.Date("2013-07-01"),to=as.Date("2015-12-30"),by=1)


#
#just for reference:
dbGetQuery(TempDB,"SELECT Sites.SiteName, Sites.SiteIDX, Sites.Description, Deployments.DeploymentIDX From Sites LEFT JOIN Deployments on Sites.SiteIDX = Deployments.SiteIDX")

windows(width=10,height=8)


plotDayPeriod(days[715],3)



#2013 flood
#plotDayPeriod(days[75],30)

