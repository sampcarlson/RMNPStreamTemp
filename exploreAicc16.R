#compare models using aicc actoss the period w/ 16 sites
#dates: 8/21 - 9/4 (14 days), 9/17 - 10-13 (26 days)

source('~/R Projects/RMNPStreamTemp/tempFitFunctions.R')
tempData=buildTempData()

makeTsPlot(highlight=data.frame(start=c("2014-8-25","2014-10-3","2015-3-20","2015-5-13"),end=c("2014-8-31","2014-10-09","2015-3-27","2015-5-23")))

# t=tempData[tempData$day==as.Date("2014-09-20"),c("site","Observation","Descriptio")]
# t[order(t$Observation),]


tempData1=tempData[tempData$day==as.Date("2014-9-20"),]
fitStreamTemp(tempData1)

tempData16=tempData[tempData$day>=as.Date("2014-09-17") & tempData$day<=as.Date("2014-10-5"),]
fitStreamTemp(tempData16)

tempData16=tempData[tempData$day>=as.Date("2014-09-17") & tempData$day<=as.Date("2014-10-13"),]
fitStreamTemp(tempData16)



# some rain
tempData16=tempData[tempData$day>=as.Date("2014-09-21") & tempData$day<=as.Date("2014-09-28"),]
fitStreamTemp(tempData16)

seq.Date(from=as.Date("2014-08-21"), to=as.Date("2014-9-4"),by="week")

seq.Date(from=as.Date("2014-09-17"), to=as.Date("2014-10-13"),by="week")

#week-ish periods
tempData16=tempData[tempData$day>=as.Date("2014-08-21") & tempData$day<=as.Date("2014-8-27"),]
fitStreamTemp(tempData16)

tempData16=tempData[tempData$day>=as.Date("2014-08-28") & tempData$day<=as.Date("2014-9-04"),]
fitStreamTemp(tempData16)

tempData16=tempData[tempData$day>=as.Date("2014-09-17") & tempData$day<=as.Date("2014-9-23"),]
fitStreamTemp(tempData16)

tempData16=tempData[tempData$day>=as.Date("2014-09-24") & tempData$day<=as.Date("2014-9-30"),]
fitStreamTemp(tempData16)

#Es best here
tempData16=tempData[tempData$day>=as.Date("2014-10-1") & tempData$day<=as.Date("2014-10-7"),]
fitStreamTemp(tempData16)

tempData16=tempData[tempData$day>=as.Date("2014-10-8") & tempData$day<=as.Date("2014-10-13"),]
fitStreamTemp(tempData16)

#explore 9-28 to 10-7 weird period:
tempData1=tempData[tempData$day==as.Date("2014-09-26"),]
fitStreamTemp(tempData1)

tempData1=tempData[tempData$day==as.Date("2014-09-27"),]
fitStreamTemp(tempData1)

#Fl best here
tempData1=tempData[tempData$day==as.Date("2014-09-28"),]
fitStreamTemp(tempData1)

tempData1=tempData[tempData$day==as.Date("2014-09-29"),]
fitStreamTemp(tempData1)

#switch to Es
tempData1=tempData[tempData$day==as.Date("2014-09-30"),]
fitStreamTemp(tempData1)

tempData1=tempData[tempData$day==as.Date("2014-10-01"),]
fitStreamTemp(tempData1)

tempData1=tempData[tempData$day==as.Date("2014-10-02"),]
fitStreamTemp(tempData1)

tempData1=tempData[tempData$day==as.Date("2014-10-03"),]
fitStreamTemp(tempData1)

tempData1=tempData[tempData$day==as.Date("2014-10-04"),]
fitStreamTemp(tempData1)

tempData1=tempData[tempData$day==as.Date("2014-10-05"),]
fitStreamTemp(tempData1)

tempData1=tempData[tempData$day==as.Date("2014-10-06"),]
fitStreamTemp(tempData1)

#back to weighted
tempData1=tempData[tempData$day==as.Date("2014-10-07"),]
fitStreamTemp(tempData1)

tempData1=tempData[tempData$day==as.Date("2014-10-08"),]
fitStreamTemp(tempData1)

tempData1=tempData[tempData$day==as.Date("2014-10-09"),]
fitStreamTemp(tempData1)

makeTsPlot(startDate=as.Date("2014-09-24"),endDate=as.Date("2014-10-11"))
