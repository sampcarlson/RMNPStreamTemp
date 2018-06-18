source('~/R Projects/RMNPStreamTemp/tempFitFunctions.R')
tempData=buildTempData()

makeTsPlot()

# day from previous analysis
tempData0=tempData[tempData$DateTimeLocal==as.Date("2014-09-20"),]
fitStreamTemp(tempData0)

#period around 9-20 - unavoidable rain event in any 10 day period
tempData1=tempData[tempData$DateTimeLocal>=as.Date("2014-09-18") & tempData$DateTimeLocal<=as.Date("2014-9-28"),]
fitStreamTemp(tempData1,T)

#earliest period w/ 16 sites
tempData2=tempData[tempData$DateTimeLocal>=as.Date("2014-08-21") & tempData$DateTimeLocal<=as.Date("2014-8-31"),]
fitStreamTemp(tempData2, T)

#earliest period w/ 13 sites
tempData3=tempData[tempData$DateTimeLocal>=as.Date("2014-08-13") & tempData$DateTimeLocal<=as.Date("2014-08-17"),]
fitStreamTemp(tempData3,T)

#latest pre-freeze data
tempData4=tempData[tempData$DateTimeLocal>=as.Date("2014-10-13") & tempData$DateTimeLocal<=as.Date("2014-10-23"),]
fitStreamTemp(tempData4, T)

#winter period
tempData5=tempData[tempData$DateTimeLocal>=as.Date("2015-1-10") & tempData$DateTimeLocal<=as.Date("2015-1-23"),]
fitStreamTemp(tempData5,T)

#early - pre runoff
tempData6=tempData[tempData$DateTimeLocal>=as.Date("2015-4-25") & tempData$DateTimeLocal<=as.Date("2015-5-05"),]
fitStreamTemp(tempData6,T)

#main runoff
tempData7=tempData[tempData$DateTimeLocal>=as.Date("2015-6-1") & tempData$DateTimeLocal<=as.Date("2015-6-10"),]
fitStreamTemp(tempData7,T)
