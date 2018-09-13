source('~/R Projects/RMNPStreamTemp/tempFitFunctions.R')
tempData=buildTempData()

makeTsPlot(highlight=data.frame(start=c("2014-8-24","2014-10-5","2015-3-28","2015-5-2"),end=c("2014-8-28","2014-10-9","2015-4-1","2015-5-6")))

#n=17, Fl(Es_El) wins
period1=tempData[tempData$day>=as.Date("2014-8-24") & tempData$day<=as.Date("2014-8-28"),]
fitStreamTemp(period1)

#n=16, Fl(Es_El) wins
period2=tempData[tempData$day>=as.Date("2014-10-5") & tempData$day<=as.Date("2014-10-9"),]
fitStreamTemp(period2)

#n=13, Es wins, very low r2
period3=tempData[tempData$day>=as.Date("2015-3-28") & tempData$day<=as.Date("2015-4-1"),]
fitStreamTemp(period3)

#n=13, Es wins
period4=tempData[tempData$day>=as.Date("2015-5-2") & tempData$day<=as.Date("2015-5-6"),]
fitStreamTemp(period4)

