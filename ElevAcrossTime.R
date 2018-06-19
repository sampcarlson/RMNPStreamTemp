
#moderate precip, early as possible w/ good n
tempData1=tempData[tempData$DateTimeLocal>=as.Date("2014-08-23") & tempData$DateTimeLocal<=as.Date("2014-09-02"),]
fitStreamTemp(tempData1,T)

#much precip?
tempData2=tempData[tempData$DateTimeLocal>=as.Date("2014-08-15") & tempData$DateTimeLocal<=as.Date("2014-10-15"),]
fitStreamTemp(tempData2, T)

#runoff
tempData3=tempData[tempData$DateTimeLocal>=as.Date("2015-05-05") & tempData$DateTimeLocal<=as.Date("2015-05-15"),]
fitStreamTemp(tempData3)

#good period
tempData4=tempData[tempData$DateTimeLocal>=as.Date("2014-09-12") & tempData$DateTimeLocal<=as.Date("2014-09-21"),]
fitStreamTemp(tempData4, T)


#one (very good) day
tempData0=tempData[tempData$DateTimeLocal==as.Date("2014-09-20"),]
fitStreamTemp(tempData0)

#plot elev r2 against Q