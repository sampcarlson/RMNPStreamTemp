#startDate=as.Date("2014-08-08")
#endDate=as.Date("2014-08-08")

source('~/R Projects/RMNPStreamTemp/tempFitFunctions.R')
tempData=buildTempData()

#for debug
#tempData=tempData[1000:1200,]

setsDF=getAllSets(tempData)
setsDF=setsDF[setsDF$setLength>=8,]
plot(setsDF$setLength,setsDF$setOccuranceCount,log="")
#identify 1
#setsDF[identify(setsDF$setLength,setsDF$setOccuranceCount,n=1),]

#best 11 set from 2014-10-29
set11=getSetByDate("2014-10-29")
set11Fit=fitElevUaa(set11)
plot(set11Fit$day,set11Fit$elev_r2)

#other 11 set from 2014-10-27 - this one includes some high flow times
set11_2=getSetByDate("2014-10-27")
set11_2Fit=fitElevUaa(set11_2)
plot(set11_2Fit$day,set11_2Fit$elev_r2)

#best 10 set is from 2014-12-15
set10=getSetByDate("2014-12-15")
set10fit=fitElevUaa(set10)
plot(set10fit$day,set10fit$elev_r2)

#best 9 set occurs on 2014-10-28
set9=getSetByDate("2014-10-28")
set9fit=fitElevUaa(set9)
plot(set9fit$day,set9fit$elev_r2)
