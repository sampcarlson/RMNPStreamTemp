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
setsDF[identify(setsDF$setLength,setsDF$setOccuranceCount,n=1),]

#best 11 set from 2014-10-29
set11=getSetByDate("2014-10-29")
set11Fit=fitSimpleModels(set11)
plot(set11Fit$day,set11Fit$elev_r2)
plotSet(set11)

#other 11 set from 2014-10-27 - this one includes some high flow times
set11_2=getSetByDate("2014-10-27")
set11_2Fit=fitSimpleModels(set11_2)

plot(set11_2Fit$day,set11_2Fit$elev_r2)

#best 10 set is from 2014-12-15
set10=getSetByDate("2014-12-15")
set10fit=SimpleModels(set10)
plot(set10fit$day,set10fit$elev_r2)

#best 9 set occurs on 2014-10-28
set9=getSetByDate("2014-10-28")
set9fit=SimpleModels(set9)
plot(set9fit$day,set9fit$elev_r2)

plotSet(set9)

#manually identified sets:
set10_2=c(2,4,7,8,11,12,13,14,17,19)
set10_2=filterToSet(set10_2)
set10_2fit=SimpleModels(set10_2)
plot(set10_2fit$day,set10_2fit$elev_r2)


#all 16
set16=getSetByDate("2014-08-21")
all8Sets=combn(unique(set16$SiteIDX),8,simplify=F)

set8Info=data.frame(id=1:length(all8Sets))

#this takes ~30 minutes
set8Info$length=sapply(all8Sets,setLength)
set8Info$end=sapply(all8Sets,setEnd)

lateSets=set8Info[set8Info$end>="2015-05-15",]
lateSets
longSets=set8Info[set8Info$length>200,]

# 
# all8Sets[4479]
# [[1]]
# [1]  2  4  8 11 13 14 17 19
#183 long, ends 6-13
long8Set=c(2,4,8,11,13,14,17,19)
long8=filterToSet(long8Set)
long8fit=fitSimpleModels(long8)
long8Smooth=smoothByWindow(long8,10)
long8SmoothFit=fitSimpleModels(long8Smooth)

plotSet(long8)
plot(long8fit$day,long8fit$elev_r2)
points(long8SmoothFit$day,long8SmoothFit$elev_r2,pch="*")


late8=filterToSet(all8Sets[11453])
late8fit=fitSimpleModels(late8)
late8Smooth=smoothByWindow(late8,10)
late8SmoothFit=fitSimpleModels(late8Smooth)
plotSet(late8)


longest8=all8Sets[11013]
longest8=c(4,7,8,11,13,14,17,19)
longest8Set=filterToSet(longest8)
plotSet(longest8Set)
longest8Fit=fitSimpleModels(longest8Set)
longest8Smooth=smoothByWindow(longest8Set,10)
longest8SmoothFit=fitSimpleModels(longest8Smooth)
