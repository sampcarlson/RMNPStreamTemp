source('~/R Projects/RMNPStreamTemp/tempFitFunctions.R')
tempData=buildTempData()

#for debug
#tempData=tempData[1000:1200,]

tempData_lake=tempData[tempData$SiteIDX!=19,]# drop glacier knobs (site w/ no LoI)

#setsDF=getAllSets(tempData_lake)


all8Sets=combn(unique(tempData_lake$SiteIDX),8,simplify=F)

set8Info=data.frame(id=1:length(all8Sets))

#small dataset for testing


#this takes ~2 hours
set8Info$length=sapply(all8Sets,setLength)
set8Info=set8Info[order(set8Info$length, decreasing = T),]
set8Info

#pick the n longest sets
set8_long=set8Info[1:200,]

plotMultipleSets(set8_long$id)
