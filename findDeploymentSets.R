source('~/R Projects/RMNPStreamTemp/findSetsFunctions.R')

#this comes from 'DataFrameForGeoff.R'
tempData=read.csv("~/R Projects/RMNPStreamTemp/NSC_GC_TempData.csv")

tempData_dayMean=aggregate(tempData[c("Observation","DeploymentIDX","SiteName","LF","FlowWtMeanLakeElev","Elevation","uaa")],by=list(dep=tempData$DeploymentIDX,day=tempData$DateTimeLocal),FUN=aggMeanFun)

#find 'sets' of deployments which are common across many days
min_n=15

days=unique(tempData_dayMean$day)

sets=lapply(days,FUN=getSet,inDF=tempData_dayMean)


uSets=unique(sets)
uSetID=1:length(uSets)

longSets=sapply(sets,length)>=min_n
setDF=data.frame(uSetID=sapply(sets, getID, uSets=uSets,uSetID=uSetID),day=days,many=longSets)
#longSets=unique(sets[sapply(sets,length)>=min_n])

#setCount=lapply(longSets,FUN=countSets,allSets=sets)
setDF$dayCountChar=unlist(lapply(sets,FUN=countSetsChar,allSetsChar=lapply(sets,FUN=paste,collapse=", ")))
#setDF$dayCount=unlist(lapply(sets,FUN=countSets,allSets=sets))
setDF=setDF[setDF$many==T,]
setDF$day=droplevels(setDF$day)
par(new=F)
plot(setDF$day,setDF$dayCountChar)


bestID=unique(setDF[setDF$dayCountChar==max(setDF$dayCountChar),c("uSetID")])
setDF[setDF$uSetID==bestID,]
unlist(uSets[bestID])

deps=unlist(uSets[bestID])
