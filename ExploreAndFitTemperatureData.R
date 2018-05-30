require(MuMIn)
temperatureDate='2014-09-17'
tempData=getDayData(temperatureDate,1)
#initial look at data

plot(tempData$Elevation,tempData$mean)
plot(tempData$AboveLakeFrac,tempData$mean)
plot(tempData$Uaa,tempData$mean)

#elevation only fit
elevFit = lm(tempData$mean~tempData$Elevation)
summary(elevFit)
plot(tempData$mean,fitted(elevFit))

#lake only fit
lakeFit = lm(tempData$mean~tempData$AboveLakeFrac)
summary(lakeFit)
plot(tempData$mean,fitted(lakeFit))


#combined fit:
nonLinearCombined=nls(formula=as.formula(mean~AboveLakeFrac*(Bl+Al*wtLakeElev)+(1-AboveLakeFrac)*(Bs+As*Elevation)),
    data=tempData,start=list(Bl=0,Al=0,Bs=0,As=0))
summary(nonLinearCombined)


#linearized lake form
linearLake=lm(mean~I(wtLakeElev*AboveLakeFrac)+AboveLakeFrac+I(Elevation*(1-AboveLakeFrac)),data=tempData,na.action=na.fail)
summary(linearLake)
plot(tempData$mean,fitted(linearLake))
dredge(linearLake)



#linearized lake form without stream elevation coefficient
linearLake=lm(mean~I(wtLakeElev*AboveLakeFrac)+AboveLakeFrac,data=tempData,na.action=na.fail)
summary(linearLake)
plot(tempData$mean,fitted(linearLake))
dredge(linearLake)



#make the 'global'model huge and redundant for valid aic comparison:
linearCombined=lm(mean~I(wtLakeElev*AboveLakeFrac)+AboveLakeFrac+I(Elevation*(1-AboveLakeFrac))+Elevation+Uaa,data=tempData,na.action=na.fail)
summary(linearCombined)

dredge(linearCombined,m.lim=c(1,3))
