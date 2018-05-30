lakeMinAreas=c(10000)




pathAppend=paste0('min_','10000',"\\lakeFractionAndElevation.csv")
siteInfoPath=paste0("C:\\Users\\Sam\\Desktop\\spatial\\QgisEnvironment\\Active\\sitesLakeInfluence_tempPaper\\",pathAppend)
tempData=getDayData('2014-09-17',siteInfoPath)
linearLake_old=linearLake
linearLake=lm(mean~I(wtLakeElev*AboveLakeFrac)+AboveLakeFrac+I(Elevation*(1-AboveLakeFrac)),data=tempData,na.action=na.fail)
print(identical(linearLake_old,linearLake))
print(m)
print(summary(linearLake))
r=summary(linearLake)$r.squared
points(m,r)
