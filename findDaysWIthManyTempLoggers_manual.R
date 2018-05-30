basePath='C:/Users/Sam/Desktop/spatial/QgisEnvironment/Active/sitesLakeInfluence_tempPaper/Inputs/'
streamPoints=read.csv(paste0(basePath,'streamPoints.csv'))

pathAppend=paste0('min_','10000',"\\lakeFractionAndElevation.csv")
siteInfoPath=paste0("C:\\Users\\Sam\\Desktop\\spatial\\QgisEnvironment\\Active\\sitesLakeInfluence_tempPaper\\",pathAppend)


day='2014-09-20'
allDayData = getDayData_manual(day,siteInfoPath,'data')



par(new=F)
first=TRUE
for(n in names(allDayData)){
  if(n!='DateTimeLocal'){
    if (first==FALSE){
      par(new=T)
    }
    if(max(allDayData[,names(allDayData)==n])<15){
      if(n%in%c('dep8','dep19','dep20')){
        plot(allDayData$DateTimeLocal,allDayData[,names(allDayData)==n],type='l',ylim=c(5,15),col='blue',lwd=2)
      }else{
        plot(allDayData$DateTimeLocal,allDayData[,names(allDayData)==n],type='l',ylim=c(5,15))
      }
      first=FALSE
    }
  }
}
tempData$depIDX

print(length(names(allDayData))-1)


allTempData=getDayData_manual(day,siteInfoPath,'summary')


selectData=allTempData[allTempData$mean<15,]

