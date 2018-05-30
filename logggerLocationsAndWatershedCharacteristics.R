basePath='C:/Users/Sam/Desktop/spatial/QgisEnvironment/Active/sitesLakeInfluence_tempPaper/Inputs/'
streamPoints=read.csv(paste0(basePath,'streamPoints.csv'))


pathAppend=paste0('min_','10000',"\\lakeFractionAndElevation.csv")
siteInfoPath=paste0("C:\\Users\\Sam\\Desktop\\spatial\\QgisEnvironment\\Active\\sitesLakeInfluence_tempPaper\\",pathAppend)

day='2014-09-20'

allTempData = getDayData(day,siteInfoPath)

#allTempData = getDayData(day,1)

selectData=allTempData[allTempData$max<15,] 
tempData=selectData[!selectData$depIDX%in%c(8,19,20),] #remove these

km2_per_pixel=(9.14308^2)/(1000^2)
streamPoints$UAA_km2=streamPoints$UAA*km2_per_pixel
tempData$UAA_km2=tempData$Uaa*km2_per_pixel
streamPoints=streamPoints[streamPoints$UAA_km2>=1,]

png(file='C:/Users/Sam/Dropbox/RMNPTemperaturePaper/UaaElevFig.png',width=1000,height=1000,res=200)


plot(streamPoints$UAA_km2,streamPoints$Elev,pch=16,cex=0.7,col='gray60',
     xlab=expression(bold(paste('Upstream Accumulated Area ', (km^2)))),ylab=expression(bold('Elevation (m)')))
points(tempData$UAA_km2,tempData$Elevation,pch=16,cex=0.7,col='gray60')
points(tempData$UAA_km2,tempData$Elevation,pch=1,cex=1.5)

legend(x='topright',legend=c('Stream Network Reaches','Sampling Points'),
       pch=c(16,1),col=c('gray60','black'),pt.cex=c(0.7,1.5),bty='n')

dev.off()
