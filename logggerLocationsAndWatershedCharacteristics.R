basePath='C:/Users/Sam/Desktop/spatial/QgisEnvironment/Active/sitesLakeInfluence_tempPaper/Inputs/'
streamPoints=read.csv(paste0(basePath,'streamPoints.csv'))
km2_per_pixel=(9.14308^2)/(1000^2)
streamPoints$UAA_km2=streamPoints$UAA*km2_per_pixel
streamPoints=streamPoints[streamPoints$UAA_km2>=1,]
sites=aggregate(tempData,by=list(site=tempData$site),FUN=mean)




png(file='C:/Users/Sam/Dropbox/RMNPTemperaturePaper/UaaElevFig.png',width=1000,height=1000,res=200)


plot(streamPoints$UAA_km2,streamPoints$Elev,pch=16,cex=0.7,col='gray60',
     xlab=expression(bold(paste('Upstream Accumulated Area ', (km^2)))),ylab=expression(bold('Elevation (m)')))
points(sites$uaa,sites$Elevation,pch=16,cex=0.7,col='gray60')
points(sites$uaa,sites$Elevation,pch=1,cex=1.5)

legend(x='topright',legend=c('Stream Network Reaches','Sampling Points'),
       pch=c(16,1),col=c('gray60','black'),pt.cex=c(0.7,1.5),bty='n')

dev.off()
