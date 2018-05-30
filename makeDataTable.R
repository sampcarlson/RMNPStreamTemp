require(MuMIn)
require(xtable)

pathAppend=paste0('min_','10000',"\\lakeFractionAndElevation.csv")
siteInfoPath=paste0("C:\\Users\\Sam\\Desktop\\spatial\\QgisEnvironment\\Active\\sitesLakeInfluence_tempPaper\\",pathAppend)

basePath='C:/Users/Sam/Desktop/spatial/QgisEnvironment/Active/sitesLakeInfluence_tempPaper/Inputs/'
#streamPoints=read.csv(paste0(basePath,'streamPoints.csv'))

day='2014-09-20'
allTempData = getDayData(day,siteInfoPath)

spatial=read.csv('TempLogggerSpatialData.csv')
simpleSpatial=spatial[,names(spatial) %in% c('X','Y','field_1','field_2','field_5')]
names(simpleSpatial)=c('X','Y','SiteIDX','SiteName','note')
fatTable = merge(allTempData,simpleSpatial,by='SiteIDX')
final=fatTable[,names(fatTable) %in% c('mean','Elevation','Uaa','AboveLakeFrac','wtLakeElev','X','Y','note','SiteName.x')]
names(final)=c('SiteName','Daily Mean Temperature','Sampling Point Elevation','Sampling Point Uaa','LF','weighted mean LoI Elevation', 'X (EPSG:32613)','Y (EPSG:32613)','note')
final$note=as.character(final$note)
final$note[1]=c('NSV above Ouzel Creek')
final$note[2]=c('Ouzel Creek above NSV')
final$note[3]=c('NSV btw Cony and Hunters')
final$note[4]=c('Hunters Creek above NSV')
final$note[5]=c('NSV below Hunters Creek')
final$note[6]=c('Chaos Creek')
final$note[7]=c('Glacier Creek')
final$note[8]=c('NSV below Thunder Lake')
final$note[9]=c('Cony Creek')
final$note[10]=c('Hunters Creek high')
final$note[11]=c('Icy Brook')
final$note[12]=c('Glacier Creek high')
final$note[13]=c('Shelf Creek')
final$note[14]=c('Unnamed draining Glacier Knobs')


final[order(as.character(final$SiteName)),]

final=final[,-1]
final
xtable(final)
