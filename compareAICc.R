source('~/R Projects/RMNPStreamTemp/tempFitFunctions.R')
tempData=buildTempData()

# day from previous analysis
tempData0=tempData[tempData$DateTimeLocal==as.Date("2014-09-20"),]
fitStreamTemp(tempData0)
#weighted_el_es is the most efficient model, and explains 81% of total variation
#LF is best single explanatory variable, @ 0.5

#period around 9-20 - unavoidable (small) rain event in any 10 day period
tempData1=tempData[tempData$DateTimeLocal>=as.Date("2014-09-18") & tempData$DateTimeLocal<=as.Date("2014-9-28"),]
fitStreamTemp(tempData1)
#weighted_el_es is the most efficient model, and explains 71% of total variation
#LF is best single explanatory variable, @ 0.35

#shorter period around 9-20, , most data w/ no rain.  overwrites 10 day period
tempData1=tempData[tempData$DateTimeLocal>=as.Date("2014-9-17") & tempData$DateTimeLocal<=as.Date("2014-9-21"),]
fitStreamTemp(tempData1)
#weighted_el_es is the most efficient model, and explains 78% of total variation
#LF is best single explanatory variable, @ 0.45

#earliest 10 day period w/ 16 sites
tempData2=tempData[tempData$DateTimeLocal>=as.Date("2014-08-21") & tempData$DateTimeLocal<=as.Date("2014-8-31"),]
fitStreamTemp(tempData2)
#weighted_el_es is the most efficient model, and explains 62% of total variation
#LF is best single explanatory variable, @ 0.19

#earliest 5 day period w/ 13 sites
tempData3=tempData[tempData$DateTimeLocal>=as.Date("2014-08-13") & tempData$DateTimeLocal<=as.Date("2014-08-18"),]
fitStreamTemp(tempData3,T)
#El is the best model, explaining 73%
#weighted el_es is much worse, explaining 59%

#latest pre-freeze data
tempData4=tempData[tempData$DateTimeLocal>=as.Date("2014-10-13") & tempData$DateTimeLocal<=as.Date("2014-10-23"),]
fitStreamTemp(tempData4, T)
#weightedel_es is best, @ 0.66
#stream elev is second best, @ 0.25

#winter period
tempData5=tempData[tempData$DateTimeLocal>=as.Date("2015-1-10") & tempData$DateTimeLocal<=as.Date("2015-1-20"),]
unique(tempData5$SiteIDX)
fitStreamTemp(tempData5,T)
#stream temp varies from 0 to ~0.8 across 11 sites
#LF is best model, though r2 = only 0.31
#weighted models are terrible

#early - beginning to melt.  temp varies from 0 to ~5.  n=13
tempData6=tempData[tempData$DateTimeLocal>=as.Date("2015-3-28") & tempData$DateTimeLocal<=as.Date("2015-4-4"),]
fitStreamTemp(tempData6,T)
#all models are horrible

#early - pre runoff.  4 day period w/ 13 sites, flow ~ 115 cfs
tempData7=tempData[tempData$DateTimeLocal>=as.Date("2015-5-2") & tempData$DateTimeLocal<=as.Date("2015-5-6"),]
fitStreamTemp(tempData7,T)
#stream elev is best (r2=0.41), stream elev+uaa is good too (r2=0.49)

#main runoff  n = 9, flow ~ 500
tempData8=tempData[tempData$DateTimeLocal>=as.Date("2015-6-1") & tempData$DateTimeLocal<=as.Date("2015-6-11"),]
fitStreamTemp(tempData8,T)
#stream elev alond is best model (r2=0.50), stream elev + lake elev is ok too.
#weighted models are horrible, but n is kinda small
unique(tempData8$SiteIDX)



highlight=data.frame(start=c("2014-09-17","2014-08-21","2014-08-13","2014-10-13","2015-01-10","2015-03-28","2015-05-02","2015-06-01"),
                     end = c("2014-09-28","2014-08-31","2014-08-18","2014-10-23","2015-01-20","2015-04-04","2015-05-06","2015-06-11"))

makeTsPlot(highlight=highlight)


set16=filterToSet(c(2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,19))
set16_singleFit=fitSimpleModels(set16)
set16_summary=fitAllModels(set16)
plot(set16_summary$day,set16_summary$`Fl(Es_El)_dAIC`,ylim=c(5,0),type="l",lwd=2)
lines(set16_summary$day,set16_summary$Es_dAIC,lty=2)
lines(set16_summary$day,set16_summary$Fl_dAIC,lty=3)


smooth16=smoothByWindow(set16,5)
smooth16_singleFit=fitSimpleModels(smooth16)
smooth16_summary=fitAllModels(smooth16)
plot(smooth16_summary$day,smooth16_summary$`Fl(Es_El)_dAIC`,ylim=c(5,0),type="l")
lines(smooth16_summary$day,smooth16_summary$Es_dAIC,lty=2)

makeTsPlot(startDate=as.Date("2014-09-20"),endDate=as.Date("2014-10-20"))
