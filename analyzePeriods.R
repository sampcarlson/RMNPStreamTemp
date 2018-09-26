source('~/R Projects/RMNPStreamTemp/tempFitFunctions.R')
tempData=buildTempData()
library(xtable)

makeTsPlot(highlight=data.frame(start=c("2014-8-24","2014-10-7","2015-3-28","2015-5-2"),end=c("2014-8-28","2014-10-11","2015-4-1","2015-5-6")))


getEqnBody=function(df){
  getRef=function(rName){
    ref=switch(rName,
               "Fl(Es_El)"="weighted_El_Es",
               "Fl(Us_El)"="weighted_El_Us",
               "Fl(Es_Us_El)"="weighted_El_Es_Us",
               "Es"="Single_StreamElev",
               "Fl"="Single_LakeFraction",
               "Es_Fl"="unweighted_LF_Es",
               "Us"="Single_StreamUaa",
               "Es_Us"="unweighted_Us_Es",
               "Us_Fl"="unweighted_LF_Us",
               "Es_Us_Fl"="unweighted_LF_Us_Es")
    return(paste0("(\\ref{eq:",ref,"})"))
  }   
  
  
  getEqn=function(rName){
    eqn=switch(rName,
               "Fl(Es_El)"="$T_s = LF(a_{el}\\bar{E_l}+b_l) + (1-LF)(a_{es}E_s+b_s)$",
               "Fl(Us_El)"="$T_s = LF(a_{el}\\bar{E_l}+b_l) + (1-LF)(a_{us}UAA_s+b_s)$",
               "Fl(Es_Us_El)"="$T_s = LF(a_{el}\\bar{E_l}+b_l) + (1-LF)(a_{es}E_s+a_{us}UAA_s+b_s)$",
               "Es"="$T_s=a_{es}E_s+b$",
               "Fl"="$T_s=a_{fl}LF + b$",
               "Es_Fl"="$T_s=a_{fl}LF+a_{es}E_s+b$",
               "Us"="$T_s=a_{us}UAA_s+b$",
               "Es_Us"="$T_s=a_{us}UAA_s+a_{es}E_s+b$",
               "Us_Fl"="$T_s=a_{fl}LF+a_{us}UAA_s+b$",
               "Es_Us_Fl"="$T_s=a_{fl}LF+a_{us}UAA_s+a_{es}E_s+b$")
    return(eqn)
  }
  
  df$refName=sapply(df$name,getRef)
  df$eqnBody=sapply(df$name,getEqn)
  df=df[,c("refName","eqnBody","r_2","dAICc_0")]
  names(df)=c(" ","Equation","$r^2$","$\\Delta AICc$")
  return(df)
}
makeTexTable=function(period){
  df=fitStreamTemp(period)
  df_more=getEqnBody(df)
  print(xtable(df_more,align=c("r","r","l","r","r")),sanitize.text.function=identity,include.rownames = F)  
}

#n=17, Fl(Es_El) wins
period1=tempData[tempData$day>=as.Date("2014-8-24") & tempData$day<=as.Date("2014-8-28"),]
fit=fitStreamTemp(period1)
summary(fit)
makeTexTable(period1)

#n=16, Fl(Es_El) wins
period2=tempData[tempData$day>=as.Date("2014-10-7") & tempData$day<=as.Date("2014-10-11"),]
makeTexTable(period2)

#n=13, Es wins, very low r2
period3=tempData[tempData$day>=as.Date("2015-3-28") & tempData$day<=as.Date("2015-4-1"),]
makeTexTable(period3)

#n=13, Es wins
period4=tempData[tempData$day>=as.Date("2015-5-2") & tempData$day<=as.Date("2015-5-6"),]
makeTexTable(period4)

#best fit individual day
#period5=tempData[tempData$day==as.Date("2014-9-20"),]
#makeTexTable(period5)

plotIndividualPredictors=function(startDate,endDate){
  #filter tempData by start dates, aggregate to have mean temps
  
  png(filename="period1Plot.png",width = 4, height = 6,units = "in", res=300)
  par(oma=c(2,1,4,1))
  par(mar=c(4.5,4.5,0,0))
  layout(matrix(c(1,2,3,4),nrow=2))
  par(font.lab=2)
  
  
  plot(tempData$Elevation,tempData$Observation,xlab='',ylab='',pch=19,lwd.ticks=2,ylim=c(7,11))
  title(font=2,main='',xlab='Stream Elevation (m)',ylab=expression(bold(paste("Mean Temperature (",degree,"C)"))),line=2.2)
  r=round(summary(elevFit)$r.squared[1],2)
  p=round(summary(elevFit)$coefficients[2,4],2)
  if(p<=0.05){
    abline(elevFit)
    #text(x=2700,y=7.5,labels=bquote(T==.(round(elevFit$coefficients[1],2)) ~ .(round(elevFit$coefficients[2],3))*E[S]),adj=0)
  }
  if(p<0.001){
    p='< 0.001'
  }else{
    p=paste('=',p)
  }
  text(x=2700,y=8.5,labels=bquote(R^2 == .(r)),adj=0)
  text(x=2700,y=8.0,labels=paste('p ',p),adj=0)
  text(x=2625,y=10.8,'a',font=2,cex=1.1)
  
  
  plot(tempData$LF,tempData$Observation,xlab='',ylab='',pch=19,lwd.ticks=2,xlim=c(0.2,1),ylim=c(7,11))
  title(xlab=expression(bolditalic('LF')), ylab=expression(bold(paste("Mean Temperature (",degree,"C)"))),line=2.2)
  r=round(summary(lakeFit)$r.squared[1],2)
  p=round(summary(lakeFit)$coefficients[2,4],2)
  if(p<=0.05){
    abline(lakeFit)
  }
  if(p<0.001){
    p='< 0.001'
  }else{
    p=paste('=',p)
  }
  text(x=0.48,y=8.5,labels=bquote(R^2 == .(r)),adj=0)
  text(x=0.48,y=8,labels=paste('p ',p),adj=0)
  text(x=0.22,y=10.8,'c',font=2,cex=1.1)
  
  par(mar=c(4.5,1.5,0,3))
  
  plot(tempData$Uaa,tempData$Observation,xlab='',ylab='',yaxt='n',xlim=c(0,83),pch=19,lwd.ticks=2,ylim=c(7,11))
  axis(side=2,at=c(7,8,9,10,11),labels=rep('',5),lwd.tick=2)
  title(xlab=expression(bold(paste('UAA'[s],' (km'^2*')'))),ylab=expression(bold(paste("Mean Temperature (",degree,"C)"))),line=2.2)
  r=round(summary(uaaFit)$r.squared[1],2)
  p=round(summary(uaaFit)$coefficients[2,4],2)
  if(p<=0.05){
    abline(uaaFit)
  }
  if(p<0.001){
    p='< 0.001'
  }else{
    p=paste('=',p)
  }
  text(x=40,y=8.5,labels=bquote(R^2 == .(r)),adj=0)
  text(x=40,y=8,labels=paste('p ',p),adj=0)
  text(x=5,y=10.8,'b',font=2,cex=1.1)
  
  
  plot(tempData$FlowWtMeanLakeElev,tempData$Observation,xlab='',ylab='',yaxt='n',pch=19,lwd.ticks=2,ylim=c(7,11))
  axis(side=2,at=c(7,8,9,10,11),labels=rep('',5),lwd.tick=2)
  title(xlab=expression(paste(bolditalic(bar('E')[L]),bold(' (m)'))),ylab=expression(bold(paste("Mean Temperature (",degree,"C)"))),line=2.2)
  r=round(summary(lakeElevFit)$r.squared[1],2)
  p=round(summary(lakeElevFit)$coefficients[2,4],2)
  if(p<=0.05){
    abline(lakeElevFit)
  }
  if(p<0.001){
    p='< 0.001'
  }else{
    p=paste('=',p)
  }
  text(x=3120,y=8.5,labels=bquote(R^2 == .(r)),adj=0)
  text(x=3120,y=8,labels=paste('p ',p),adj=0)
  text(x=3075,y=10.8,'d',font=2,cex=1.1)
  
  dev.off()
}


f=flow[flow$Date>=as.Date("2014-8-24")&flow$Date<=as.Date("2014-8-28"),]
mean(f$Flow,na.rm = T)


t=temp[temp$date>=as.Date("2014-8-24")&temp$date<=as.Date("2014-8-28"),]
mean(t$airtemp_avg,na.rm=T)


