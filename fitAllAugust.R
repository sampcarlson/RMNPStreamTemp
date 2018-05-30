require(MuMIn)
require(xtable)


tempData=read.csv("NSC_GC_TempData.csv")
#head(tempData)

tempData$month=months(as.Date(tempData$DateTimeLocal))
tempData$julian=julian.Date(as.Date(tempData$DateTimeLocal))
#tempDataAug=tempData[tempData$month=="August",]
tempDataAug=tempData[tempData$julian==16300,]

augMean=aggregate(tempDataAug,by=list(by=tempDataAug$DeploymentIDX),FUN=mean)
augMean=augMean[,names(augMean)%in%c("Observation","LF","FlowWtMeanLakeElev","Elevation","uaa")]
names(augMean)
names(augMean)=c("mean","AboveLakeFrac","wtLakeElev","Elevation","Uaa")



tempData=augMean



km2_per_pixel=(9.14308^2)/(1000^2)
tempData$Uaa=tempData$Uaa*km2_per_pixel

#if no lakes are above a point, wtLakeElev comes through as a 0 - NaN is better
#tempData$wtLakeElev[tempData$wtLakeElev==0]=NaN
tempData=tempData[complete.cases(tempData),]
#elevation only fit
elevFit = lm(tempData$mean~tempData$Elevation)
summary(elevFit)

#uaa only fit
uaaFit = lm (tempData$mean~tempData$Uaa)
summary(uaaFit)


#lake influence only fit
lakeFit = lm(tempData$mean~tempData$AboveLakeFrac)
summary(lakeFit)


#lake elevation only fit
lakeElevFit=lm(tempData$mean~tempData$wtLakeElev)
summary(lakeElevFit)

plot(tempData$wtLakeElev,tempData$AboveLakeFrac)
summary(lm(tempData$AboveLakeFrac~tempData$wtLakeElev))


#############------individual predictors plot------------
png(file='individual_predictors.png',width=1000,height=1000,res=220)
par(oma=c(2,1,4,1))
par(mar=c(4.5,4.5,0,0))
layout(matrix(c(5,1,2,5,3,4),nrow=3),heights = c(0.08,1,1))
par(font.lab=2)


plot(tempData$Elevation,tempData$mean,xlab='',ylab='',pch=19,lwd.ticks=2,ylim=c(7,12))
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


plot(tempData$AboveLakeFrac,tempData$mean,xlab='',ylab='',pch=19,lwd.ticks=2,xlim=c(0.2,1),ylim=c(7,12))
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

plot(tempData$Uaa,tempData$mean,xlab='',ylab='',yaxt='n',xlim=c(0,83),pch=19,lwd.ticks=2,ylim=c(7,12))
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


plot(tempData$wtLakeElev,tempData$mean,xlab='',ylab='',yaxt='n',pch=19,lwd.ticks=2,ylim=c(7,14))
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

par(mar=c(0,0,0,0))
plot.new()
mtext("Single Predictors of Daily Mean Stream Temperature",side=3,font=2,cex=0.9)
dev.off()



###############-----------multi-term regressions-------------

#linear elev and uaa:
streamElevUaa=lm(mean~Elevation+Uaa,data=tempData,na.action=na.fail)
summary(streamElevUaa)



#combined fit:
weightedEl_Es_Us=nls(formula=as.formula(mean~AboveLakeFrac*(Bl+Al*wtLakeElev)+(1-AboveLakeFrac)*(Bs+As*Elevation+Au*Uaa)),
                     data=tempData,start=list(Bl=0,Al=0,Bs=0,As=0,Au=0),na.action=na.fail)
summary(weightedEl_Es_Us)


weightedEl_Es=nls(formula=as.formula(mean~AboveLakeFrac*(Bl+Al*wtLakeElev)+(1-AboveLakeFrac)*(Bs+As*Elevation)),
                  data=tempData,start=list(Bl=0,Al=0,Bs=0,As=0),na.action=na.fail)
summary(weightedEl_Es)


weightedEl_Us=nls(formula=as.formula(mean~AboveLakeFrac*(Bl+Al*wtLakeElev)+(1-AboveLakeFrac)*(Bs+Au*Uaa)),
                  data=tempData,start=list(Bl=0,Al=0,Bs=0,Au=0),na.action=na.fail)
summary(weightedEl_Us)


#flat regressions-------------------
#all
flat_Lf_El_Es_Us = lm(mean~AboveLakeFrac+wtLakeElev+Elevation+Uaa,data=tempData)
summary(flat_Lf_El_Es_Us)

#minus one:
flat_Lf_El_Es = lm(mean~AboveLakeFrac+wtLakeElev+Elevation,data=tempData)
summary(flat_Lf_El_Es)

flat_Lf_El_Us = lm(mean~AboveLakeFrac+wtLakeElev+Uaa,data=tempData)
summary(flat_Lf_El_Us)

flat_Lf_Es_Us = lm(mean~AboveLakeFrac+Elevation+Uaa,data=tempData)
summary(flat_Lf_Es_Us)

flat_El_Es_Us = lm(mean~wtLakeElev+Elevation+Uaa,data=tempData)
summary(flat_El_Es_Us)


#minus two
flat_Lf_El = lm(mean~AboveLakeFrac+wtLakeElev,data=tempData)
summary(flat_Lf_El)

flat_Lf_Us = lm(mean~AboveLakeFrac+Uaa,data=tempData)
summary(flat_Lf_Us)

flat_Lf_Es = lm(mean~AboveLakeFrac+Elevation,data=tempData)
summary(flat_Lf_Es)

flat_El_Es = lm(mean~wtLakeElev+Elevation,data=tempData)
summary(flat_El_Es)

flat_El_Us = lm(mean~wtLakeElev+Uaa,data=tempData)
summary(flat_El_Us)



#plot(tempData$mean,nonLinearCombined$m$fitted())

#linearized lake form
linearLake=lm(mean~I(wtLakeElev*AboveLakeFrac)+AboveLakeFrac+I(Elevation*(1-AboveLakeFrac))+I(Uaa*(1-AboveLakeFrac)),data=tempData,na.action=na.fail)
summary(linearLake)
plot(tempData$mean,fitted(linearLake))
abline(a=0,b=1)
#dredge(linearLake)

#linearized lake form without uaa
linearLake_noUaa=lm(mean~I(wtLakeElev*AboveLakeFrac)+AboveLakeFrac+I(Elevation*(1-AboveLakeFrac)),data=tempData,na.action=na.fail)
summary(linearLake_noUaa)
png(file='C:/Users/Sam/Dropbox/RMNPTemperaturePaper/PredictObserve.png',width=1000,height=1100,res=270 )
plot(tempData$mean,fitted(linearLake_noUaa),xlab='Observation', ylab='Model Prediction', 
     main='Predicted and Observed Stream Temperature',cex.main=0.9,pch=16,xlim=c(7.5,10.7),ylim=c(7.5,10.7) )
points(tempData$mean,fitted(lakeElevFit),pch=16,col='red')
abline(a=0,b=1,lty=2)
dev.off()

#linearized lake form without elevation
linearLake_noElev=lm(mean~I(wtLakeElev*AboveLakeFrac)+AboveLakeFrac+I(Uaa*(1-AboveLakeFrac)),data=tempData,na.action=na.fail)
summary(linearLake_noElev)


#make the 'global'model huge and redundant for valid aic comparison:
linearCombined=glm(mean~I(wtLakeElev*AboveLakeFrac)+AboveLakeFrac+I(Elevation*(1-AboveLakeFrac))+I(Uaa*(1-AboveLakeFrac))+Elevation+Uaa+wtLakeElev,data=tempData,na.action=na.fail)
summary(linearCombined)


d=dredge(linearCombined,m.lim=c(1,6),
         subset=(!("Elevation" && "I(Elevation * (1 - AboveLakeFrac))")
                 && !("wtLakeElev" && "I(wtLakeElev * AboveLakeFrac)")),
         extra='R^2')

d$`R^2`=round(d$`R^2`,7)

#8 hypothesized equations: one term: (streamElev, streamUaa, lakeElev, FUAA_L), multi term: (streamElev+streamUAA, FuaaL(El)+FuaaS(Es), FuaaL(El)+FuaaS(Us), FuaaL(El)+FuaaS(Es+Us))
#the following is easy, but not robust.  check it after any changes
r2_nls=function(nlsName){
  return(summary(lm(tempData$mean~fitted(nlsName)))$r.squared)
}

modelNames=data.frame(name=c('elevFit','uaaFit','lakeElevFit','lakeFit','streamElevUaa','weightedEl_Es','weightedEl_Us','weightedEl_Es_Us','flat_El_Es','flat_El_Es_Us'
                             ,'flat_El_Us','flat_Lf_El','flat_Lf_El_Es','flat_Lf_El_Es_Us','flat_Lf_El_Us','flat_Lf_Es','flat_Lf_Es_Us','flat_Lf_Us'),stringsAsFactors = F)

modelNames$r=round(unlist(lapply(lapply(list(modelNames$name)[[1]],get),r2_nls)),7)

d_hyp = d[d$`R^2` %in% modelNames$r]


display=data.frame(r=d$`R^2`,AICc=d$AICc,deltaAICc=d$delta)
display=merge(x=display,y=modelNames,by='r',all.x=TRUE,sort=FALSE)

head(d_hyp)
