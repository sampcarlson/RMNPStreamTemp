fitStreamTemp=function(tempData){
  source('~/R Projects/RMNPStreamTemp/plotContext.R')
  plotContext(min(tempData$day),max(tempData$day))
  
  
  aggMeanFun=function(x){
    if (is.numeric(x)){
      return(mean(x,na.rm=T))
    } else {
      if(length(unique(x)==1)){
        return(x[1])
      } else { return("Multiple Records")} 
    }
  }
  
  tempData=aggregate(tempData,by=list(d=tempData$dep),FUN=aggMeanFun)
  
  require(MuMIn)
  km2_per_pixel=(9.14308^2)/(1000^2)
  tempData$uaa=tempData$uaa*km2_per_pixel
  
  #if no lakes are above a point, FlowWtMeanLakeElev comes through as a 0 - NaN is better
  #tempData$FlowWtMeanLakeElev[tempData$FlowWtMeanLakeElev==0]=NaN
  
  #elevation only fit
  elevFit = lm(tempData$Observation~tempData$Elevation)
  summary(elevFit)
  
  #uaa only fit
  uaaFit = lm (tempData$Observation~tempData$uaa)
  summary(uaaFit)
  
  
  #lake influence only fit
  lakeFit = lm(tempData$Observation~tempData$LF)
  summary(lakeFit)
  
  
  #lake elevation only fit
  lakeElevFit=lm(tempData$Observation~tempData$FlowWtMeanLakeElev)
  summary(lakeElevFit)
  
  #plot(tempData$FlowWtMeanLakeElev,tempData$LF)
  summary(lm(tempData$LF~tempData$FlowWtMeanLakeElev))
  
  
  #############------individual predictors plot------------
  # png(file='C:/Users/Sam/Dropbox/RMNPTemperaturePaper/individual_predictors.png',width=1000,height=1000,res=220)
  # par(oma=c(2,1,4,1))
  # par(mar=c(4.5,4.5,0,0))
  # layout(matrix(c(5,1,2,5,3,4),nrow=3),heights = c(0.08,1,1))
  # par(font.lab=2)
  # 
  # 
  # plot(tempData$Elevation,tempData$Observation,xlab='',ylab='',pch=19,lwd.ticks=2,ylim=c(7,11))
  # title(font=2,main='',xlab='Stream Elevation (m)',ylab=expression(bold(paste("Mean Temperature (",degree,"C)"))),line=2.2)
  # r=round(summary(elevFit)$r.squared[1],2)
  # p=round(summary(elevFit)$coefficients[2,4],2)
  # if(p<=0.05){
  #   abline(elevFit)
  #   #text(x=2700,y=7.5,labels=bquote(T==.(round(elevFit$coefficients[1],2)) ~ .(round(elevFit$coefficients[2],3))*E[S]),adj=0)
  # }
  # if(p<0.001){
  #   p='< 0.001'
  # }else{
  #   p=paste('=',p)
  # }
  # text(x=2700,y=8.5,labels=bquote(R^2 == .(r)),adj=0)
  # text(x=2700,y=8.0,labels=paste('p ',p),adj=0)
  # text(x=2625,y=10.8,'a',font=2,cex=1.1)
  # 
  # 
  # plot(tempData$LF,tempData$Observation,xlab='',ylab='',pch=19,lwd.ticks=2,xlim=c(0.2,1),ylim=c(7,11))
  # title(xlab=expression(bolditalic('LF')), ylab=expression(bold(paste("Mean Temperature (",degree,"C)"))),line=2.2)
  # r=round(summary(lakeFit)$r.squared[1],2)
  # p=round(summary(lakeFit)$coefficients[2,4],2)
  # if(p<=0.05){
  #   abline(lakeFit)
  # }
  # if(p<0.001){
  #   p='< 0.001'
  # }else{
  #   p=paste('=',p)
  # }
  # text(x=0.48,y=8.5,labels=bquote(R^2 == .(r)),adj=0)
  # text(x=0.48,y=8,labels=paste('p ',p),adj=0)
  # text(x=0.22,y=10.8,'c',font=2,cex=1.1)
  # 
  # par(mar=c(4.5,1.5,0,3))
  # 
  # plot(tempData$uaa,tempData$Observation,xlab='',ylab='',yaxt='n',xlim=c(0,83),pch=19,lwd.ticks=2,ylim=c(7,11))
  # axis(side=2,at=c(7,8,9,10,11),labels=rep('',5),lwd.tick=2)
  # title(xlab=expression(bold(paste('uaa'[s],' (km'^2*')'))),ylab=expression(bold(paste("Mean Temperature (",degree,"C)"))),line=2.2)
  # r=round(summary(uaaFit)$r.squared[1],2)
  # p=round(summary(uaaFit)$coefficients[2,4],2)
  # if(p<=0.05){
  #   abline(uaaFit)
  # }
  # if(p<0.001){
  #   p='< 0.001'
  # }else{
  #   p=paste('=',p)
  # }
  # text(x=40,y=8.5,labels=bquote(R^2 == .(r)),adj=0)
  # text(x=40,y=8,labels=paste('p ',p),adj=0)
  # text(x=5,y=10.8,'b',font=2,cex=1.1)
  # 
  # 
  # plot(tempData$FlowWtMeanLakeElev,tempData$Observation,xlab='',ylab='',yaxt='n',pch=19,lwd.ticks=2,ylim=c(7,11))
  # axis(side=2,at=c(7,8,9,10,11),labels=rep('',5),lwd.tick=2)
  # title(xlab=expression(paste(bolditalic(bar('E')[L]),bold(' (m)'))),ylab=expression(bold(paste("Mean Temperature (",degree,"C)"))),line=2.2)
  # r=round(summary(lakeElevFit)$r.squared[1],2)
  # p=round(summary(lakeElevFit)$coefficients[2,4],2)
  # if(p<=0.05){
  #   abline(lakeElevFit)
  # }
  # if(p<0.001){
  #   p='< 0.001'
  # }else{
  #   p=paste('=',p)
  # }
  # text(x=3120,y=8.5,labels=bquote(R^2 == .(r)),adj=0)
  # text(x=3120,y=8,labels=paste('p ',p),adj=0)
  # text(x=3075,y=10.8,'d',font=2,cex=1.1)
  # 
  # par(mar=c(0,0,0,0))
  # plot.new()
  # mtext("Single Predictors of Daily Mean Stream Temperature",side=3,font=2,cex=0.9)
  # dev.off()
  
  
  
  ###############-----------multi-term regressions-------------
  
  #linear elev and uaa:
  streamElevuaa=lm(Observation~Elevation+uaa,data=tempData,na.action=na.fail)
  summary(streamElevuaa)
  
  
  
  #combined fit:
  weightedEl_Es_Us=nls(formula=as.formula(Observation~LF*(Bl+Al*FlowWtMeanLakeElev)+(1-LF)*(Bs+As*Elevation+Au*uaa)),
                       data=tempData,start=list(Bl=0,Al=0,Bs=0,As=0,Au=0),na.action=na.fail)
  summary(weightedEl_Es_Us)
  
  
  weightedEl_Es=nls(formula=as.formula(Observation~LF*(Bl+Al*FlowWtMeanLakeElev)+(1-LF)*(Bs+As*Elevation)),
                    data=tempData,start=list(Bl=0,Al=0,Bs=0,As=0),na.action=na.fail)
  summary(weightedEl_Es)
  
  
  weightedEl_Us=nls(formula=as.formula(Observation~LF*(Bl+Al*FlowWtMeanLakeElev)+(1-LF)*(Bs+Au*uaa)),
                    data=tempData,start=list(Bl=0,Al=0,Bs=0,Au=0),na.action=na.fail)
  summary(weightedEl_Us)
  
  
  #flat regressions-------------------
  #all
  flat_Lf_El_Es_Us = lm(Observation~LF+FlowWtMeanLakeElev+Elevation+uaa,data=tempData)
  summary(flat_Lf_El_Es_Us)
  
  #minus one:
  flat_Lf_El_Es = lm(Observation~LF+FlowWtMeanLakeElev+Elevation,data=tempData)
  summary(flat_Lf_El_Es)
  
  flat_Lf_El_Us = lm(Observation~LF+FlowWtMeanLakeElev+uaa,data=tempData)
  summary(flat_Lf_El_Us)
  
  flat_Lf_Es_Us = lm(Observation~LF+Elevation+uaa,data=tempData)
  summary(flat_Lf_Es_Us)
  
  flat_El_Es_Us = lm(Observation~FlowWtMeanLakeElev+Elevation+uaa,data=tempData)
  summary(flat_El_Es_Us)
  
  
  #minus two
  flat_Lf_El = lm(Observation~LF+FlowWtMeanLakeElev,data=tempData)
  summary(flat_Lf_El)
  
  flat_Lf_Us = lm(Observation~LF+uaa,data=tempData)
  summary(flat_Lf_Us)
  
  flat_Lf_Es = lm(Observation~LF+Elevation,data=tempData)
  summary(flat_Lf_Es)
  
  flat_El_Es = lm(Observation~FlowWtMeanLakeElev+Elevation,data=tempData)
  summary(flat_El_Es)
  
  flat_El_Us = lm(Observation~FlowWtMeanLakeElev+uaa,data=tempData)
  summary(flat_El_Us)
  
  
  
  #plot(tempData$Observation,nonLinearCombined$m$fitted())
  
  #linearized lake form
  linearLake=lm(Observation~I(FlowWtMeanLakeElev*LF)+LF+I(Elevation*(1-LF))+I(uaa*(1-LF)),data=tempData,na.action=na.fail)
  summary(linearLake)
  # plot(tempData$Observation,fitted(linearLake))
  # abline(a=0,b=1)
  #dredge(linearLake)
  
  #linearized lake form without uaa
  linearLake_nouaa=lm(Observation~I(FlowWtMeanLakeElev*LF)+LF+I(Elevation*(1-LF)),data=tempData,na.action=na.fail)
  summary(linearLake_nouaa)
  # png(file='C:/Users/Sam/Dropbox/RMNPTemperaturePaper/PredictObserve.png',width=1000,height=1100,res=270 )
  # plot(tempData$Observation,fitted(linearLake_nouaa),xlab='Observation', ylab='Model Prediction', 
  #      main='Predicted and Observed Stream Temperature',cex.main=0.9,pch=16,xlim=c(7.5,10.7),ylim=c(7.5,10.7) )
  # points(tempData$Observation,fitted(lakeElevFit),pch=16,col='red')
  # abline(a=0,b=1,lty=2)
  # dev.off()
  
  #linearized lake form without elevation
  linearLake_noElev=lm(Observation~I(FlowWtMeanLakeElev*LF)+LF+I(uaa*(1-LF)),data=tempData,na.action=na.fail)
  summary(linearLake_noElev)
  
  
  #make the 'global'model huge and redundant for valid aic comparison:
  linearCombined=glm(Observation~I(FlowWtMeanLakeElev*LF)+LF+I(Elevation*(1-LF))+I(uaa*(1-LF))+Elevation+uaa+FlowWtMeanLakeElev,data=tempData,na.action=na.fail)
  summary(linearCombined)
  
  
  d=dredge(linearCombined,m.lim=c(1,6),
           subset=(!("Elevation" && "I(Elevation * (1 - LF))")
                   && !("FlowWtMeanLakeElev" && "I(FlowWtMeanLakeElev * LF)")),
           extra='R^2')
  
  d$`R^2`=round(d$`R^2`,7)
  d$internalModelID=row.names(d)
  
  #8 hypothesized equations: one term: (streamElev, streamuaa, lakeElev, Fuaa_L), 
  #multi term: (streamElev+streamuaa, FuaaL(El)+FuaaS(Es), FuaaL(El)+FuaaS(Us), FuaaL(El)+FuaaS(Es+Us))
  
  r2_nls=function(nlsName){
    return(summary(lm(tempData$Observation~fitted(nlsName)))$r.squared)
  }
  
  modelNames=data.frame(name=c('elevFit','uaaFit','lakeElevFit','lakeFit','streamElevuaa','weightedEl_Es','weightedEl_Us','weightedEl_Es_Us','flat_El_Es','flat_El_Es_Us'
                               ,'flat_El_Us','flat_Lf_El','flat_Lf_El_Es','flat_Lf_El_Es_Us','flat_Lf_El_Us','flat_Lf_Es','flat_Lf_Es_Us','flat_Lf_Us'),internalModelID=0,stringsAsFactors = F)
  
  modelNames$internalModelID[modelNames$name=='elevFit']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='uaaFit']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ uaa + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='lakeElevFit']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ FlowWtMeanLakeElev + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='lakeFit']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ LF + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='streamElevuaa']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + uaa + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='weightedEl_Es']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ I(Elevation * (1 - LF)) + I(FlowWtMeanLakeElev * LF) + LF + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='weightedEl_Us']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ I(FlowWtMeanLakeElev * LF) + LF + I(uaa * (1 - LF)) + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='weightedEl_Es_Us']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ I(Elevation * (1 - LF)) + I(FlowWtMeanLakeElev * LF) + LF + I(uaa * (1 - LF)) + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_El_Es']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + FlowWtMeanLakeElev + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_El_Es_Us']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + FlowWtMeanLakeElev + uaa + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_El_Us']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ FlowWtMeanLakeElev + uaa + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_Lf_El']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ FlowWtMeanLakeElev + LF + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_Lf_El_Es']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + FlowWtMeanLakeElev + LF + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_Lf_El_Es_Us']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + FlowWtMeanLakeElev + LF + uaa + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_Lf_El_Us']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ FlowWtMeanLakeElev + LF + uaa + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_Lf_Es']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + LF + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_Lf_Es_Us']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ Elevation + LF + uaa + 1, data = tempData, na.action = na.fail)"]
  modelNames$internalModelID[modelNames$name=='flat_Lf_Us']=
    d$internalModelID[as.character(attributes(d)$model.calls)=="glm(formula = Observation ~ LF + uaa + 1, data = tempData, na.action = na.fail)"]

  d = d[d$internalModelID %in% modelNames$internalModelID]
  display=data.frame(internalModelID=d$internalModelID,r_2=d$`R^2`,AICc=d$AICc,deltaAICc=d$delta)
  display=merge(x=display,y=modelNames,by='internalModelID',all.x=TRUE,sort=FALSE)[,c('name','r_2','AICc','deltaAICc')]
 
  return(display)
}