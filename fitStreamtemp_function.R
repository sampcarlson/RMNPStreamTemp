fitStreamTemp=function(tempData, plotContext=F){

  
  #get set of sites which are consistent across whole period
  n_all=length(unique(tempData$SiteIDX))
  allSites=unique(tempData$SiteIDX)
  numDays=length(unique(tempData$day))
  daysThisSite=function(Site){
    dayCount=length(unique(tempData$day[tempData$SiteIDX==Site]))
    return(dayCount)
  }
  fullSites=allSites[sapply(allSites,daysThisSite)==numDays]
  tempData=tempData[tempData$SiteIDX%in%fullSites,]
  n=length(unique(tempData$SiteIDX))
  
  if(n == n_all){
  print(paste("n =",n))
  } else {
    print(paste0("n = ",n,", ",n_all-n," incomplete observations removed"))
  }
  #plot
  if(plotContext){
    source('~/R Projects/RMNPStreamTemp/timeseriesFigure.R')
    makeTsPlot(min(tempData$day),max(tempData$day))
  }
  
  
  aggMeanFun=function(x){
    if (is.numeric(x)){
      return(mean(x,na.rm=T))
    } else {
      if(length(unique(x)==1)){
        return(x[1])
      } else { return("Multiple Records")} 
    }
  }
  
  tempData=aggregate(tempData,by=list(s=tempData$site),FUN=aggMeanFun)
  
  require(MuMIn)
  km2_per_pixel=(9.14308^2)/(1000^2)
  tempData$uaa=tempData$uaa*km2_per_pixel
  
  #make the 'global'model huge (and redundant) for aic comparison:
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