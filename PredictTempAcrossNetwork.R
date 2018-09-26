library(tidyverse)
#predict across NSV and GV
q_data=read.csv("C:\\Users\\Sam\\Desktop\\spatial\\QgisEnvironment\\Active\\sitesLakeInfluence_tempPaper2\\min_10000\\LakeFractionAndElevation.csv",header = F)

names(q_data)=c("SegIDX","LF","EL")
q_data=inner_join(q_data,
                 read.csv("C:\\Users\\Sam\\Desktop\\spatial\\QgisEnvironment\\Active\\sitesLakeInfluence_tempPaper2\\Inputs\\NSV_GC_AllSegPoints.csv"))

q_data=aggregate(q_data,by=list(SegIDX=q_data$SegIDX),FUN=mean)

predictStreamTemp=function(segData,a_Es=-0.002403,a_El=-0.009402,b_star=23.0200,bs=15.910){
  segData$Temperature_Predicted=0
  for(i in 1:nrow(segData)){
    segData$Temperature_Predicted[i]=a_El*(segData[i,]$LF*segData[i,]$EL)+b_star*(segData[i,]$LF)+a_Es*((1-segData[i,]$LF)*segData[i,]$Es)+bs
  }
  return(segData)
}

q_data=predictStreamTemp(q_data)
write.csv(q_data,"predictedTemperature.csv")
fit
hist(q_data$LF)
plot(q_data$Es,q_data$Temperature_Predicted)
