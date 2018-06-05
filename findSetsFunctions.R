aggMeanFun=function(x){
  if (is.numeric(x)){
    return(mean(x,na.rm=T))
  } else {
    if(length(unique(x)==1)){
      return(x[1])
    } else { return("Multiple Records")} 
  }
}

getSet=function(day,inDF){
  dayDF=inDF[inDF$day==day,]
  return(unique(dayDF$DeploymentIDX))
}

getID=function(set,uSets,uSetID){
  return(uSetID[uSets%in%list(s=set)])
}

countSets=function(targetSet,allSets){
  return(sum(allSets%in%list(t=targetSet)))
}

countSetsChar=function(targetSet,allSetsChar){
  targetSetChar=paste(targetSet,collapse=", ")
  return(sum(grepl(targetSetChar,allSetsChar)))
}