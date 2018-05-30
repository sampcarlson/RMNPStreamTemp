
startDate = as.Date("2014-08-1")
numLoggers=data.frame(day=NA,number=NA)
for(i in 1:65){
  day=startDate+i
  result=getDayData(as.character(day),1)
  if (!is.null(result)){
  numLoggers=rbind(numLoggers,data.frame(day=i,number=nrow(result)))
  }
  plot(startDate+numLoggers$day,numLoggers$number)
  print(i)
}

