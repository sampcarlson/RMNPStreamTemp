
plotDayPeriod=function(day,period){
  #plot function
  plotTempData=function(tempData,title){
    deps=left_join(dbGetQuery(TempDB,"SELECT DeploymentIDX, SiteIDX FROM Deployments"),
                   dbGetQuery(TempDB,"SELECT SiteIDX, SiteName FROM Sites"),by = "SiteIDX")
    thisDeps=unique(tempData$DeploymentIDX)
    deps_sites=left_join(data.frame(DeploymentIDX=thisDeps),deps,by = "DeploymentIDX")
    dataNames=deps_sites$DeploymentIDX
    ymin=max(min(tempData$Observation),-4)
    ymax=min(max(tempData$Observation)+1,20)
    
    xmin=min(tempData$DateTimeLocal)
    xmax=max(tempData$DateTimeLocal)
    
    f_pch=function(x){
      m=floor(x/25)
      x=x-(m*25)
      return(x)
    }
    require(RColorBrewer)
    pal=brewer.pal(n=8,name="Accent")
    pal=c(pal,brewer.pal(n=8,name="Set3"))
    pal=c(pal,brewer.pal(n=8,name="Dark2"))
    pal=c(pal,brewer.pal(n=8,name="Pastel1"))
    
    plot(as.POSIXct(tempData$DateTimeLocal),tempData$Observation, type='n',ylim=c(ymin,ymax),xlim=c(as.POSIXct(xmin),as.POSIXct(xmax)),main=title,xaxt='n')
    axis.POSIXct(side=1,x=tempData$DateTimeLocal,format = "%Y-%m-%d")
    lid=1
    for(d in thisDeps){
      thisDepData=tempData[tempData$DeploymentIDX==d,]
      lines(as.POSIXct(thisDepData$DateTimeLocal),thisDepData$Observation,type='o',lty=lid,pch=f_pch(lid),col=pal[lid],cex=thisDepData$Status,lwd=2)
      lid=lid+1
      abline(h=0)
    }
       lines(as.POSIXct(BigThom$DateTime),BigThom$Temperature,lwd=3,lty=1)
    legend(x="topright",legend=paste0(deps_sites$DeploymentIDX,"  (",deps_sites$SiteName,")"),lty=c(1:lid),lwd=2,pch=f_pch(c(1:lid)),col=pal[c(1:lid)],cex=1,bg="white",seg.len=5)
    par(new=T)
    plot(as.POSIXct(BigThom$DateTime),BigThom$Flow,type="l",col="black",yaxt="n",xaxt="n",log='y',ylab="",xlab="",xlim=c(as.POSIXct(xmin),as.POSIXct(xmax)),lty=1,lwd=1)
    axis(side = 4)
  }
  
  title=paste(day," +/- ",(period-1),"day(s)")
  
  plotTempData(
    dbGetQuery(TempDB, paste0("SELECT DeploymentIDX, Observation, DateTimeLocal, Status FROM Data WHERE ABS( (JULIANDAY(DateTimeLocal) - JULIANDAY('",day,"')) ) < ",period)),
    title=title)
  
}
