#########################
# This script aims to get longitudinal profile in correct order (i.e. the flow direction order)
# There are two DEMs dataset availabe but in different spatial resolution (i.e. 1m & 5m). This scritp
# deals with both datasets and return the longitudinal profile for them, respectively.
#########################


library(raster)
setwd("C:/Users/s2974665/Google Drive/PhD at GU/Part 3 River channel identification/Kobble Cr")

longprofile.5m<-raster("Kob 5m DEM/kob_longprof.tif")
longprofile.1m<-raster("kob_longprof.tif")


flowdir.5m<-raster("Kob 5m DEM/Flowdir_kob.tif")
flowdir.1m<-raster("Flowdir_kob.tif")

plot(values(longprofile.5m)[!is.na(values(longprofile.5m))])
plot(values(longprofile.1m)[!is.na(values(longprofile.1m))])


longitudinal.profile.value<-function(longprofile=longprofile.1m,flowdir=flowdir.1m){
  
  value<-values(longprofile)[!is.na(values(longprofile))] #the order of "value" is from top to bottom
  
  flowdir.value<-values(flowdir)[!is.na(values(flowdir))]
  
  value.location<-which(!is.na(values(longprofile)))
  
  value.col<-value.location%%raster::ncol(longprofile)  #4708 is the number of columns of the raster
  value.row<-value.location%/%raster::ncol(longprofile)
  
  pool<-data.frame(x=value.col,y=value.row)
  
  flowdir.rowno<-which(max(pool$y)==pool$y)
  
  flowdirection<-c()
  
  flowdirection[1]<-flowdir.rowno[1]
  
  for(i in 1:(length(flowdir.value)-1)){
    
    if(flowdir.value[flowdirection[i]]==32)
      flowdirection[i+1]<-which(pool$x==pool$x[flowdirection[i]]-1&pool$y==pool$y[flowdirection[i]]-1)
    
    if(flowdir.value[flowdirection[i]]==1)
      flowdirection[i+1]<-which(pool$x==pool$x[flowdirection[i]]+1&pool$y==pool$y[flowdirection[i]])
    
    if(flowdir.value[flowdirection[i]]==2)
      flowdirection[i+1]<-which(pool$x==pool$x[flowdirection[i]]+1&pool$y==pool$y[flowdirection[i]]+1)
    
    if(flowdir.value[flowdirection[i]]==4)
      flowdirection[i+1]<-which(pool$x==pool$x[flowdirection[i]]&pool$y==pool$y[flowdirection[i]]+1)
    
    if(flowdir.value[flowdirection[i]]==8)
      flowdirection[i+1]<-which(pool$x==pool$x[flowdirection[i]]-1&pool$y==pool$y[flowdirection[i]]+1)
    
    if(flowdir.value[flowdirection[i]]==16)
      flowdirection[i+1]<-which(pool$x==pool$x[flowdirection[i]]-1&pool$y==pool$y[flowdirection[i]])
    
    if(flowdir.value[flowdirection[i]]==64)
      flowdirection[i+1]<-which(pool$x==pool$x[flowdirection[i]]&pool$y==pool$y[flowdirection[i]]-1)
    
    if(flowdir.value[flowdirection[i]]==128)
      flowdirection[i+1]<-which(pool$x==pool$x[flowdirection[i]]+1&pool$y==pool$y[flowdirection[i]]-1)
  }
  
  value<-value[rev(flowdirection)]
  
  return(value)
}

value.1m<-longitudinal.profile.value(longprofile = longprofile.1m,flowdir = flowdir.1m)
value.5m<-longitudinal.profile.value(longprofile = longprofile.5m,flowdir = flowdir.5m)

