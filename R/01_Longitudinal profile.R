#---
# This script is used to plot longitudinal profile for Kobble creek 1m DEM data
# Author: Songyan Yu
# Date created: 04/06/2020
#---
library(raster)

longprofile<-raster("../../Kobble Cr/kob_longprof.tif")  #1m longitudinal profile
#plot(values(longprofile)[!is.na(values(longprofile))])
value<-values(longprofile)[!is.na(values(longprofile))] # the order of "value" is from top to bottom
value.location<-which(!is.na(values(longprofile)))
value.col<-value.location %% ncol(longprofile)  #4708 is the number of columns of the raster
value.row<-value.location %/% ncol(longprofile)

flowdir<-raster("../../Kobble Cr/Flowdir_kob.tif")
flowdir.value<-values(flowdir)[!is.na(values(flowdir))]

## Rearrange all of the main stem pixels in the order of upstream to downstream.
## They were initially in a scanning order (from top to bottom and from left to right)
pool<-data.frame(x=value.col,y=value.row)

# Starting from the most upstream cell, which has the biggest row number.
flowdir.rowno<-which(max(pool$y)==pool$y)

flowdirection<-c()
flowdirection[1]<-flowdir.rowno

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

##plot the longitudinal channel profile
value<-value[rev(flowdirection)]
#plot(value)

#width<-7
#asp<-1.5
#ppi<-150
#png(paste0("Longitudinal profile Kobble Cr_1.png"),width = width*asp*ppi,height = width*ppi,res=ppi)
#plot(value[2550:4020],type="l",xlab=c("Upstream distance /m"),ylab=c("Elevation /m"),ylim=c(75,93))
#dev.off()

