#---
# this script aims to i) identify potential pools,
#                    ii) calculate pool depth (residual depth)
# Author: Songyan Yu
# Date created: 18/12/2017; updated: 04/06/2020
#---

# prerequisite variables
#source("R/01_Longitudinal profile.R")  # need the "value" variable
#summary(value)

# customised funciton ro calculate local maxima
localMaxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

local.maxima.location<-localMaxima(value) #local maxima 

#start from the first maxima (the downstream end)
start.point<-local.maxima.location[1]
i<-1   #the number of identified water pools
pool.points.list<-list()   #the extent of each water pool

while(start.point<max(local.maxima.location)){
  
  pool.point<-start.point
  j<-1
  pool.point.c<-c()
  
  while(value[pool.point]<=value[start.point]){
    pool.point.c[j]<-pool.point
    pool.point<-pool.point+1
    j<-j+1
  }
  
  if(value[pool.point]>value[start.point]){
    cat(i,"\n")
    pool.point.c[j]<-pool.point
    pool.point.c[j+1]<-start.point
  }
  
  pool.points.list[[i]]<-pool.point.c
  i<-i+1
  end.point<-pool.point
  start.point<-local.maxima.location[local.maxima.location>=end.point][1]
}


## another method to identify potential water pools
## the reason to keep this method is to calculate pool depth.

top.elevation.pool<-value[local.maxima.location[1]]
upstream.end.pool<-which(value>top.elevation.pool)[1]
downstream.end.pool<-which(local.maxima.location>=upstream.end.pool)[1]

start.pool.location<-c()
end.pool.location<-c()
start.pool.elevation<-c()

start.pool.elevation[1]<-top.elevation.pool
start.pool.location[1]<-local.maxima.location[1]
end.pool.location[1]<-upstream.end.pool

i=2
while(local.maxima.location[downstream.end.pool]<max(local.maxima.location)){
  
  top.elevation.pool<-value[local.maxima.location[downstream.end.pool]]
  start.pool.location[i]<-local.maxima.location[downstream.end.pool]
  start.pool.elevation[i]<-top.elevation.pool
  
  upstream.end.pool<-which(value[local.maxima.location[downstream.end.pool]:length(value)]>top.elevation.pool)[1]+local.maxima.location[downstream.end.pool]-1
  end.pool.location[i]<-upstream.end.pool
  downstream.end.pool<-which(local.maxima.location>=upstream.end.pool)[1]
  
  i=i+1
}

##Identify surface water availability (i.e. remaining water pools) when river channel becomes disconnected.
#pool depth and width
pool.depth<-c()
pool.width<-c()

for(i in 1:length(start.pool.location)){
  lowest.ele.in.pool<-min(value[start.pool.location[i]:end.pool.location[i]])
  pool.depth[i]<-start.pool.elevation[i]-lowest.ele.in.pool
  pool.width[i]<-end.pool.location[i]-start.pool.location[i]
}

# exclude identified pools that are less than 0.1m deep.
#exclu.pool<-which(pool.depth<=0.1)  #0.1m is a number referred to Zimmermann's paper (2008)
#start.pool.location<-start.pool.location[-exclu.pool]
#end.pool.location<-end.pool.location[-exclu.pool]
#pool.depth<-pool.depth[pool.depth>0.1]
#summary(pool.depth)
#summary(pool.width)

#exclu.pool<-rev(exclu.pool)
#for(i in exclu.pool){
#  pool.points.list[[i]]<-NULL
#}

#Integrate all water pools to estimate surface water extent (% length of river channel covered by surface water)
#sum(pool.width)/length(value)

## plot for Kobble Cr
#width<-5
#asp<-2.5
#ppi<-100
#png(paste0("Figure/Longitudinal profile Kobble Cr.png"),width = width*asp*ppi,height = width*ppi,res=ppi)
plot(value[2500:4000],type="l",xlab=c("Upstream distance /m"),ylab=c("Elevation /m"),ylim=c(75,93))
for(i in 68:length(pool.points.list)){
  temp_df<-data.frame(x=pool.points.list[[i]]-2500,y=value[pool.points.list[[i]]])
  
  end=nrow(temp_df)
  b=temp_df[1,2]
  a=temp_df[(end-2),1]+(b-temp_df[(end-2),2])*(temp_df[(end-1),1]-temp_df[(end-2),1])/(temp_df[(end-1),2]-temp_df[(end-2),2])
  newrow<-c(a,b)
  
  temp_df[(end-1),]<-newrow
  polygon(temp_df,col="blue")
}
#dev.off()


