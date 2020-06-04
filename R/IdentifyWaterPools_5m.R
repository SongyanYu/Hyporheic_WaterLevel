######################################
# This script aims to i) identify potential pools,
#                    ii) calculate pool depth (residual depth) and other dimentions of pools.
# Longitudinal profile is needed for running this script, which returns the starting and ending points of pools
######################################

identify.water.pools<-function(value){
  
  local.maxima.location<-localMaxima(value) #local maxima 
  
  #start from the first maxima (i.e. the downstream end)
  
  start.point<-local.maxima.location[1]
  i<-1
  pool.points.list<-list()
  
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
      pool.point.c[j]<-pool.point
      pool.point.c[j+1]<-start.point
    }
    
    
    pool.points.list[[i]]<-pool.point.c
    i<-i+1
    
    end.point<-pool.point
    
    start.point<-local.maxima.location[local.maxima.location>=end.point][1]
  }
  
  return(pool.points.list)
}

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

water.pools.1m<-identify.water.pools(value = value.1m)
water.pools.5m<-identify.water.pools(value = value.5m)

####
# calculate water depth and width
####

poolDepth<-function(waterPool,value){
  
  pool.depth<-c()

  for(i in 1:length(waterPool)){
    
    lowest.point<-min(value[waterPool[[i]]])
    pool.depth[i]<-value[waterPool[[i]][1]]-lowest.point
  }
  
  return(pool.depth)
}

poolLength<-function(waterPool,DEMReso){
  
  pool.length<-c()
  
  for(i in 1:length(waterPool)){
    pool.length[i]<-waterPool[[i]][length(waterPool[[i]])-1]-waterPool[[i]][1]
    pool.length[i]<-pool.length[i]*DEMReso
  }
  
  return(pool.length)
}


pool.depth.1m<-poolDepth(waterPool = water.pools.1m, value = value.1m)
pool.depth.5m<-poolDepth(waterPool = water.pools.5m, value = value.5m)

pool.length.1m<-poolLength(waterPool = water.pools.1m, DEMReso = 1)
pool.length.5m<-poolLength(waterPool = water.pools.5m, DEMReso = 5)

#### The sensitivity of the number of pools to minimum pool depth
no.pools.5m<-c()
no.pools.1m<-c()
no.pools.threshold<-c(0,0.1,0.2,0.3)   #unit:m   0.1m is a number referred to Zimmermann's paper (2008)

for(i in 1:length(no.pools.threshold)){
  no.pools.1m[i]<-sum(pool.depth.1m>no.pools.threshold[i])
  no.pools.5m[i]<-sum(pool.depth.5m>no.pools.threshold[i])
}

#### The sensitivity of pool depth to minimum pool depth

poolDepthSubset<-function(waterPool,poolDepth,value){
  pool.depth.subset<-list()
  
  for(i in 1:length(no.pools.threshold)){
    pool.points.list<-waterPool
    
    exclu.pool<-which(poolDepth<=no.pools.threshold[i])
    
    exclu.pool<-rev(exclu.pool)
    for (j in exclu.pool) {
      pool.points.list[[j]]<-NULL
    }
  pool.depth.subset[[i]]<-poolDepth(waterPool = pool.points.list,value)
  }
  
  return(pool.depth.subset)
}

pool.depth.subset.1m<-poolDepthSubset(waterPool = water.pools.1m,poolDepth = pool.depth.1m,value=value.1m)
pool.depth.subset.5m<-poolDepthSubset(waterPool = water.pools.5m, poolDepth = pool.depth.5m, value = value.5m)

#### The sensitivity of pool length to minimum pool depth

poolLengthSubset<-function(waterPool,poolDepth,DEMReso){
  
  pool.length.subset<-list()
  
  for(i in 1:length(no.pools.threshold)){
    
    pool.points.list<-waterPool
    
    exclu.pool<-which(poolDepth<=no.pools.threshold[i])  
    
    exclu.pool<-rev(exclu.pool)
    for(j in exclu.pool){
      pool.points.list[[j]]<-NULL
    }
    
    pool.length.subset[[i]]<-poolLength(waterPool = pool.points.list,DEMReso)
  }
  
  return(pool.length.subset)
  
}

pool.length.subset.1m<-poolLengthSubset(waterPool = water.pools.1m, poolDepth = pool.depth.1m, DEMReso = 1)
pool.length.subset.5m<-poolLengthSubset(waterPool = water.pools.5m, poolDepth = pool.depth.5m, DEMReso = 5)


#### The sensitivity of total watering length of a channel to minimum pool depth

totalLengthSubset<-function(waterPool,poolDepth,DEMReso){
  
  pool.length.subset<-list()
  totalLength<-c()
  
  for(i in 1:length(no.pools.threshold)){
    
    pool.points.list<-waterPool
    
    exclu.pool<-which(poolDepth<=no.pools.threshold[i])  
    
    exclu.pool<-rev(exclu.pool)
    for(j in exclu.pool){
      pool.points.list[[j]]<-NULL
    }
    
    pool.length.subset[[i]]<-poolLength(waterPool = pool.points.list,DEMReso)
    totalLength[i]<-sum(pool.length.subset[[i]])
  }
  return(totalLength)
}


total.length.1m<-totalLengthSubset(waterPool = water.pools.1m, poolDepth = pool.depth.1m, DEMReso = 1)
total.length.5m<-totalLengthSubset(waterPool = water.pools.5m, poolDepth = pool.depth.5m, DEMReso = 5)

### The sensitivity of average distance between pools to minimum pool depth

distancePools<-function(waterPool,poolDepth,DEMReso){
  
  distance.pools.list<-list()
  
  for(i in 1:length(no.pools.threshold)){
    
    distance.pools<-c()
    pool.points.list<-waterPool
    
    exclu.pool<-which(poolDepth<=no.pools.threshold[i])  
    
    exclu.pool<-rev(exclu.pool)
    for(j in exclu.pool){
      pool.points.list[[j]]<-NULL
    }
    
    for(j in 1:(length(pool.points.list)-1)){
      
      distance.pools[j]<-pool.points.list[[j+1]][1]-pool.points.list[[j]][length(pool.points.list[[j]])-1]
      distance.pools[distance.pools==0]<-1
      
    }
    
    distance.pools.list[[i]]<-distance.pools*DEMReso
    
  }
  
  return(distance.pools.list)
}

distance.pools.1m<-distancePools(waterPool = water.pools.1m, poolDepth = pool.depth.1m, DEMReso = 1)
distance.pools.5m<-distancePools(waterPool = water.pools.5m, poolDepth = pool.depth.5m, DEMReso = 5)


#### Plot the comparison between attributes from the two datasets.

library(dplyr)
library(RColorBrewer)
library(ggplot2)

data.ggplot<-function(poolAttribute,DEMReso){
  
  depth<-poolAttribute[[1]]
  depth_df<-data.frame(depth)
  depth_df$threshold<-no.pools.threshold[1]
  
  for(i in 2:length(pool.depth.subset.1m)){
    depth<-poolAttribute[[i]]
    depth2_df<-data.frame(depth)
    depth2_df$threshold<-no.pools.threshold[i]
    
    depth_df<-rbind(depth_df,depth2_df)
  }
  
  depth_df$DEM<-DEMReso
  
  return(depth_df)
}

#### pool depth
depth_df.1m<-data.ggplot(pool.depth.subset.1m,DEMReso = 1)
depth_df.5m<-data.ggplot(pool.depth.subset.5m, DEMReso = 5)
depth.ggplot<-rbind(depth_df.1m,depth_df.5m)

depth.ggplot$threshold<-factor(depth.ggplot$threshold)
depth.ggplot$DEM<-factor(depth.ggplot$DEM,labels = c("1 m","5 m"))

give.n <- function(x){
  return(c(y = median(x), label = length(x)))
}

g<-ggplot(depth.ggplot,aes(x=threshold,y=depth,fill=DEM))
g<-g+geom_boxplot()
g<-g+scale_x_discrete(name="Minimum pool depth / m")+
  scale_y_continuous(name = "Pool depth / m")
g<-g+theme_bw()
g<-g+stat_summary(fun.data = give.n,geom = "text",position = position_dodge(width = 0.75),size=3.5)
g<-g+scale_fill_brewer(palette = "Accent")+
  labs(fill="DEM")
ggsave(filename = "Pool depth.png",width = 6,height = 4)


#### pool length
length_df.1m<-data.ggplot(pool.length.subset.1m,DEMReso = 1)
length_df.5m<-data.ggplot(pool.length.subset.5m,DEMReso = 5)
length.ggplot<-rbind(length_df.1m,length_df.5m)

length.ggplot$threshold<-factor(length.ggplot$threshold)
length.ggplot$DEM<-factor(length.ggplot$DEM,labels = c("1 m", "5 m"))

total.length_df<-data.frame(total_length=total.length.1m, threshold=no.pools.threshold, DEM=1)
total.length2_df<-data.frame(total_length=total.length.5m,threshold=no.pools.threshold, DEM=5)
total.length.ggplot<-rbind(total.length_df,total.length2_df)

total.length.ggplot$threshold<-factor(total.length.ggplot$threshold)
total.length.ggplot$DEM<-factor(total.length.ggplot$DEM, labels = c("1 m","5 m"))

g<-ggplot(length.ggplot,aes(x=threshold,y=depth,fill=DEM))
g<-g+geom_boxplot()
g<-g+scale_x_discrete(name="Minimum pool depth / m")+
  scale_y_continuous(name = "Pool length / m")
g<-g+theme_bw()
g<-g+scale_fill_brewer(palette = "Accent")+
  labs(fill="DEM")
g<-g+stat_summary(fun.data = give.n,geom = "text",position = position_dodge(width = 0.75),size=3.5)
ggsave(filename = "Pool length.png",width = 6,height = 4)

#g<-g+geom_point(data=total.length.ggplot,aes(x=threshold,y=total_length/18,fill=DEM))+
#  scale_y_continuous(sec.axis = sec_axis(~.*18))


#### distance between pools
distance_df.1m<-data.ggplot(distance.pools.1m,DEMReso = 1)
distance_df.5m<-data.ggplot(distance.pools.5m,DEMReso = 5)
distance.ggplot<-rbind(distance_df.1m,distance_df.5m)

distance.ggplot$threshold<-factor(distance.ggplot$threshold)
distance.ggplot$DEM<-factor(distance.ggplot$DEM, labels = c("1 m", "5 m"))

g<-ggplot(distance.ggplot,aes(x=threshold,y=depth,fill=DEM))
g<-g+geom_boxplot()
g<-g+scale_x_discrete(name="Minimum pool depth / m")+
  scale_y_continuous(name = "Distance between pools / m")
g<-g+theme_bw()
g<-g+scale_fill_brewer(palette = "Accent")+
  labs(fill="DEM")
ggsave(filename = "Distance between pools.png",width = 6,height = 4)


#### total water pool depth

width=5
ratio=0.7
ppi=100

png("Total water pool length.png",width=width*ppi,height = width*ratio*ppi)
plot(total.length.1m/length(value.1m)*100,ylim=c(10,100),
     ylab="Percentage of total pool length to entire streaem / %",
     xlab="Minimum pool depth /m",
     xaxt="n",
     type="b")
lines(total.length.5m/length(value.1m)*100,
      col="red",type="b")
axis(side = 1,at=c(1,2,3,4),labels = c(0, 0.1, 0.2, 0.3))
legend(x=3.1,y=100,legend = c("1m DEM", "5m DEM"),
       col=c("black","red"),
       lty=c(1,1))
dev.off()

#### Locations of identified water pools from the 1m and 5m DEMs
value.1m
value.5m


value<-value.1m
pool.points.list<-water.pools.1m

width<-5
asp<-2.5
ppi<-100

png(paste0("Longitudinal profile Kobble Cr_1m.png"),width = width*asp*ppi,height = width*ppi,res=ppi)

plot(value[2925:3475]~c(2925:3475),type="l",xlab=c("Upstream distance /m"),ylab=c("Elevation /m"),ylim=c(80,89))

for(i in 1:length(pool.points.list)){
  
  temp_df<-data.frame(x=pool.points.list[[i]],y=value[pool.points.list[[i]]])
  
  end=nrow(temp_df)
  b=temp_df[1,2]
  
  a=temp_df[(end-2),1]+(b-temp_df[(end-2),2])*(temp_df[(end-1),1]-temp_df[(end-2),1])/(temp_df[(end-1),2]-temp_df[(end-2),2])
  newrow<-c(a,b)
  
  temp_df[(end-1),]<-newrow
  
  polygon(temp_df,col="blue")
}

dev.off()


value<-value.5m
pool.points.list<-water.pools.5m

which(value>87.08&value<87.09)
value[673]

which(value>81.50&value<81.51)
value[569]

which(value>86.96&value<86.97)


width<-5
asp<-2.5
ppi<-100

png(paste0("Longitudinal profile Kobble Cr_5m.png"),width = width*asp*ppi,height = width*ppi,res=ppi)

plot(value[568:674]~c(568:674),type="l",ylim=c(80,89))

for(i in 1:length(pool.points.list)){
  
  temp_df<-data.frame(x=pool.points.list[[i]],y=value[pool.points.list[[i]]])
  
  end=nrow(temp_df)
  b=temp_df[1,2]
  
  a=temp_df[(end-2),1]+(b-temp_df[(end-2),2])*(temp_df[(end-1),1]-temp_df[(end-2),1])/(temp_df[(end-1),2]-temp_df[(end-2),2])
  newrow<-c(a,b)
  
  temp_df[(end-1),]<-newrow
  
  polygon(temp_df,col="blue")
}

dev.off()


