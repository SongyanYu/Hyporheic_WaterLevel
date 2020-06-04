###################
# this script can only deal with non-increasing water level change.
###################


### Needed previous variables
pool.points.list<-pool.points.list.1    # should run"IdentifyWaterPools.R" before this script

##changing water level
# plot Day 1 since cease to flow

pool.points.list.1<-pool.points.list

plot(value[2925:3300]~c(2925:3300),type="p",xlab=c("Upstream distance /m"),ylab=c("Elevation /m"),ylim=c(80,86))

for(i in c(75:80)){
  points(value[pool.points.list[[i]]]~pool.points.list[[i]],col="blue",pch=16)
}

surface.water.extent<-c()

element.length<-c()
for(i in c(75:80)){
  element.length[i]<-length(unlist(pool.points.list.1[[i]]))
  
}

surface.water.extent[1]<-sum(element.length,na.rm=TRUE)/(3300-2925+1)

# Day 2~
GW<-seq(-10,10,1)
PPT<-c(5.6,0,2.3,4)
PET<-c(12.5,11.5,13.2,9.6)

SW<-PET[1]-PPT[1]+GW

SW<-c(0,0.06,0.06,0.07,0.04,0.10,0.08,0.07,0.08,0.04)

surface.water.extent<-c()
water.level<-c()

######### under one GW scenario ############

for(j in 1:length(SW)){          #j is the number of days to estimate surface water extent.
  
  cat("Day ",j,"\n")
  
  if(j==1){
    
    for(i in 1:length(pool.points.list)){     #i is the number of identified water pools along the stream.
      
      water.level[i]<-value[pool.points.list.1[[i]]][1]-SW[j]
      
      dry.point<-unique(pool.points.list.1[[i]][which(value[pool.points.list.1[[i]]]>water.level[i])])
      wet.point<-setdiff(pool.points.list.1[[i]],dry.point)
      pool.points.list[[i]]<-wet.point
        
    }
    
    element.length<-c()
    for(m in c(75:80)){
      element.length[m]<-length(unique(unlist(pool.points.list[[m]])))
    }
    
    surface.water.extent[j]<-sum(element.length,na.rm=TRUE)/(3300-2925+1)
    
  }
  
  if(j>1){
    
    for(i in 1:length(pool.points.list)){     #i is the number of identified water pools along the stream.
      
      water.level[i]<-water.level[i]-SW[j]
      
      dry.point<-unique(pool.points.list[[i]][which(value[pool.points.list[[i]]]>water.level[i])])
      wet.point<-setdiff(pool.points.list[[i]],dry.point)
      pool.points.list[[i]]<-wet.point

    }
    
    element.length<-c()
    for(m in c(75:80)){
      element.length[m]<-length(unique(unlist(pool.points.list[[m]])))
    }
    
    surface.water.extent[j]<-sum(element.length,na.rm=TRUE)/(3300-2925+1)
  }
  
  width<-5
  asp<-2.5
  ppi<-100
  
  png(paste0("Longitudinal profile Kobble Cr_Day ",j,".png"),width = width*asp*ppi,height = width*ppi,res=ppi)
  
  plot(value[2925:3300]~c(2925:3300),type="p",xlab=c("Upstream distance /m"),ylab=c("Elevation /m"),ylim=c(80,86))
  
  for(i in c(1:99)){
    points(value[pool.points.list[[i]]]~pool.points.list[[i]],col="blue",pch=16)
  }
  
  dev.off()
}
  