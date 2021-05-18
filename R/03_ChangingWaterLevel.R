
#---
# This script estimates reduction in surface water extent when flows cease.
# Note that this script works for non-increasing water level change.
# Author: Songyan Yu
# Date created: 28/11/2017; updated: 04/06/2020
#---

# prerequisite variables "value" and "pool.points.list"
source("R/01_Longitudinal profile.R")
source("R/02_IdentifyWaterPools.R")

# Day 1 of cease-to-flow period
surface.water.extent<-c()
wet.length<-length(unique(unlist(pool.points.list[170:306])))
surface.water.extent[1]<-wet.length/(4067-2500+1)

#plot(value[2500:4067]~c(2500:4067),type="p",xlab=c("Upstream distance /m"),ylab=c("Elevation /m"),ylim=c(75,93))
#for(i in c(170:306)){
#  points(value[pool.points.list[[i]]]~pool.points.list[[i]],col="blue",pch=16)
#}

# Day 2~14 of cease-to-flow period
recession.rate<-seq(0.002,0.02,by=0.002)  # unit: m/day
water.level<-c()
pool.points.dyn<-list()
surface.water.extent.list<-list()

for (m in 1:length(recession.rate)){
  for(j in 2:14){          #j - the number of days; 14 - the number of observation days (9-22 Feb 2018)
    
    #cat("Day ",j,"\n")
    
    if(j==2){
      for(i in 1:length(pool.points.list)){     #i is the number of identified water pools along the stream.
        water.level[i]<-value[pool.points.list[[i]]][1]-recession.rate[m]
        dry.point<-unique(pool.points.list[[i]][which(value[pool.points.list[[i]]]>water.level[i])])
        wet.point<-setdiff(pool.points.list[[i]],dry.point)
        pool.points.dyn[[i]]<-wet.point
      }
      wet.length<-length(unique(unlist(pool.points.dyn[170:306])))
      surface.water.extent[j]<-wet.length/(4067-2500+1)
    }
    
    if(j>2){
      for(i in 1:length(pool.points.list)){     #i is the number of identified water pools along the stream.
        water.level[i]<-water.level[i]-recession.rate[m]
        dry.point<-unique(pool.points.dyn[[i]][which(value[pool.points.dyn[[i]]]>water.level[i])])
        wet.point<-setdiff(pool.points.dyn[[i]],dry.point)
        pool.points.dyn[[i]]<-wet.point
      }
      wet.length<-length(unique(unlist(pool.points.dyn[170:306])))
      surface.water.extent[j]<-wet.length/(4067-2500+1)
    }
    
    #width<-5
    #asp<-2.5
    #ppi<-100
    #png(paste0("Figure/Surface water extent_Day ",j,".png"),width = width*asp*ppi,height = width*ppi,res=ppi)
    #plot(value[2500:4067]~c(2500:4067),type="p",xlab=c("Upstream distance /m"),ylab=c("Elevation /m"),ylim=c(75,93))
    #for(i in c(170:306)){
    #  points(value[pool.points.dyn[[i]]]~pool.points.dyn[[i]],col="blue",pch=16)
    #}
    #dev.off()
  }
  
  surface.water.extent.list[[m]]<-surface.water.extent
}

recession.scenarios<-do.call(rbind.data.frame,surface.water.extent.list)
colnames(recession.scenarios)<-c(1:14)
rownames(recession.scenarios)<-as.character(seq(0.002,0.02,by=0.002))
recession.scenarios<-data.frame(t(recession.scenarios))
recession.scenarios$Date<-seq.Date(from = dmy("09/02/2018"),
                              to=dmy("22/02/2018"),
                              by="day")

# combine sim vs obs data
obs<-read.csv("Data/Field data.csv")
colnames(obs)[2]<-"obs"

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
  
obs<-mutate(obs,Date=as.Date(Date,format="%d/%m/%Y"))

# plot obs vs sim surface water extent
recession.scenarios%>%
  left_join(.,obs,by="Date")%>%
  pivot_longer(cols = -c(Date,obs),names_to = "Recession_rate")%>%
  mutate(Recession_rate=as.character(gsub("X","",Recession_rate)))%>%
  ggplot(aes(x=Date))+
  geom_line(aes(y=value,color=Recession_rate))+
  geom_point(aes(y=obs),size=2.3,shape=19)+
  geom_line(data = obs,aes(y=obs,group="Observation"),size=1.2,color="black")+
  theme_classic()+
  scale_color_brewer(palette = "Paired",name="Water level\nrecession rate\n(m/day)")+
  ylab("Proportion of Kobble Cr with surface water")+
  scale_x_date(date_breaks = "3 days",date_labels = "%d/%m/%Y")+
  ggsave(filename = "Figure/Surface water extent_Obs-Sim.png",
         width = 6.18, height = 3.77)

# plot remaining pools
width<-5
asp<-2.5
ppi<-150
png(paste0("Figure/Longitudinal profile Kobble Cr.png"),width = width*asp*ppi,height = width*ppi,res=ppi)
plot(value[2500:4067],type="l",xlab=c("Upstream distance (m)"),ylab=c("Elevation (AHD m)"),ylim=c(75,93))
for(i in 170:length(pool.points.list)){
  temp_df<-data.frame(x=pool.points.list[[i]]-2500,y=value[pool.points.list[[i]]])
  
  end=nrow(temp_df)
  b=temp_df[1,2]
  a=temp_df[(end-2),1]+(b-temp_df[(end-2),2])*(temp_df[(end-1),1]-temp_df[(end-2),1])/(temp_df[(end-1),2]-temp_df[(end-2),2])
  newrow<-c(a,b)
  
  temp_df[(end-1),]<-newrow
  polygon(temp_df,col="blue")
}
dev.off()

width<-5
asp<-1.5
ppi<-100
png(paste0("Figure/Longitudinal profile Kobble Cr_inset.png"),width = width*asp*ppi,height = width*ppi,res=ppi)
plot(value[3190:3500],type="l",xlab=c("Upstream distance (m)"),ylab=c("Elevation (AHD m)"),ylim=c(81,89))
for(i in 246:269){
  temp_df<-data.frame(x=pool.points.list[[i]]-3189,y=value[pool.points.list[[i]]])
  
  end=nrow(temp_df)
  b=temp_df[1,2]
  a=temp_df[(end-2),1]+(b-temp_df[(end-2),2])*(temp_df[(end-1),1]-temp_df[(end-2),1])/(temp_df[(end-1),2]-temp_df[(end-2),2])
  newrow<-c(a,b)
  
  temp_df[(end-1),]<-newrow
  polygon(temp_df,col="blue")
}
dev.off()
