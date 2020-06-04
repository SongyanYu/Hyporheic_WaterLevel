#---------------------------
# this script aims to
# 1) idenfity the recorded waypoints in the longitudinal profile (its index in the "value" variable).
# 2) plot the longprofile of the Kobble creek section studied.
# 3) find out each feature point (waypoint) on the plot.
#
# Created date: 2018-3-2; Updated date: 2018-3-2
# Author: Songyan Yu
#--------------------------

# prerequisite variables
source("R/01_Longitudinal profile.R")  # need the "value" variable
summary(value)

# 1) idenfity the recorded waypoints in the longitudinal profile (its index in the "value" variable).
waypoints_all<-read.csv("../../Kobble Cr/waypoint.csv")
location<-c()

waypoints<-waypoints_all[waypoints_all$Date==1,]  ## use "Date" for different date groups.

for(i in 1:nrow(waypoints)){
  waypoint.elevation<-waypoints$elevation[i]
  waypoint.elevation.backward.1<-waypoints$backward1[i]
  waypoint.elevation.backward.2<-waypoints$backward2[i]
  
  n<-which(value>waypoint.elevation-0.01&value<waypoint.elevation+0.01)
  
  if(length(n)>1){
    x<-which(round(value[n],2)==waypoint.elevation)
    if(length(x)>1){
      y_1<-which(round(value[n[x]+1],2)==waypoint.elevation.backward.1)
      y_2<-which(round(value[n[x]+2],2)==waypoint.elevation.backward.2)
      if(length(intersect(y_1,y_2))>1){
          cat("The point of ",waypoints$waypoint[i]," still cannot makt it.")
      }
      if(length(intersect(y_1,y_2))==1){
        location[i]<-n[x[intersect(y_1,y_2)]]
      }
    }
   if(length(x)==1){
      location[i]<-n[x]
   }
  }
  if(length(n)==1){
    location[i]<-n
  }
}
waypoints$location<-location

# 2) plot the longprofile of the Kobble creek section studied.
width<-7
asp<-1.5
ppi<-300
png(paste0("Longitudinal profile Kobble Cr_20180213.png"),width = width*asp*ppi,height = width*ppi,res=ppi)
plot(value[2500:4067],type="l",xlab=c("Downstream distance /m"),ylab=c("Elevation /m"))
points(x=waypoints$location-2500,y=value[waypoints$location],col="blue",pch=20,cex=0.9)
graphics::text(x=waypoints$location-2500,y=value[waypoints$location],labels=waypoints$waypoint,
               pos=1)
dev.off()



