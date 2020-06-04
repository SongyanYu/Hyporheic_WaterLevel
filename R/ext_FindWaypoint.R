
# NOTE
#------
# This script is a test script for "FindWayPoint_20180213.R", where the process of finding waypoints has been
# made more automatic.
# Therefore, this script is now redundant.

# Below are the codes
#---------------------------
# this script aims to
# 1) idenfity the recorded waypoints in the longitudinal profile (its index in the "value" variable).
# 2) plot the longprofile of the Kobble creek section studied.
# 3) find out each feature point (waypoint) on the plot.
#
# Created date: 2018-2-11; Updated date: 2018-3-2
# Author: Songyan Yu
#--------------------------

# needed variables
source("R/Longitudinal profile.R")  # need the "value" variable
summary(value)

# 1) idenfity the recorded waypoints in the longitudinal profile (its index in the "value" variable).
waypoint.64.elevation<-92.32  # the waypoint number can be found from Kobble creek field data
n<-which(value>waypoint.64.elevation-0.01&value<waypoint.64.elevation+0.01)
value[n]
value[n+2]
location.64<-n[2]

waypoint.92.elevation<-91.44  # the isolated pool near the bridge
n<-which(value>waypoint.92.elevation-0.01&value<waypoint.92.elevation+0.01)
value[n]
location.92<-n[3]

waypoint.67.elevation<-90.13
n<-which(value>waypoint.67.elevation-0.01&value<waypoint.67.elevation+0.01)
value[n]
location.67<-n[2]

waypoint.68.elevation<-88.84
n<-which(value>waypoint.68.elevation-0.01&value<waypoint.68.elevation+0.01)
x<-which(round(value[n],2)==waypoint.68.elevation)
which(round(value[n[x]+1],2)==88.86)
which(round(value[n[x]+2],2)==88.86)
location.68<-n[x[7]]

waypoint.69.elevation<-88.87
n<-which(round(value,2)==waypoint.69.elevation)
x<-which(round(value[n+1],2)==88.85)
y<-which(round(value[n[x]+2],2)==88.77)
location.69<-n[x[y]]

waypoint.70.elevation<-88.05
n<-which(round(value,2)==waypoint.70.elevation)
location.70<-n

waypoint.71.elevation<-87.85
n<-which(round(value,2)==waypoint.71.elevation)
location.71<-n

waypoint.72.elevation<-87.51
n<-which(round(value,2)==waypoint.72.elevation)
location.72<-n

waypoint.73.elevation<-87.36
n<-which(round(value,2)==waypoint.73.elevation)
x<-which(round(value[n+1],2)==87.36)
location.73<-n[x]

waypoint.74.elevation<-84.30
n<-which(round(value,2)==waypoint.74.elevation)
location.74<-n

waypoint.77.elevation<-79.84
n<-which(round(value,2)==waypoint.77.elevation)
x<-which(round(value[n+1],2)==79.78)
location.77<-n[x]

waypoint.75.elevation<-78.33
n<-which(round(value,2)==waypoint.75.elevation)
x<-which(round(value[n+1],2)==78.34)
location.75<-n[x]

waypoint.76.elevation<-77.86
n<-which(round(value,2)==waypoint.76.elevation)
x<-which(round(value[n+1],2)==77.99)
location.76<-n[x]


# 2) plot the longprofile of the Kobble creek section studied.
width<-7
asp<-1.5
ppi<-200
png(paste0("Longitudinal profile Kobble Cr.png"),width = width*asp*ppi,height = width*ppi,res=ppi)
plot(value[2500:4067],type="l",xlab=c("Downstream distance /m"),ylab=c("Elevation /m"))
dev.off()

# 3) find out each feature point (waypoint) on the plot
#    draw them on the above plot with PPT.
location.64
plot(value[(location.64-1):(location.64+50)],type="l",xlab=c("Downstream distance /m"),ylab=c("Elevation /m"))

location.67
plot(value[(location.67-1):(location.67+50)],type="l",xlab=c("Downstream distance /m"),ylab=c("Elevation /m"))

location.68
plot(value[(location.68-1):(location.68+50)],type="l",xlab=c("Downstream distance /m"),ylab=c("Elevation /m"))

location.69
plot(value[(location.69-1):(location.69+50)],type="l",xlab=c("Downstream distance /m"),ylab=c("Elevation /m"))

location.70
plot(value[(location.70):(location.70+50)],type="l",xlab=c("Downstream distance /m"),ylab=c("Elevation /m"))

location.71
plot(value[(location.71):(location.71+50)],type="l",xlab=c("Downstream distance /m"),ylab=c("Elevation /m"))

location.72
plot(value[(location.72):(location.72+50)],type="l",xlab=c("Downstream distance /m"),ylab=c("Elevation /m"))

location.73
plot(value[(location.73):(location.73+50)],type="l",xlab=c("Downstream distance /m"),ylab=c("Elevation /m"))

location.74
plot(value[(location.74-2):(location.74+50)],type="l",xlab=c("Downstream distance /m"),ylab=c("Elevation /m"))

location.77
plot(value[(location.77-2):(location.77+50)],type="l",xlab=c("Downstream distance /m"),ylab=c("Elevation /m"))

location.75
plot(value[(location.75-150):(location.75+50)],type="l",xlab=c("Downstream distance /m"),ylab=c("Elevation /m"))

location.76
plot(value[(location.76):(location.76+50)],type="l",xlab=c("Downstream distance /m"),ylab=c("Elevation /m"))



