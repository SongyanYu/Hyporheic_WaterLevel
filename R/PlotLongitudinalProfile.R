# variables needed
value
pool.points.list

# plot the Kobble Creek Section studied.

plot(value[2550:4020]~c(2550:4020),type="l",xlab=c("Upstream distance /m"),ylab=c("Elevation /m"),ylim=c(75,93))

for(i in 1:length(pool.points.list)){
  
  temp_df<-data.frame(x=pool.points.list[[i]],y=value[pool.points.list[[i]]])
  
  end=nrow(temp_df)
  b=temp_df[1,2]
  
  a=temp_df[(end-2),1]+(b-temp_df[(end-2),2])*(temp_df[(end-1),1]-temp_df[(end-2),1])/(temp_df[(end-1),2]-temp_df[(end-2),2])
  newrow<-c(a,b)
  
  temp_df[(end-1),]<-newrow
  
  polygon(temp_df,col="blue")
}



