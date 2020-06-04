library(gbm)
library(dismo)
data("Anguilla_train")
head(Anguilla_train)
angaus.tc5.lr01<-gbm.step(data=Anguilla_train,
                          gbm.x = 3:13,
                          gbm.y = 2,
                          family = "bernoulli",
                          tree.complexity = 5,
                          learning.rate = 0.01,
                          bag.fraction = 0.5)

angaus.tc5.lr01[29]


angaus.tc5.lr005<-gbm.step(data=Anguilla_train,
                           gbm.x = 3:13,
                           gbm.y = 2,
                           family = "bernoulli",
                           tree.complexity = 5,
                           learning.rate = 0.005,
                           bag.fraction = 0.5)


###
# to more broadly explore whether other settings
# perform beterr, and assuming that these are the 
# only data available, you could either split the
# data into a training and testing set or use
# the cross-validation results. You could systemarically
# alter tc,lr and the bag fraction and compare the
# results.
###

###
# Simplifying the model by dropping variables.
###

angaus.simp<-gbm.simplify(angaus.tc5.lr005,n.drops = 5)

# The optimal number of variables to drop was 2

angaus.tc5.lr005.simp<-gbm.step(Anguilla_train,
                                gbm.x = angaus.simp$pred.list[[1]],
                                gbm.y = 2,
                                tree.complexity = 5,
                                learning.rate = 0.005)



###
# Plotting the functions and fitted values from the model.
###

gbm.plot(angaus.tc5.lr005,
         n.plots = 11,
         write.title = FALSE)



gbm.plot.fits(angaus.tc5.lr005)


###
# Interrogate and plot the interactions
###
find.int<-gbm.interactions(angaus.tc5.lr005)
find.int$interactions

find.int$rank.list

#plot
gbm.perspec(angaus.tc5.lr005,7,1,y.range = c(15,20),z.range = c(0,0.6))


###
# Predicting to new data
###
data("Anguilla_test")
library(gbm)

preds<-predict.gbm(angaus.tc5.lr005,
                   Anguilla_test,
                   n.trees = angaus.tc5.lr005$gbm.call$best.trees,
                   type="response")
preds
calc.deviance(obs = Anguilla_test$Angaus_obs,pred = preds,calc.mean = TRUE)

d<-cbind(Anguilla_test$Angaus_obs,preds)
pres<-d[d[,1]==1,2]
abs<-d[d[,1]==0,2]
e<-evaluate(p=pres,a=abs)
e

angaus.5000<-gbm.fixed(data=Anguilla_train,
                       gbm.x = 3:13,
                       gbm.y = 2,
                       learning.rate = 0.005,
                       tree.complexity = 5,
                       n.trees = 5000)
tree.list<-seq(100,5000,by=100)
pred<-predict.gbm(angaus.5000,Anguilla_test,
                  n.trees = tree.list,
                  "response")
pred

angaus.pred.deviance<-rep(0,50)

for (i in 1:50) {
  angaus.pred.deviance[i]<-calc.deviance(Anguilla_test$Angaus_obs,
                                         pred[,i],
                                         calc.mean = TRUE)
}

plot(tree.list,angaus.pred.deviance,ylim=c(0.7,1),
     xlim=c(-100,5000),
     type='l',
     xlab="number of trees",
     ylab='predictive deviance',
     cex.lab=1.5)


###
# Spatial prediction
###
data("Anguilla_grids")
plot(Anguilla_grids)

Method<-factor('electric',levels = levels(Anguilla_train$Method))
add<-data.frame(Method)
p<-predict(Anguilla_grids,angaus.tc5.lr005,const=add,
           n.trees=angaus.tc5.lr005$gbm.call$best.trees,
           type="response")
p<-mask(p,raster(Anguilla_grids,1))
plot(p)
