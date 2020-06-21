#Load Data
data<-read.table("D:/datagwr.csv",header=TRUE,sep=";")

head(data)
tail(data)
names(data)
library(spgwr)
library(ggplot2)
library(maptools)
library(sp)
locations <- cbind(data$ui,data$vi)

#Multiple Linier Regression
model1 <- lm(data$Y ~ data$X1+data$X2+data$X3+data$X4+data$X5)
summary(model1)
anova <- aov(model1)
summary(anova)
resid <- residuals(model1)
colours <- c("orange", "blue", "red", "green", "black")
map.resid <- SpatialPointsDataFrame(data=data.frame(resid), coords=locations)
spplot(map.resid, cuts=quantile(resid), col.regions=colours, cex=1)

#MORAN'S I
library(ape)
data.dists=as.matrix(dist(cbind(data$ui,data$vi)))
data.dists.inv=1/data.dists
data.dists.inv[is.infinite(data.dists.inv)]=0
Moran.I(data$Y, data.dists.inv)

#BP Test
library(lmtest)
model<-lm(Y~X1+X2+X3+X4+X5, data=data)
bptest(model)


#Adaptive Bisquare#
adaptive.bs1 <- gwr.sel(data$Y ~ data$X1+data$X2+data$X3+data$X4+data$X5,
	adapt = TRUE, method = "cv", gweight = gwr.bisquare,coords = locations)
gwr1 <- gwr(data$Y ~ data$X1+data$X2+data$X3+data$X4+data$X5,
	adapt = adaptive.bs1, gweight = gwr.bisquare, coords = locations,hatmatrix = TRUE)
gwr1

#Fix Bisquare#
fix.bs <- gwr.sel(data$Y ~ data$X1+data$X2+data$X3+data$X4+data$X5,
	adapt = FALSE, method = "cv", gweight = gwr.bisquare,coords = locations)
gwr2 <- gwr(data$Y ~ data$X1+data$X2+data$X3+data$X4+data$X5,
	bandwidth = fix.bs, gweight = gwr.bisquare, coords = locations,hatmatrix = TRUE)
gwr2

#Adaptive Gauss#
adaptive.gauss <- gwr.sel(data$Y ~ data$X1+data$X2+data$X3+data$X4+data$X5,
	adapt = TRUE, method = "cv", gweight = gwr.Gauss,coords = locations)
gwr3 <- gwr(data$Y ~ data$X1+data$X2+data$X3+data$X4+data$X5,
	adapt = adaptive.gauss, gweight = gwr.Gauss, coords = locations,hatmatrix = TRUE)
gwr3

#Fixed Gaussian#
fix.gauss <- gwr.sel(data$Y ~ data$X1+data$X2+data$X3+data$X4+data$X5,
	adapt = FALSE, method = "cv", gweight = gwr.Gauss,coords = locations)
gwr4 <- gwr(data$Y ~ data$X1+data$X2+data$X3+data$X4+data$X5,
	bandwidth = fix.gauss, gweight = gwr.Gauss, coords = locations,hatmatrix = TRUE)
gwr4

#Selecting the Best Model by Comparing AIC Between GWR and Global Regression#
model1 <- lm(data$Y ~ data$X1+data$X2+data$X3+data$X4+data$X5)
AIC(model1)

results<-as.data.frame(gwr4$SDF)
write.csv(results,"D:/Best Model Result's.csv")

#ANOVA
LMZ.F1GWR.test(gwr4) 
LMZ.F3GWR.test(gwr4) 

#Estimated Parameters
t1 <-results$data.X1/results$data.X1_se
t2 <-results$data.X2/results$data.X2_se
t3 <-results$data.X3/results$data.X3_se
t4 <-results$data.X4/results$data.X4_se
t5 <-results$data.X4/results$data.X5_se