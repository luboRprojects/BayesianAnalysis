library(BRugs)
library(ggplot2)
library(reshape2)

# Two-proportions comparison:

model<-"{
 for (i in 1:n1){
	y1[i]~dbern(theta1)
 }
 for (j in 1:n2){
	y2[j]~dbern(theta2)
 }

theta1~dbeta(prior1a,prior1b)
theta2~dbeta(prior2a,prior2b)

prior1a <- 1 # musí být <-
prior1b <- 1
prior2a <- 1
prior2b <- 1

}"

writeLines(model,con="model.txt")
modelCheck("model.txt")

data<-list(
n1 = 7,
y1 = c(rep(1,5),rep(0,2)),
n2 = 7,
y2 = c(rep(1,2),rep(0,5))
)

modelData(bugsData(data)) # kontrola správnosti zápisu dat

modelCompile()
modelGenInits()

samplesSet("theta1")
samplesSet("theta2")

chainLength = 10000

modelUpdate(chainLength)

theta1_val<-samplesSample("theta1")
theta2_val<-samplesSample("theta2")
theta1_sum<-samplesStats("theta1")
theta2_sum<-samplesStats("theta2")

results<-data.frame(cbind(theta1_val,theta2_val))
colnames(results)<-c("theta1","theta2")


m0<-ggplot(results,aes(x=theta1,y=theta2))
m0+geom_point(alpha=0.1)


# Rozdíl podílù?
mean<-c(theta1_sum[[1]][1],theta2_sum[[1]][1])
LI<-c(theta1_sum[[4]][1],theta2_sum[[4]][1])
UI<-c(theta1_sum[[6]][1],theta2_sum[[6]][1])
param<-c("theta1","theta2")

int<-data.frame(param,mean,LI,UI)

m2<-ggplot(int,aes(x=param,y=mean))
m2+geom_point(size=4) + geom_errorbar(aes(ymax = UI, ymin = LI))


mres<-melt(results)
m1<-ggplot(mres,aes(value))
m1+geom_histogram(binwidth=0.02)+facet_grid(variable~.)


