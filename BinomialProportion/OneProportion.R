library(BRugs)

# model 1 je character, vloží se do BUGS

model1<- "
  model{
   for(i in 1:n){
    y[i]~dbern(theta)
   }
theta ~ dbeta(priorA,priorB)
priorA<-1
priorB<-1
}"

writeLines(model1,con="model.txt")

modelCheck("model.txt") # kontrola správnosti zápisu modelu

# dataList bude vložen do BUGS, tedy není povoleno <- pro přiřazení
dataList<-list(
 n=14,
 y=c(rep(1,11),rep(0,3))
)

modelData(bugsData(dataList)) # kontrola správnosti zápisu dat

modelCompile()
modelGenInits()

samplesSet("theta")
chainLength = 500

modelUpdate(chainLength)

theta_val<-samplesSample("theta")
theta_sum<-samplesStats("theta")


plot(theta_val[seq(1000)],type="l",ylim=c(0,1),
main=paste("Results of chain of ",bquote(.(chainLength)),"iterations"),
ylab=expression(paste("Posterior ",theta)))

abline(a=as.numeric(theta_sum[1]),b=0,col="red")
abline(a=as.numeric(theta_sum[4]),b=0,col="grey",lty=3)
abline(a=as.numeric(theta_sum[6]),b=0,col="grey",lty=3)


# library(ggplot2)
# qplot(theta_val, geom="histogram", binwidth=0.01)
