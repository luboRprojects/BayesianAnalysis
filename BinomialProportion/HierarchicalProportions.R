library(BRugs)

model<-"{
 for (i in 1:n) {y[i] ~ dbern(theta[coin[i]])} # n = počet opakování
 for (j in 1:m) {theta[j] ~ dbeta(prA,prB)I(0.00001,0.99999)} # m = počet subjektů

prA <- mu*kappa
prB <- (1.0-mu)*kappa

mu ~ dbeta(muA,muB)
muA <- 3.0
muB <- 3.0

kappa ~ dgamma(sk,lk)
sk <- 1.0
lk <- 10/pow(10,2)
}"

writeLines(model,con="model.txt")
modelCheck("model.txt")

N <- c(5,5,5,5,5)
z <- c(2,3,4,1,4)
coin <-NULL
y <- NULL

for (coinIdx in 1:length(N)){
 coin <- c(coin, rep(coinIdx, N[coinIdx]))
 y <- c(y, rep(1, z[coinIdx]),rep(0,N[coinIdx]-z[coinIdx]))
}

n<- length(y)
m<- length(N)

data<-list(
y = y,
coin = coin,
n = n,
m = m)


modelData(bugsData(data))

chainLength = 10000
nChains <- 3

modelCompile(numChains=nChains)
modelGenInits()	

burninSteps = 1000
modelUpdate(burninSteps)

samplesSet(c("theta","mu","kappa"))
nPerChain = 1000

modelUpdate(nPerChain, thin = 10)

theta1_val<-samplesSample("theta[1]")
theta2_val<-samplesSample("theta[2]")
mu_val<-samplesSample("mu")
kappa_val<-samplesSample("kappa")

theta1_sum<-samplesStats("theta[1]")
theta2_sum<-samplesStats("theta[2]")
theta3_sum<-samplesStats("theta[3]")
theta4_sum<-samplesStats("theta[4]")
theta5_sum<-samplesStats("theta[5]")
mu_sum<-samplesStats("mu")
kappa_sum<-samplesStats("kappa")

rbind(theta1_sum,theta2_sum,theta3_sum,theta4_sum,theta5_sum,mu_sum,kappa_sum)

