#------------------------------
# ST590-A6
# Homework 6
# Date: Mar 18, 2016
#------------------------------

rm(list=ls())
library(rjags)
#------------------------------#------------------------------
#  R code of the Gibbs samplers.
#------------------------------#------------------------------
dat   <- read.csv("http://www4.stat.ncsu.edu/~reich/ABA/assignments/hurricane.csv")
dat   <- dat[101:(nrow(dat)-1),]
dat$t <- dat$year - 1949

f <- function(t,u){max(0, t-u)}

X1 <- apply(matrix(dat$t,ncol=1),1,function(x) f(x,15))
X2 <- apply(matrix(dat$t,ncol=1),1,function(x) f(x,30))
X3 <- apply(matrix(dat$t,ncol=1),1,function(x) f(x,45))
X  <- cbind(X1,X2,X3)
Y  <- dat$numhurr
n  <- length(Y)

#----------------------------
#1. uninformative priors.
#----------------------------
model_string1 <- "model{

# Likelihood:
for(i in 1:n){
lamda[i] <- exp(beta[1] + beta[2]*X[i,1] + beta[3]*X[i,2] + beta[4]*X[i,3])
Y[i] ~ dpois(lamda[i])
}

# priors:
for(i in 1:4){
beta[i] ~ dnorm(0,0.0001)

}
}"

model1 <- jags.model(textConnection(model_string1),
                    n.chains = 3,
                    data = list(Y=Y,n=n,X=X))

update(model1, 10000, progress.bar="none"); # Burnin for 5000 samples

samp1 <- coda.samples(model1, thin=10,
                     variable.names=c("beta"),
                     n.iter=5000, progress.bar="none")

summary(samp1)
plot(samp1)
plot(samp1[[1]][,1])
plot(samp1[[1]][,2])
plot(samp1[[1]][,3])
plot(samp1[[1]][,4])
autocorr.plot(samp1)

# extract the posterior mean of beta
beta.m1   <- apply(samp1[[1]],2,mean)

# lambda evaluated at the posterior mean of beta
lambda.m1 <- exp(beta.m1[1] + beta.m1[2]*X[,1] + beta.m1[3]*X[,2] + beta.m1[4]*X[,3])

# plot the data vs the fitted value
par(mfrow=c(1,2))
plot(dat$t, Y, xlab="t",main="(1)")
lines(lambda.m1,col="red")



#----------------------------
#2. informative priors.
#----------------------------
model_string <- "model{

# Likelihood
for(i in 1:n){
lamda[i] <- exp(gamma[i] + beta[1] + beta[2]*X[i,1] + beta[3]*X[i,2] + beta[4]*X[i,3])
Y[i] ~ dpois(lamda[i])
}

# priors:
for(i in 1:n){
gamma[i] ~ dnorm(0,inv)
}

for(i in 1:4){
beta[i] ~ dnorm(0,0.0001)
}

inv ~ dgamma(0.001, 0.001) # give sigma^2 an inverse gamma prior.

}"

model <- jags.model(textConnection(model_string),
                    n.chains = 3,
                    data = list(Y=Y,n=n,X=X))

update(model, 10000, progress.bar="none"); # Burnin for 5000 samples

samp <- coda.samples(model, thin=10,
                     variable.names=c("gamma","beta","inv"),
                     n.iter=5000, progress.bar="none")

summary(samp)
plot(samp)
autocorr.plot(samp)

# extract the posterior mean of beta
temp   <- apply(samp[[1]],2,mean)
beta.m <- temp[1:4]
gamma.m <- temp[5:70]

# lambda evaluated at the posterior mean of beta
lambda.m <- exp(gamma.m + beta.m[1] + beta.m[2]*X[,1] + beta.m[3]*X[,2] + beta.m[4]*X[,3])

# plot the data vs the fitted value
plot(dat$t, Y, xlab="t",main="(2)")
lines(lambda.m,col="red")
