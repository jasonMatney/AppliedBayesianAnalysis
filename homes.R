#------------------------------
# ST590-A7
# Assignment 7
# Date: Mar 26, 2016
#------------------------------

rm(list=ls())
library(rjags)
#------------------------------#------------------------------
#  Load the data.
#------------------------------#------------------------------
load("Homes.RData")

# Extract the data
Y <- homes$MeanAnnualPrecipitation
X <- ifelse(OTU!=0, 1, 0)

# First, extract the 50 OTU with the largest absolute correlation with Y.
corr <- cor(X,Y)
# get the index of the 50 largest absolute corr.
inx <- order(abs(corr), decreasing = T)[1:50]
# extract the columns of X as Xnew
Xnew <- X[,inx]

n <- nrow(Xnew)
p <- ncol(Xnew)


#------------------------------#------------------------------
# (1) Uninformative normal prior
#------------------------------#------------------------------
model_string1 <- "model{

# Likelihood
for (i in 1:n){
Y[i]  ~ dnorm(mu[i], inv.var)
mu[i] <- alpha + inprod(X[i,], beta[])
}

# Prior for beta
for (j in 1:p){
beta[j] ~ dnorm(0, 0.0001)
}

# Prior for the inverse variance
inv.var ~ dgamma(0.01, 0.01)
alpha   ~ dnorm(0, 0.01)

}"

model1 <- jags.model(textConnection(model_string1),
                     data = list(Y=Y, X=Xnew,n=n,p=p),quiet=TRUE)

update(model1, 5000, progress.bar="none")

samp1 <- coda.samples(model1, variable.names=c("beta"),
                      n.iter=2000, progress.bar="none")
plot(samp1)


#------------------------------#------------------------------
# (2) Hierarchical normal prior
#------------------------------#------------------------------

model_string2 <- "model{

# Likelihood
for (i in 1:n){
Y[i]  ~ dnorm(mu[i], inv.var)
mu[i] <- alpha + inprod(X[i,], beta[])
}

# Prior for beta
for (j in 1:p){
beta[j] ~ dnorm(0, tau)
}

# Prior for the inverse variance
inv.var ~ dgamma(0.01, 0.01)
alpha   ~ dnorm(0, 0.01)
tau     ~ dgamma(0.01, 0.01)

}"

model2 <- jags.model(textConnection(model_string2),
                     data = list(Y=Y,n=n,p=p,X=Xnew),quiet=TRUE)

update(model2, 5000, progress.bar="none")

samp2 <- coda.samples(model2,
                      variable.names=c("beta"),
                      n.iter=2000, progress.bar="none")


#------------------------------#------------------------------
# (3) Hierarchical normal prior
#------------------------------#------------------------------
model_string3 <- "model{

# Likelihood
for (i in 1:n){
Y[i]  ~ dnorm(mu[i], inv.var)
mu[i] <- alpha + inprod(X[i,], beta[])
}

# Prior for beta
for (j in 1:p){
beta[j] ~ ddexp(0, inv.var.b)
}

# Prior for the inverse variance
inv.var   ~ dgamma(0.01, 0.01)
inv.var.b ~ dgamma(0.01, 0.01)
alpha     ~ dnorm(0, 0.01)

}"

model3 <- jags.model(textConnection(model_string3),
                     data = list(Y=Y,n=n,p=p,X=Xnew),quiet=TRUE)

update(model3, 1000)

samp3 <- coda.samples(model3, variable.names=c("beta"),
                      n.iter=2000)


#------------------------------#------------------------------
# Compare the 3 different priors:
#------------------------------#------------------------------
# extract the 95% posterior credible interval for beta.
CI1 <- apply(samp1[[1]], 2, function(t) quantile(t, probs=c(0.025, 0.975)))
CI2 <- apply(samp2[[1]], 2, function(t) quantile(t, probs=c(0.025, 0.975)))
CI3 <- apply(samp3[[1]], 2, function(t) quantile(t, probs=c(0.025, 0.975)))

# extract the significant index
paste(which(!(CI1[1,]<0)*(CI1[2,]>0)),collapse=",")
paste(which(!(CI2[1,]<0)*(CI2[2,]>0)),collapse=",")
paste(which(!(CI3[1,]<0)*(CI3[2,]>0)),collapse=",")
