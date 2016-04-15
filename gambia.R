#------------------------------
# ST590
# Assignment 8
# Date: Apr 13, 2016
#------------------------------

rm(list=ls())
library(rjags)
#------------------------------#------------------------------
# 1. Load the data.
#------------------------------#------------------------------
library(geoR)

# extract the response and X.
Y <- gambia$pos
X <- gambia$netuse

# extract the village.
Village <- as.matrix(paste(gambia[,1],gambia[,2],sep=","))
Village <- as.matrix(as.numeric(as.factor(Village)))


data<-cbind(Y,Village,X)
colnames(data)<-c("Y","Village","X")


#Model
n <- nrow(data)
nv <- length(unique(Village))  # the total number of villages


#------------------------------#------------------------------
# 2. JAGS model
#------------------------------#------------------------------
modelstring <- "model{

# Likelihood

for(i in 1:n){

Y[i] ~ dbern(q[i])
logit(q[i]) <- alpha[Village[i]] + beta[Village[i]]*X[i]

}


# prior
for(j in 1:nv){

alpha[j]~dnorm(mualpha,inv.var.alpha)
beta[j]~dnorm(mubeta,inv.var.beta)

}

mualpha~dnorm(0,0.001)
mubeta~dnorm(0,0.001)
inv.var.alpha~dgamma(0.001,0.001)
inv.var.beta~dgamma(0.001,0.001)

}"


model <- jags.model(textConnection(modelstring),data = list(Y=data[,1],X=data[,3],n=n,Village=data[,2],nv=nv),quiet=TRUE)


update(model, 10000, progress.bar="none")

samp <- coda.samples(model, variable.names=c("alpha", "beta"),
                      n.iter=20000, progress.bar="none")
# plot(samp)

summary(samp)


#------------------------------#------------------------------
# 3. check whether the slop and intercept vary by village.
#------------------------------#------------------------------
# extract the posteriors of alpha and beta.
alpha.post <- samp[[1]][,1:nv]
beta.post <- samp[[1]][,(nv+1):(nv*2)]

# get the 95% posterior credible interval.
alpha.low <- apply(alpha.post, 2, function(x) quantile(x, 0.025))
alpha.high <- apply(alpha.post, 2, function(x) quantile(x, 0.975))
beta.low <- apply(beta.post, 2, function(x) quantile(x, 0.025))
beta.high <- apply(beta.post, 2, function(x) quantile(x, 0.975))

# get the posterior mean.
alpha.mean <- colMeans(alpha.post)
beta.mean <- colMeans(beta.post)

par(mfrow=c(1,2))
# plot the posterior mean together with the 95% credible interval.
plot(1:nv, alpha.mean,
     ylim=range(c(alpha.low, alpha.high)),
     pch=19, xlab="Village", ylab=expression(alpha))
# hack: we draw arrows but with very special "arrowheads"
arrows(1:nv, alpha.low, 1:nv, alpha.high, length=0.05, angle=90, code=3)
abline(h=0,col="red", lwd=0.5)


plot(1:nv, beta.mean,
     ylim=range(c(beta.low, beta.high)),
     pch=19, xlab="Village", ylab=expression(beta))
# hack: we draw arrows but with very special "arrowheads"
arrows(1:nv, beta.low, 1:nv, beta.high, length=0.05, angle=90, code=3)
abline(h=0,col="red", lwd=0.5)



# plot(NA,xlim=c(-3,3),ylim=c(1,nv),xlab="95% interval",ylab="Village")
# abline(v=0)
#
# for(j in 1:nv){
#   if(0>alpha.low[j] & 0<alpha.high[j]){lines(c(alpha.low[j],alpha.high[j]),c(j,j),col="black")}
#   else{lines(c(alpha.low[j],alpha.high[j]),c(j,j),col="red")}
# }


#------------------------------#------------------------------
# 4. largest slope village
#------------------------------#------------------------------
which.max(alpha.mean)
which.max(beta.mean)
which.max(abs(beta.mean))

max(alpha.mean)
max(beta.mean)
max(abs(beta.mean))


prop = aggregate(Y ~ Village, data=data, mean)
which.max(prop[,2])
max(prop[,2])


prop1 = aggregate(Y ~ Village + X, data=data, mean)
prop1

Y[Village==40]
X[Village==40]

Y[Village==45]
X[Village==45]


#------------------------------#------------------------------
# 5. check the sensitivity to hyperparameters.
#------------------------------#------------------------------
modelstring1 <- "model{

# Likelihood

for(i in 1:n){

Y[i] ~ dbern(q[i])
logit(q[i]) <- alpha[Village[i]] + beta[Village[i]]*X[i]

}


# prior
for(j in 1:nv){

alpha[j]~dnorm(mualpha,inv.var.alpha)
beta[j]~dnorm(mubeta,inv.var.beta)

}

mualpha~dnorm(0,1)  # the number and the distribution can be both changed!
mubeta~dnorm(0,1)   # the number and the distribution can be both changed!
inv.var.alpha~dgamma(0.001,0.001)  # the number and the distribution can be both changed!
inv.var.beta~dgamma(0.001,0.001)   # the number and the distribution can be both changed!

}"


model1 <- jags.model(textConnection(modelstring1),data = list(Y=data[,1],X=data[,3],n=n,Village=data[,2],nv=nv),quiet=TRUE)


update(model1, 10000, progress.bar="none")

samp1 <- coda.samples(model1, variable.names=c("alpha", "beta"),
                     n.iter=20000, progress.bar="none")
# plot(samp1)

summary(samp1)

# then repeat the above process...
#------------------------------#------------------------------
# check whether the slop and intercept vary by village.
#------------------------------#------------------------------
# extract the posteriors of alpha and beta.
alpha.post1 <- samp1[[1]][,1:nv]
beta.post1 <- samp1[[1]][,(nv+1):(nv*2)]

# get the 95% posterior credible interval.
alpha.low1 <- apply(alpha.post1, 2, function(x) quantile(x, 0.025))
alpha.high1 <- apply(alpha.post1, 2, function(x) quantile(x, 0.975))
beta.low1 <- apply(beta.post1, 2, function(x) quantile(x, 0.025))
beta.high1 <- apply(beta.post1, 2, function(x) quantile(x, 0.975))

# get the posterior mean.
alpha.mean1 <- colMeans(alpha.post1)
beta.mean1 <- colMeans(beta.post1)

par(mfrow=c(1,2))
# plot the posterior mean together with the 95% credible interval.
plot(1:nv, alpha.mean1,
     ylim=range(c(alpha.low1, alpha.high1)),
     pch=19, xlab="Village", ylab=expression(alpha))
# hack: we draw arrows but with very special "arrowheads"
arrows(1:nv, alpha.low1, 1:nv, alpha.high1, length=0.05, angle=90, code=3)
abline(h=0,col="red", lwd=0.5)


plot(1:nv, beta.mean1,
     ylim=range(c(beta.low1, beta.high1)),
     pch=19, xlab="Village", ylab=expression(beta))
# hack: we draw arrows but with very special "arrowheads"
arrows(1:nv, beta.low1, 1:nv, beta.high1, length=0.05, angle=90, code=3)
abline(h=0,col="red", lwd=0.5)

#------------------------------#------------------------------
# largest slope village
#------------------------------#------------------------------
which.max(alpha.mean1)
which.max(beta.mean1)
max(alpha.mean1)
max(beta.mean1)
