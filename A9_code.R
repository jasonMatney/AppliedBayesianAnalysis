#------------------------------
# ST590-A9
#------------------------------

rm(list=ls())
library(rjags)
#------------------------------#------------------------------
# 1. Load the data. 
#------------------------------#------------------------------
data <- read.csv("data.csv")

# Subset by each species. 

Gorg <- data[data$Taxon == "Gorgosaurus",]
Albert <- data[data$Taxon == "Albertosaurus",]
Dasp <- data[data$Taxon == "Daspletosaurus",]
TRex <- data[data$Taxon == "Tyrannosaurus",]

# par(mfrow=c(1,3))
# get the plot.
plot(TRex$Age,TRex$Mass,main="Model in Paper", xlab="Age (years)", ylab="Body Mass (kg)", col=1, pch=15) 
points(Dasp$Age,Dasp$Mass, col=2, pch=16)  
points(Albert$Age,Albert$Mass, col=3, pch=17)  
points(Gorg$Age,Gorg$Mass, col=4, pch=18)  

xx = seq(0,30,length.out=100)
FT = function(x){(5551/(1+exp(-0.57*(x-16.1))))+5}
FD = function(x){(1728/(1+exp(-0.44*(x-12.1))))+5}
FA = function(x){(1234/(1+exp(-0.38*(x-12.4))))+5}
FG = function(x){(1218/(1+exp(-0.43*(x-14.1))))+5}

lines(xx,FT(xx), col=1) 
lines(xx,FD(xx), col=2)  
lines(xx,FA(xx), col=3) 
lines(xx,FG(xx), col=4)  

legend("topleft",c("Tyrannosaurus","Daspletosaurus","Albertosaurus","Gorgosaurus" ), col=1:4, pch=15:18)


#------------------------------#------------------------------
# 2. Fit a separate model for each species. 
#------------------------------#------------------------------

model_stringG <- "model{

# Likelihood

for(i in 1:n){

mu[i] <- 5 + a/( 1+exp(-b*(X[i]-c) ))
Y[i] ~ dnorm(mu[i],taue)

}

#prior
taue ~ dgamma(0.01,0.01)

a ~ dnorm(1200, tau.a)
tau.a ~ dgamma(0.01, 0.01)

b ~ dunif(0,1)

c ~ dnorm(12, tau.c)
tau.c ~ dgamma(0.01, 0.01)


# Prediction
for(k in 1:np){
mup[k] <- 5 + a/( 1+exp(-b*(Xp[k]-c) ))
Yp[k] ~ dnorm(mup[k], taue)
}
}"

modelG <- jags.model(textConnection(model_stringG),data = list(Y=Gorg$Mass,X=Gorg$Age,n=nrow(Gorg),np=length(xx),
                                                               Xp=xx),
                     quiet=TRUE,n.chains = 3)

update(modelG, 10000, progress.bar="none")

sampG <- coda.samples(modelG, variable.names=c("a","b", "c","Yp"), n.iter=10000, progress.bar="none")
teG <- summary(sampG)


#----------------------------
model_stringT <- "model{

# Likelihood

for(i in 1:n){

mu[i] <- 5 + a/( 1+exp(-b*(X[i]-c) ))
Y[i] ~ dnorm(mu[i],taue)

}

#prior
taue ~ dgamma(0.01,0.01)

a ~ dnorm(5500, tau.a)
tau.a ~ dgamma(0.01, 0.01)

b ~ dunif(0,1)

c ~ dnorm(16, tau.c)
tau.c ~ dgamma(0.01, 0.01)

# Prediction
for(k in 1:np){
mup[k] <- 5 + a/( 1+exp(-b*(Xp[k]-c) ))
Yp[k] ~ dnorm(mup[k], taue)
}

}"

modelT <- jags.model(textConnection(model_stringT),data = list(Y=TRex$Mass,X=TRex$Age,n=nrow(TRex),np=length(xx),
                                                               Xp=xx),
                     quiet=TRUE,n.chains = 3)

update(modelT, 10000, progress.bar="none")

sampT <- coda.samples(modelT, variable.names=c("a","b", "c","Yp"), n.iter=10000, progress.bar="none")
teT <- summary(sampT)

#----------------------------
model_stringD <- "model{

# Likelihood

for(i in 1:n){

mu[i] <- 5 + a/( 1+exp(-b*(X[i]-c) ))
Y[i] ~ dnorm(mu[i],taue)

}

#prior
taue ~ dgamma(0.01,0.01)

a ~ dnorm(1700, tau.a)
tau.a ~ dgamma(0.01, 0.01)

b ~ dunif(0,1)

c ~ dnorm(12, tau.c)
tau.c ~ dgamma(0.01, 0.01)

# Prediction
for(k in 1:np){
mup[k] <- 5 + a/( 1+exp(-b*(Xp[k]-c) ))
Yp[k] ~ dnorm(mup[k], taue)
}
}"

modelD <- jags.model(textConnection(model_stringD),data = list(Y=Dasp$Mass,X=Dasp$Age,n=nrow(Dasp),np=length(xx),
                                                               Xp=xx),
                     quiet=TRUE,n.chains = 3)

update(modelD, 10000, progress.bar="none")

sampD <- coda.samples(modelD, variable.names=c("a","b", "c","Yp"), n.iter=10000, progress.bar="none")
teD <- summary(sampD)

#----------------------------
model_stringA <- "model{

# Likelihood

for(i in 1:n){

mu[i] <- 5 + a/( 1+exp(-b*(X[i]-c) ))
Y[i] ~ dnorm(mu[i],taue)

}

#prior
taue ~ dgamma(0.01,0.01)

a ~ dnorm(1200, tau.a)
tau.a ~ dgamma(0.01, 0.01)

b ~ dunif(0,1)

c ~ dnorm(14, tau.c)
tau.c ~ dgamma(0.01, 0.01)

# Prediction
for(k in 1:np){
mup[k] <- 5 + a/( 1+exp(-b*(Xp[k]-c) ))
Yp[k] ~ dnorm(mup[k], taue)
}

}"

modelA <- jags.model(textConnection(model_stringA),data = list(Y=Albert$Mass,X=Albert$Age,n=nrow(Albert),np=length(xx),
                                                               Xp=xx),
                     quiet=TRUE,n.chains = 3)

update(modelA, 10000, progress.bar="none")

sampA<- coda.samples(modelA, variable.names=c("a","b", "c","Yp"), n.iter=10000, progress.bar="none")
teA <- summary(sampA)


#----------------------------
#----------------------------
plot(TRex$Age,TRex$Mass,main="Separate Model", xlab="Age (years)", ylab="Body Mass (kg)", col=1, pch=15) 
points(Dasp$Age,Dasp$Mass, col=2, pch=16)  
points(Albert$Age,Albert$Mass, col=3, pch=17)  
points(Gorg$Age,Gorg$Mass, col=4, pch=18)  

lines(xx, teG[[1]][1:100,1], lty=1, col=4)
lines(xx, teG[[2]][1:100,1], lty=2, col=4)
lines(xx, teG[[2]][1:100,5], lty=2, col=4)

lines(xx, teA[[1]][1:100,1], lty=1, col=3)
lines(xx, teA[[2]][1:100,1], lty=2, col=3)
lines(xx, teA[[2]][1:100,5], lty=2, col=3)

lines(xx, teT[[1]][1:100,1], lty=1, col=1)
lines(xx, teT[[2]][1:100,1], lty=2, col=1)
lines(xx, teT[[2]][1:100,5], lty=2, col=1)

lines(xx, teD[[1]][1:100,1], lty=1, col=2)
lines(xx, teD[[2]][1:100,1], lty=2, col=2)
lines(xx, teD[[2]][1:100,5], lty=2, col=2)

legend("topleft",c("Tyrannosaurus","Daspletosaurus","Albertosaurus","Gorgosaurus" ), col=1:4, pch=15:18)


#----------------------------
# report the estimate body mass at age=30
#----------------------------
c(teG[[1]]["Yp[100]",1], teG[[2]]["Yp[100]",c(1,5)])
c(teA[[1]]["Yp[100]",1], teA[[2]]["Yp[100]",c(1,5)])
c(teD[[1]]["Yp[100]",1], teD[[2]]["Yp[100]",c(1,5)])
c(teT[[1]]["Yp[100]",1], teT[[2]]["Yp[100]",c(1,5)])



#------------------------------#------------------------------
# 3. Fit a hierarchical model for all species. 
#------------------------------#------------------------------

model_string <- "model{

# Likelihood

for(i in 1:n){

mu[i] <- 5 + a[Species[i]]/( 1+exp(-b[Species[i]]*(X[i]-c[Species[i]]) ))
Y[i] ~ dnorm(mu[i],taue)

}

# prior
taue ~ dgamma(0.01, 0.01)
tau.a ~ dgamma(0.01, 0.01)
tau.c ~ dgamma(0.01, 0.01)

for(j in 1:nspecies){
a[j] ~ dnorm(3000, tau.a)

b[j] ~ dunif(0,1)

c[j] ~ dnorm(10, tau.c)
}

# Prediction
for(k in 1:np){
mup[k] <- 5 + a[Species.p[k]]/( 1+exp(-b[Species.p[k]]*(Xp[k]-c[Species.p[k]]) ))
Yp[k] ~ dnorm(mup[k], taue)
}

}"

Xp <- rep(xx,4)
Species.p <- c(rep(1,100), rep(2,100), rep(3,100), rep(4,100))
data$Species <- as.numeric(factor(data$Taxon))


model <- jags.model(textConnection(model_string),data = list(Y=data$Mass,X=data$Age,Species=data$Species,
                                                             n=nrow(data),nspecies=4,
                                                             np=length(Xp), Xp=Xp, Species.p=Species.p),
                     quiet=TRUE,n.chains = 3)

update(model, 10000, progress.bar="none")

samp<- coda.samples(model, variable.names=c("a","b", "c","Yp"), n.iter=10000, progress.bar="none")
te <- summary(samp)

#----------------------------
plot(TRex$Age,TRex$Mass,main="Hierarchical Model", xlab="Age (years)", ylab="Body Mass (kg)", col=1, pch=15) 
points(Dasp$Age,Dasp$Mass, col=2, pch=16)  
points(Albert$Age,Albert$Mass, col=3, pch=17)  
points(Gorg$Age,Gorg$Mass, col=4, pch=18)  

lines(Xp[1:100], te[[1]][1:100,1], lty=1, col=3)
lines(Xp[1:100], te[[2]][1:100,1], lty=2, col=3)
lines(Xp[1:100], te[[2]][1:100,5], lty=2, col=3)

lines(Xp[101:200], te[[1]][101:200,1], lty=1, col=2)
lines(Xp[101:200], te[[2]][101:200,1], lty=2, col=2)
lines(Xp[101:200], te[[2]][101:200,5], lty=2, col=2)

lines(Xp[201:300], te[[1]][201:300,1], lty=1, col=4)
lines(Xp[201:300], te[[2]][201:300,1], lty=2, col=4)
lines(Xp[201:300], te[[2]][201:300,5], lty=2, col=4)

lines(Xp[301:400], te[[1]][301:400,1], lty=1, col=1)
lines(Xp[301:400], te[[2]][301:400,1], lty=2, col=1)
lines(Xp[301:400], te[[2]][301:400,5], lty=2, col=1)

legend("topleft",c("Tyrannosaurus","Daspletosaurus","Albertosaurus","Gorgosaurus" ), col=1:4, pch=15:18)


#----------------------------
# report the estimate body mass at age=30
#----------------------------
c(te[[1]]["Yp[100]",1], te[[2]]["Yp[100]",c(1,5)]) #A
c(te[[1]]["Yp[200]",1], te[[2]]["Yp[200]",c(1,5)]) #D
c(te[[1]]["Yp[300]",1], te[[2]]["Yp[300]",c(1,5)]) #G
c(te[[1]]["Yp[400]",1], te[[2]]["Yp[400]",c(1,5)]) #T
