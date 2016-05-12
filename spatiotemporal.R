Y<-read.csv("random_matrix.csv",header=TRUE)
Y<-as.matrix(Y)
ns   <- nrow(Y)
nt   <- ncol(Y)
promo_string <- "model{

# Likelihood
for(i in 1:ns){for(j in 1:nt){
Y[i,j]    ~ dnorm(mean[i,j],taue)
mean[i,j] <- mu + alpha[i] + gamma[j]
}}

# Random effects
for(i in 1:ns){
alpha[i] ~ dnorm(0,taus)
}
for(j in 1:nt){
gamma[j] ~ dnorm(0,taut)
}

# Priors
mu   ~ dnorm(0,0.000001)
taue ~ dgamma(0.1,0.1)
taus ~ dgamma(0.1,0.1)
taut ~ dgamma(0.1,0.1)

# Output the parameters of interest
sigma2[1] <- 1/taue
sigma2[2] <- 1/taus
sigma2[3] <- 1/taut
sigma[1]  <- 1/sqrt(taue)
sigma[2]  <- 1/sqrt(taus)
sigma[3]  <- 1/sqrt(taut)
pct[1]    <- sigma2[1]/sum(sigma2[])   
pct[2]    <- sigma2[2]/sum(sigma2[])   
pct[3]    <- sigma2[3]/sum(sigma2[])   

}"


model <- jags.model(textConnection(promo_string), 
                    data = list(Y=Y,ns=ns,nt=nt))

update(model, 10000, progress.bar="none");

samp <- coda.samples(model, 
                     variable.names=c("alpha","sigma","pct"), 
                     n.iter=20000, progress.bar="none")
