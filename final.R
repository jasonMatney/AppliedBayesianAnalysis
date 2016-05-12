train<-read.csv("2013_edited.csv", header=TRUE)
rem<-c("Year","Month","Store","Store_assortment")
train<-train[ , !(names(train) %in% rem)]

Y <- train$Sales
n <- length(Y)          
type<-train$Store_type
customers<-train$Customers
promos<-train$Promos
X<-train[,5:16]
v<- unique(type)
 

model_string <- "model{

# Likelihood for Y
for(i in 1:n){
Y[i]   ~ dnorm(mu[i],inv.var)

mu[i] <- a+b[type[i]]*customers[i] + c[type[i]]*promos[i]+inprod(X[i,],d[])

}


# Priors
a ~ dnorm(0, 0.0001)


for (j in v){
b[j] ~ dunif(0,1000) 
c[j] ~ dunif(0,1000)
}


for (j in 1:12){
d[j] ~ dnorm(0,0.0001)
}


inv.var ~ dgamma(0.1,0.1)
}"

# Start the clock!
ptm <- proc.time()

model <- jags.model(textConnection(model_string), 
                    data = list(Y=Y,X=X,n=n,v=v,type=type,promos=promos,customers=customers))
# Stop the clock
(proc.time() - ptm)/60


# Start the clock!
ptm <- proc.time()
update(model, 10000, progress.bar="none");
# Stop the clock
(proc.time() - ptm)/60


# Start the clock!
ptm <- proc.time()
samp <- coda.samples(model, 
                     variable.names=c("a","b","c","d"), 
                     n.iter=20000, progress.bar="none")
# Stop the clock
