
---
output: pdf_document
---
```{r chunk_name, echo=T}
#------------------------------
# ST590-HW1
#------------------------------
data <- read.csv("ozone.csv")
data <- as.matrix(data[,-1])
# summary of the data table. 
n <- nrow(data); m <- ncol(data) 

#------------------------------#------------------------------
#  (a) Create a table with the overall mean, standard deviation, and percent missing.
#------------------------------#------------------------------
data.nonmissing <- data[!is.na(data)]
overall.mean <- mean(data.nonmissing) #mean(data, na.rm=T)
overall.sd <- sd(data.nonmissing)     #sd(data, na.rm=T)
percent.missing <- 100*(1-length(data.nonmissing)/(n*m) )
output <- cbind(overall.mean, overall.sd, percent.missing)
colnames(output) <- c("Mean", "SD", "Percent Missing (%)")
print(output, digits=4)

#------------------------------#------------------------------
# (b) Write a loop to compute the mean, variance, and percent missing for each of the n sites.
#------------------------------#------------------------------
site.mean <- site.var <- site.percent.missing <- NULL
for(i in 1:n){
  data.tem <- data[i,]
  data.nonmissing <- data.tem[!is.na(data.tem)]
  site.mean <- c(site.mean, mean(data.nonmissing))
  site.var <- c(site.var, var(data.nonmissing))
  site.percent.missing <- c(site.percent.missing, 100*(1-length(data.nonmissing)/m))
}

# # Another way to get the site mean. 
# apply(data,1, function(x) mean(x, na.rm=T))
# apply(data,1, function(x) var(x, na.rm=T))
# apply(data,1, function(x) 100*sum(is.na(x))/length(x))


# make histogram of each variable. 
# pdf(file="hw1_figure.pdf")
par(mfrow=c(2,3))
hist(site.mean, main="", breaks=30, freq=F, xlab="Mean")
hist(site.var, main="", breaks=30, freq=F, xlab="Variance")
hist(site.percent.missing, main="", breaks=30, freq=F, xlab="Percent Missing (%)")

# create scatter plots
plot(site.mean, site.var, xlab="Mean", ylab="Variance")
plot(site.mean, site.percent.missing, xlab="Mean", ylab="Percent Missing (%)")
plot(site.var, site.percent.missing, xlab="Variance", ylab="Percent Missing (%)")

#------------------------------#------------------------------
# (c) Conduct a linear regression. 
#------------------------------#------------------------------
fit <- lm(site.mean ~ site.var + site.percent.missing)
summary(fit)
# plot(fit)
```




