# Stat 414 - Environmental Statistics
# proj.r
# by: Matt Murray
# murray6@umbc.edu
# 3/28/14
# due monday 4/7/14
################################################################################
rm(list=ls()) 

# Cities used in analysis
cols<-c("Chicago","Philadelphia","New York","Los Angeles",
        "Albuquerque","Memphis","Houston","St. Louis",
        "San Diego","Milwaukee")
# Years of sample data coverage
rows<-c(1990:2013)

#Summary statistic data frames
Means<-data.frame(row.names=rows)
Medians<-data.frame(row.names=rows)
MeanAQI<-data.frame(row.names=rows)
Variance<-data.frame(row.names=rows)
VarAQI<-data.frame(row.names=rows)


#Get data file names
files <- list.files(path="C:/Users/Matthew/Documents/MyDocuments/Stat 414/Proj",
                    pattern="*.csv", full.names=T, recursive=FALSE)

#Get summary statistics per city from 1990-2013
temp<-data.frame()
for(i in 1:24){
  temp<-read.csv(files[i],header=TRUE,sep=",")
  for(k in 1:10){
    Means[i,k]<-mean(temp[temp$City.Name==cols[k],]$Arithmetic.Mean)
    Medians[i,k]<-median(temp[temp$City.Name==cols[k],]$Arithmetic.Mean)
    MeanAQI[i,k]<-mean(temp[temp$City.Name==cols[k],]$AQI)
    Variance[i,k]<-var(temp[temp$City.Name==cols[k],]$Arithmetic.Mean)
    VarAQI[i,k]<-var(temp[temp$City.Name==cols[k],]$AQI)
  }
}

colnames(Means)<-cols
colnames(Medians)<-cols
colnames(MeanAQI)<-cols
colnames(Variance)<-cols
colnames(VarAQI)<-cols

###############################################################################

#plot mean ozone per city over time
matplot(Means,type="l", xlab="Years", ylab="Mean Ozone per City (ppm)", 
        main="Mean Ozone Over Time per City (ppm)",xaxt='n')
axis(1, at=1:24, labels=rows,cex.axis=.6)


#plot median ozone per city over time
matplot(Medians,type="l", xlab="Years", ylab="Median Ozone per City (ppm)", 
        main="Median Ozone Over Time per City (ppm)",xaxt='n')
axis(1, at=1:24, labels=rows,cex.axis=.6)
#plot mean AQI over time
matplot(MeanAQI,type="l", xlab="Years", ylab="Air Quality Index per City", 
        main="Mean Air Quality Index Over Time per City",xaxt='n')
axis(1, at=1:24, labels=rows,cex.axis=.6)

###############################################################################
matplot(Variance, type="l", main="Variance of 8-Hour Primary Readings by Year",
        xlab="Years",ylab="Variance (ppm)",xaxt='n')
axis(1, at=1:24, labels=rows,cex.axis=.6)
matplot(Variance, type="l", main="Variance of AQI per City by Year",
        xlab="Years",ylab="Variance (ppm)",xaxt='n')
axis(1, at=1:24, labels=rows,cex.axis=.6)
###############################################################################
# Get sample of max readings from 1990-2013
# test a sample of 10,000 readings against the 8-hour ozone level
# create a confidence interval over the 1990-2013 time frame a graph it
#####################################
level<-.075
n=100000
temp<-data.frame()
CI<-data.frame(row.names=rows)
#Find confidence level
CI <- sapply(1:24, function(i) {
  temp <- read.csv(files[i], header=T, sep=",")
  return(mean(sample(temp$Arithmetic.Mean, n, replace=T, prob=NULL) < level))
})
#visualize
plot(CI,type="b",xlab="Years",ylab="8-Hour Primary Ozone Confidence Level",
     main="Confidence Levels Over Time",xaxt='n')
axis(1, at=1:24, labels=rows,cex.axis=.6)
####################################
# get national average over time
####################################
National<-data.frame(row.names=rows)
National <- sapply(1:24, function(i) {
  temp <- read.csv(files[i], header=T, sep=",")
  return(mean(sample(temp$Arithmetic.Mean, n, replace=T, prob=NULL)))
})
#plot national average
plot(National, type="b",xlab="Years",ylab="Mean 8-Hour Primary Reading(ppm)",
     main="National 8-Hour Primary Ozone Average(ppm)", xaxt='n')
axis(1, at=1:24, labels=rows,cex.axis=.6)
#####################################
# 2 sample z-test
##################################### 
t1<-read.csv(files[1],header=T,sep=",")
t2<-read.csv(files[24],header=T,sep=",")
# take samples
a<-sample(t1$Arithmetic.Mean, n, replace=T, prob=NULL)
b<-sample(t2$Arithmetic.Mean, n, replace=T, prob=NULL)
#plot samples
hist(a, xlab="8-Hour Primary Readings",ylab="Number of Readings", main="1990 Data Sample")
hist(b, xlab="8-Hour Primary Readings",ylab="Number of Readings", main="2013 Data Sample")
#2-sample  z-test
z2test = function(a, b){
  n=100000 #declared again in scope 
  zeta = (mean(a) - mean(b)) / (sqrt(var(a)/n + var(b)/n))
  return(zeta)
}
Z<-z2test(a,b)
print(Z)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Code borrowed from week 7 examples !!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
mu0 = mean(a)
mu1 = mean(b)

sigma = sd(a)

alpha = 0.05
beta = 0.1
power = 1-beta

lowerPt = mu0 - sigma # the lower bound to the graph's x-axis
upperPt = mu1 + sigma # the upper bound to the graph's x-axis

# Calculate the sample size needed to get the desired power when mu = mu1
n_optimal = ceiling( (qnorm(1-alpha)+qnorm(power))^2 * sigma^2 / (mu0-mu1)^2 )

x = seq(lowerPt, upperPt, by=.0001) # x-axis of the plot (possible "True" values for mu)
y = c(1:length(x)) # y-axis of the plot (initiate with vector to be filled in below)
MC = 1000
for (i in 1:length(x)){
  reject = c(1:MC) # initiate a vector to be filled in below
  for (j in 1:MC){
    mysample = rnorm(n = n_optimal, mean = x[i], sd = sigma)
    TS = (mean(mysample) - mu0)/(sigma/sqrt(n_optimal))
    reject[j] = TS > qnorm(1-alpha)
  }
  y[i] = mean(reject)
}
y = 1 - pnorm(qnorm(1-alpha)+(mu0-x)/(sigma/sqrt(n_optimal)))
plot(x, y, type="l", ylim=c(0,1), xlab="mu", ylab="Power", main="Decision Performance Goal Diagram")

# Draw the lines for the Grey region
lines( c(mu0, mu0), c(0,1) )
lines( c(mu1, mu1), c(0,1) )

# draw stars and lines to indicate the disired power and type I error 
points( mu1, power, pch=8)
lines(c(mu1, upperPt), c(power, power))
points( mu0, alpha, pch=8)
lines(c(lowerPt, mu0), c(alpha, alpha))


# now add power curves for different sample sizes to explore difference choices 
# for the sample size
n = 120
y = 1 - pnorm(qnorm(1-alpha)+(mu0-x)/(sigma/sqrt(n)))
lines(x,y, lty=2)
n = 180
y = 1 - pnorm(qnorm(1-alpha)+(mu0-x)/(sigma/sqrt(n)))
lines(x,y, lty=3)

# add a legend to the graph
legend("bottomright", c(paste("mu0 =", mu0), paste("mu1 =", mu1), paste("sigma =", sigma),
                        paste("alpha =", alpha), paste("n =",180),paste("n =",n_optimal),paste("n =",120)),
       lty=c(-1,-1,-1,-1,3,1,2))





 