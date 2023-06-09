setwd("D:/Brockwell_Data")
data<-read.table("wine.txt")
dim(data)
head(data)
data.ts<-ts(data,start=c(1980,1),end=c(1991,12),frequency=12)
View(data.ts)
ts.plot(data.ts,xlab="Year",ylab="Sales(thousands of litres)")
#####################
rm(list=ls())
data<-read.table("deaths.txt")
dim(data)
head(data)
data.ts<-ts(data,start=c(1973,1),end=c(1978,12),frequency=12)
plot(data.ts[,3],xlab="Time(Year)",ylab="Deaths",lty=3,main="Figure 1:Time series plot")
##########################
rm(list=ls())
data<-read.table("uspop.txt")
yr<-seq(1790,1990,by=10)
data.<-data.frame(yr,data)
plot(data.,xlab="Time(Year)",ylab="Deaths",col="blue",type="l",lty=1,main="Figure 1:Time series plot")
t<-1:21
t2<-t^2
ndata<-data.frame(data$V1,t,t2)
fit<-lm(data$V1~t+t2)
results<-summary(fit)
results

coef<-results$coefficients[,1]
fobs<-coef[1]+coef[2]*t+coef[3]*t2
head(fobs)
plot(data.,xlab="Time(Year)",ylab="Deaths",type="l",lty=3)
points(yr,fobs,type = "l",col="red")
data<-rnorm(100)
acf1<-acf(data,lag=40)
acf1 
ddata<-read.table("oshorts.txt")
acf<-acf(data,lag=20)
pacf<-pacf(data,lag=20)
dd<-diff(data$V1,lag = 1,differences = 1)
plot(dd)
###############################
install.packages("smooth")
install.packages("Mcomp")
install.packages("Rcpp")
library(smooth)
library(Rcpp)
library(Mcomp)
library(ggplot2)

x<-read.table("wine.txt")
ma<-sma(x[,3],order=12,h=18)
ma
summary(ma)
forecast(ma)
plot(forecast(ma))
##########################
x<-read.table("wine.txt")
es<-es(x[,3],order=12,h=18)
summary(es)
forecast(es)
plot(forecast(es))
########################
data<-read.table("wine.txt")
x<-ts(data[,3],start=c(1980,1),end=c(1991,12),frequency=12)
m<-decompose(x,type=c("additive"))
m
plot(m)
plot(m$trend)
##############
ARMA22<-list(order=c(2,0,2),ar=c(-0.7,0.2),ma=c(0.7,0.2))
ARMA22
mu<-5
ARMA_sim<-arima.sim(n=10000,model=ARMA22)+mu
ARMA_sim
rr<-arima(x=ARMA_sim,order=c(2,0,2))
rr
autoplot(rr)
checkresiduals(rr)
install.packages("tseries")
library(tseries)
adf.test(rr$residuals)
