#PQ-1
uspop<-read.table("D:/Brockwell_Data/uspop.txt")
time<-seq(1790,1990,10)
plot(time,uspop[,1],type="l",ylab="population",main="Time series plot of uspopulation")
xt<-uspop[,1]
t<-1:length(time)
tsq<-t^2
reg<-lm(xt~(t+tsq))
summary(reg)
reg$coefficients;reg$residuals

#PQ-2...................
setwd("D:/Brockwell_Data")
lake_data<-read.table("lake.txt")
dim(lake_data)
plot(lake_data[,1],lake_data[,2],type="b",pch=16,xlab="Time",ylab="Observation",col="#B22222")
Time<-lake_data[,1]; Measure<-lake_data[,2]
t1<-1:length(Time); xt1<-Measure
r<-lm(xt1~t1)
summary(r)
abline(lm(Measure~Time),col="#40E0D0",lty=3,lwd=4)
plot(Time,r$residuals,type="b",pch=16)
rr<-acf(Measure,plot=T,col="#B22222")
rr$acf
rr$type
#PQ-3...................
#Random Walk
x<-sample(c(-1,1),size=10,replace = T)
plot(cumsum(x),xlab="Time",ylab="Random Walk",type="b",col="#B22222")
y<-rnorm(100)
plot(cumsum(y),xlab="Time",ylab="Random Walk",type="b",col="#B22222")
z<-runif(100)
plot(cumsum(z),xlab="Time",ylab="Random Walk",type="b",col="#B22222")
#PQ-4...................
#Remove seasonality trend in time series using R(Air passengers)
data("AirPassengers")
plot(AirPassengers)
decompose<-decompose(AirPassengers, type=c("multiplicative"),filter=NULL)
plot(decompose)
air_dese<-AirPassengers/decompose$seasonal
air_dese_trend<-air_dese/decompose$trend
plot(air_dese_trend)
### using function for same procedure###
library(forecast)
deseason<-seasadj(decompose)
plot(deseason)
ddeseason<-diff(deseason)
plot.ts(ddeseason)
#now the data set is stationary and now we could fit the model for forecasting.
#PQ-5...................              
#Smoothing the data
#install.packages("smooth",dependencies = T)
#install.packages("fpp2")
#install.packages("greybox")
library(greybox)
library(smooth)
library(fpp2)
getwd()
setwd("D:/Brockwell_Data")
data2<-read.table("wine.txt")
data3<-ts(data2[,3],start=1980,end=1991,frequency = 12)
plot(data3)
decompose2<-decompose(data3, type=c("multiplicative"),filter=NULL)
plot(decompose2)
deseason3<-seasadj(decompose2)
plot(deseason3)
ddeseason2<-diff(deseason2)
plot(ddeseason2)
fc<-ses(ddeseason2, h = 12)
summary(fc)
fc1<-ses(ddeseason2,alpha = 0.2, ic=c("AIC","AICc"),h=12)
summary(fc1)
fc%>%
  autoplot()+
  autolayer(fitted(fc), series = "Fitted")+
  autolayer(fitted(fc1), series = "Alpha=0.2")


#PQ-6...................
#Use of ARIMA model & Prediction
library(tseries)
(lake_data1<-ts(lake_data[,2],start=1875,end=1972,frequency=1))
plot(lake_data1,type="b",pch=16,xlab="T",ylab="O")
plot(log(lake_data1))
plot(diff(log(lake_data1)))
acf(lake_data1)
acf(diff(log(data1))) #determine the value of q
pacf(lake_data1)
pacf(diff(log(lake_data1))) #determine the value of p
#####  ARIMA in lake_data1 using prior estimated p and q ##########
fit<-arima(log(lake_data1), c(1,1,0), seasonal=list(order=c(1,1,0), period=1))
pred<-predict(fit, n.ahead=20)
(pred1<-2.718^pred$pred)
### ARIMA with wine data ###
getwd()
wine_data<-read.table("wine.txt")
r1<-arima(wine_data[,3],order = c(2,0,0))
r2<-arima(wine_data[,3],order = c(2,3,0))
r3<-arima(wine_data[,3],order = c(2,5,2))
plot(r1$residuals)
plot(r2$residuals)
plot(r3$residuals)

#PQ-7......................
#normality checking
rr<-qqnorm(fit$residuals ,plot.it=TRUE)
qqline(fit$residuals,col="#40E0D0",lwd=2)
#PQ-8......................
#Function creation
myfun<-function(a,b){
  s=a+b
  return(s)
}
myfun(7,8)
#PQ-9.....................
#spectrum analysis
x<-rnorm(10000)
spectrum(x)
t<-1:length(x)
X<-2*pi*sin(x)+cos(t)
#PQ-10
library(readxl)
gdp <- read_excel("D:/307/time series GDP-ARIMA youtube.xlsx")
View(gdp)
class(gdp)
gdpts<-ts(gdp$GDP, start = min(gdp$DATE), end = max(gdp$DATE), frequency = 4 )
class(gdpts)
library(forecast)
library(tseries)
plot(gdpts)
acf(gdpts)
pacf(gdpts)
adf.test(gdpts)
(gdptsmodel<-auto.arima(gdpts, ic="aic", trace = TRUE))
(acf(ts(gdptsmodel$residuals)))#/(acf((gdptsmodel$residuals)))
(pacf(ts(gdptsmodel$residuals)))
(mygdpforecast=forecast(gdptsmodel,level = c(95),h=10*4))
plot(mygdpforecast)
Box.test(mygdpforecast$resid, lag=5, type= "Ljung-Box")
Box.test(mygdpforecast$resid, lag=15, type= "Ljung-Box")
Box.test(mygdpforecast$resid, lag=25, type= "Ljung-Box")

#The Ljung-Box test uses the following hypotheses:
#H0: The residuals are independently distributed.
#HA: The residuals are not independently distributed; they exhibit serial corr.
#Ideally, we would like to fail to reject the null hypothesis.That is,
#we would like to see the p-value of the test be greater than 0.05 because
#this means the residuals for our time series model are independent,
#which is often an assumption we make when creating a model.

#PQ-11..................
