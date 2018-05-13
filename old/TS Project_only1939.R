###Time Series Project 21.5.18

library(itsmr)
library(tseries)

#importation of data (quarterly indice des loyers, y1939-2017)

#data 1939-2017 quarterly (98 obs.)
load("C:/Users/Spörri/Desktop/Marc UniNE/SS 18 Time Series Analysis/TS Project/indiceloyers1939quarterly.RData")

indiceloyers <- indiceloyers1939quarterly #load indiceloyers1939quarterly in the case of 1939-2017 data

#formating and data cleansing
indiceloyers.vector <- as.vector(t(indiceloyers)) #transposing first the matrix, then vectorizing in order to transform into a time series object
indiceloyers.vector <- indiceloyers.vector[3:320]  #data should be [3:320] (98 observations, beginning in the 3rd quarter)
indiceloyers.vector <- as.numeric(indiceloyers.vector)
indiceloyers <- ts(indiceloyers.vector,start=c(1939,3),end=c(2017,4),freq=4)  #in the case of the 1939-2017 start=c(1939,3)


plot(indiceloyers)

#since there is fore sure a positive trend and the values are kinda increasing exponentially we could choose a exponential model

#linear, quadratic and loglinear regressions
t <- 1:length(indiceloyers) 
linear.trend <- lm(indiceloyers~t)  #lin
summary(linear.trend) 
t2<-t^2		
quadratic.trend <- lm(indiceloyers~t+t2) 
summary(quadratic.trend) 
loglinear.trend <- lm(indiceloyers~log(t)) 
summary(loglinear.trend) 
lines(x = t,y=linear.trend$fitted.values , lwd = 2)


##2. remove the trend and seasonal component to get stationary residuals
#removing trend when there is no seasonality, p.25/46
plot(linear.trend$residuals)
abline(h = 0, lwd = 2)
resid.linear.trend <- ts(residuals(linear.trend), start=1939, freq=4)   #in the case of the 1939-2017 start=1939
Box.test(resid.linear.trend, lag=1, type="Ljung-Box")   #we reject the H0 of independence of lag1 of the residuals
adf.test(resid.linear.trend, alternative = "stationary")
kpss.test(resid.linear.trend)

plot(resid.linear.trend, main="Rent index (residuals from a quadratic trend)",
     ylab="residuals")  #trend is removed
abline(h = 0, lwd = 2)
acf(resid.linear.trend)
pacf(resid.linear.trend)
#the residual time series >t clearly shows temporal dependence, and would badly be modelled by an iid noise

plot(quadratic.trend$residuals)
abline(h = 0, lwd = 2)
resid.quadratic.trend <- ts(residuals(quadratic.trend), start=1939, freq=4) #in the case of the 1939-2017 start=1939
Box.test(resid.quadratic.trend, lag=1, type="Ljung-Box")   #we reject the H0 of independence of lag1 of the residuals
adf.test(resid.quadratic.trend, alternative = "stationary")
kpss.test(resid.quadratic.trend)
plot(resid.quadratic.trend, main="Rent index (residuals from a quadratic trend)",
     ylab="residuals")  #trend is removed
abline(h = 0, lwd = 2)
acf(resid.quadratic.trend)
pacf(resid.quadratic.trend)

plot(loglinear.trend$residuals)
abline(h = 0, lwd = 2)
resid.loglinear.trend <- ts(residuals(loglinear.trend), start=1939, freq=4) #in the case of the 1939-2017 start=1939
Box.test(resid.loglinear.trend, lag=1, type="Ljung-Box")   #we reject the H0 of independence of lag1 of the residuals
plot(resid.loglinear.trend, main="Rent index (residuals from a loglinear regression)",
     ylab="residuals")  #trend is removed
abline(h = 0, lwd = 2)
acf(resid.loglinear.trend)
pacf(resid.loglinear.trend)

#trend elimination by differentiation with lag = 1
d1.indiceloyers <- ts(diff(indiceloyers, lag=1), start=1939, freq=4) #in the case of the 1939-2017 start=1939
d1.indiceloyers

##############################################################
d2.indiceloyers <- ts(diff(indiceloyers, lag=2), start=1939, freq=4) #in the case of the 1939-2017 start=1939
d2.indiceloyers  #for comparison

d4.indiceloyers <- ts(diff(indiceloyers, lag=4), start=1939, freq=4) #in the case of the 1939-2017 start=1939
d4.indiceloyers  #for comparison

d5.indiceloyers <- ts(diff(indiceloyers, lag=5), start=1939, freq=4) #in the case of the 1939-2017 start=1939
d5.indiceloyers  #for comparison

d10.indiceloyers <- ts(diff(indiceloyers, lag=10), start=1939, freq=4) #in the case of the 1939-2017 start=1939
d10.indiceloyers  #for comparison

plot(d1.indiceloyers, main="Rent index lag=1", ylab="diff rent index" )  #for comparison
Box.test(d1.indiceloyers, lag=1, type="Ljung-Box")   #we cannot reject the H0 of independence of lag1 of the residuals
#therefore there is probably no more auto-correlation between the residuals in the differentiation-1-model
acf(d1.indiceloyers)   #acf shows the same pattern, no more auto-correlation in the differentiation-1-model
pacf(d1.indiceloyers)
#conclusion: we should work on the model which reduced the trend by lag1-differentiation

plot(d2.indiceloyers, main="Rent index lag=2", ylab="diff rent index" )  #for comparison
plot(d4.indiceloyers, main="Rent index lag=4", ylab="diff rent index" )  #for comparison
plot(d5.indiceloyers, main="Rent index lag=5", ylab="diff rent index" )  #for comparison
plot(d10.indiceloyers, main="Rent index lag=5", ylab="diff rent index" )  #for comparison
##############################################################
















