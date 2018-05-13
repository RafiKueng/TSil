###Time Series Project 21.5.18

library(itsmr)

#importation of data (quarterly indice des loyers, y1939 - 2018)

#data 1939-2017 quarterly (318 obs.)
load("C:/Users/Spörri/Desktop/Marc UniNE/SS 18 Time Series Analysis/TS Project/indiceloyers1939quarterly.RData")
#data 1993-2017 quarterly (98 obs.)
load("C:/Users/Spörri/Desktop/Marc UniNE/SS 18 Time Series Analysis/TS Project/indiceloyers1993quarterly.RData")

indiceloyers <- indiceloyers1993quarterly #load indiceloyers1993quarterly in the case of 1993-2017 data

#formating and data cleansing
indiceloyers.vector <- as.vector(t(indiceloyers)) #transposing first the matrix, then vectorizing in order to transform into a time series object
indiceloyers.vector <- indiceloyers.vector[3:320]  #in the case of the 1993-2017 data it should be [3:100] (98 observations)
indiceloyers.vector <- as.numeric(indiceloyers.vector)
indiceloyers <- ts(indiceloyers.vector,start=c(1939,3),end=c(2017,4),freq=4)  #in the case of the 1993-2017 start=c(1993,3)


plot(indiceloyers)

#since there is fore sure a positive trend and the values are kinda increasing exponentially we could choose a exponential model

#A model with trend, least square regression, p.17/46
t <- 1:length(indiceloyers) 
linear.trend <- lm(indiceloyers~t)  #lin
summary(linear.trend) 
t2<-t^2		
quadratic.trend <- lm(indiceloyers~t+t2) 
summary(quadratic.trend) 
loglinear.trend <- lm(indiceloyers~log(t)) 
summary(loglinear.trend) 
plot(indiceloyers) 
lines(x = t,y=linear.trend$fitted.values , lwd = 2)


##2. remove the trend and seasonal component to get stationary residuals
#removing trend when there is no seasonality, p.25/46
plot(linear.trend$residuals)
abline(h = 0, lwd = 2)
resid.linear.trend <- ts(residuals(linear.trend), start=1939, freq=4)   #in the case of the 1993-2017 start=1993
plot(resid.linear.trend, main="Rent index (residuals from a quadratic trend)",
     ylab="residuals")  #trend is removed
abline(h = 0, lwd = 2)
#the residual time series >t clearly shows temporal dependence, and would badly be modelled by an iid noise

plot(quadratic.trend$residuals)
abline(h = 0, lwd = 2)
resid.quadratic.trend <- ts(residuals(quadratic.trend), start=1939, freq=4) #in the case of the 1993-2017 start=1993
plot(resid.quadratic.trend, main="Rent index (residuals from a quadratic trend)",
     ylab="residuals")  #trend is removed
abline(h = 0, lwd = 2)

plot(loglinear.trend$residuals)
abline(h = 0, lwd = 2)
resid.loglinear.trend <- ts(residuals(loglinear.trend), start=1939, freq=4) #in the case of the 1993-2017 start=1993
plot(resid.loglinear.trend, main="Rent index (residuals from a loglinear regression)",
     ylab="residuals")  #trend is removed
abline(h = 0, lwd = 2)

#trend elimination by differentiation with lag = 1
d1.indiceloyers <- ts(diff(indiceloyers, lag=1), start=1939, freq=4) #in the case of the 1993-2017 start=1993
d1.indiceloyers

##############################################################
d0.indiceloyers <- ts(diff(indiceloyers, lag=0), start=1939, freq=4) #in the case of the 1993-2017 start=1993
d0.indiceloyers  #for comparison

d2.indiceloyers <- ts(diff(indiceloyers, lag=2), start=1939, freq=4) #in the case of the 1993-2017 start=1993
d2.indiceloyers  #for comparison

d4.indiceloyers <- ts(diff(indiceloyers, lag=4), start=1939, freq=4) #in the case of the 1993-2017 start=1993
d4.indiceloyers  #for comparison

d5.indiceloyers <- ts(diff(indiceloyers, lag=5), start=1939, freq=4) #in the case of the 1993-2017 start=1993
d5.indiceloyers  #for comparison

d10.indiceloyers <- ts(diff(indiceloyers, lag=10), start=1939, freq=4) #in the case of the 1993-2017 start=1993
d10.indiceloyers  #for comparison

plot(d1.indiceloyers, main="Rent index lag=1", ylab="diff rent index" )  #for comparison
plot(d2.indiceloyers, main="Rent index lag=2", ylab="diff rent index" )  #for comparison
plot(d4.indiceloyers, main="Rent index lag=4", ylab="diff rent index" )  #for comparison
plot(d5.indiceloyers, main="Rent index lag=5", ylab="diff rent index" )  #for comparison
##############################################################

#check ACF and PACF















#plot
plot(d1.milk, main="Milk production lag=1", ylab="diff index" )


#smoothing with different degrees of freedom
plot(milkts, main= "milk production (pounds)", xlab = "time", 
     ylab="monthly production", col="darkblue")  #there seem to be a trend & a seasonality
spline.milk <- smooth.spline(milkts, df=20)
lines(spline.milk, col="red")

spline.milk1 <- smooth.spline(milkts, df=2)  #fitting a linear trend
spline.milk2 <- smooth.spline(milkts, df=3)   #fitting a quadratic trend
lines(spline.milk1, col="black")
lines(spline.milk2, col="green")




set.seed(8) 
t <- 1:length(indiceloyers) 
tsquare <- t^2 
Xt <- -102.74309 + 3.85323*t + rnorm(length(indiceloyers) ,0,10)  #polynomial of degree1
Xt <- -102.74309 + 3.85323*t - 0.01*t^2 + rnorm(316,0,10)  #polynomial of degree2
plot(Xt)

#Removing trend, when there is no seasonality (polynomial trend of degree1)
linreg_degree1 <- lm(Xt~t) 
summary(linreg_degree1) #a_0 = 19.5, a_1 = 0.58 plotc(linreg_degree1$residuals)



#Removing trend, when there is no seasonality (polynomial trend of degree2)
